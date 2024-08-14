{-|
Module      : GeniusYield.Transaction.Common
Description : Common utility types used during transaction building and coin selection.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

module GeniusYield.Transaction.Common (
    GYBalancedTx (..),
    GYTxInDetailed (..),
    utxoFromTxInDetailed,
    GYBuildTxError (..),
    GYBalancingError (..),
    minimumUTxO,
    adjustTxOut
) where

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Ledger.Coin                  as Ledger
import           GeniusYield.Imports
import           GeniusYield.Transaction.CBOR
import           GeniusYield.Types.Address
import           GeniusYield.Types.ProtocolParameters (GYProtocolParameters,
                                                       protocolParametersToApi)
import           GeniusYield.Types.PubKeyHash
import           GeniusYield.Types.Redeemer
import           GeniusYield.Types.Script
import           GeniusYield.Types.Slot
import           GeniusYield.Types.TxCert.Internal
import           GeniusYield.Types.TxIn
import           GeniusYield.Types.TxMetadata
import           GeniusYield.Types.TxOut
import           GeniusYield.Types.TxWdrl
import           GeniusYield.Types.UTxO
import           GeniusYield.Types.Value
import qualified Text.Printf                          as Printf


{- | An *almost* finalized Tx.

This is fully balanced _except_ potentially missing an ada change output, and missing the exact fee.
Both of these will be set by 'GeniusYield.Transaction.finalizeGYBalancedTx'.
-}
data GYBalancedTx v = GYBalancedTx
    { gybtxIns           :: ![GYTxInDetailed v]
    , gybtxCollaterals   :: !GYUTxOs
    , gybtxOuts          :: ![GYTxOut v]
    , gybtxMint          :: !(Maybe (GYValue, [(GYMintScript v, GYRedeemer)]))
    , gybtxWdrls         :: ![GYTxWdrl v]
    , gybtxCerts         :: ![GYTxCert' v]
    , gybtxInvalidBefore :: !(Maybe GYSlot)
    , gybtxInvalidAfter  :: !(Maybe GYSlot)
    , gybtxSigners       :: !(Set GYPubKeyHash)
    , gybtxRefIns        :: !GYUTxOs
    , gybtxMetadata      :: !(Maybe GYTxMetadata)
    }

-- | A further detailed version of 'GYTxIn', containing all information about a UTxO.
data GYTxInDetailed v = GYTxInDetailed
    { gyTxInDet          :: !(GYTxIn v)
    , gyTxInDetAddress   :: !GYAddress
    , gyTxInDetValue     :: !GYValue
    , gyTxInDetDatum     :: !GYOutDatum
    , gyTxInDetScriptRef :: !(Maybe (Some GYScript))
    }
  deriving (Eq, Show)

utxoFromTxInDetailed :: GYTxInDetailed v -> GYUTxO
utxoFromTxInDetailed (GYTxInDetailed (GYTxIn ref _witns) addr val d ms) = GYUTxO ref addr val d ms

-------------------------------------------------------------------------------
-- Transaction Building Errors
-------------------------------------------------------------------------------

data GYBalancingError
    = GYBalancingErrorInsufficientFunds !GYValue
    | forall v. GYBalancingErrorNonPositiveTxOut !(GYTxOut v)
    | GYBalancingErrorChangeShortFall !Natural
    -- ^ Lovelace shortfall in constructing a change output. See: "Cardano.CoinSelection.Balance.UnableToConstructChangeError"
    | GYBalancingErrorEmptyOwnUTxOs
    -- ^ User wallet has no utxos to select.

deriving stock instance Show GYBalancingError

instance Printf.PrintfArg GYBalancingError where
    formatArg = Printf.formatArg . show

instance Eq GYBalancingError where
    GYBalancingErrorInsufficientFunds v1 == GYBalancingErrorInsufficientFunds v2 = v1 == v2
    GYBalancingErrorChangeShortFall n1 == GYBalancingErrorChangeShortFall n2 = n1 == n2
    GYBalancingErrorEmptyOwnUTxOs == GYBalancingErrorEmptyOwnUTxOs = True
    GYBalancingErrorNonPositiveTxOut out1 == GYBalancingErrorNonPositiveTxOut out2 = txOutToApi out1 == txOutToApi out2
    _ == _ = False

-- | 'GYBuildTxError' may be raised when building transactions, for non-trivial errors.
-- Insufficient funds and similar are considered trivial transaction building errors.
data GYBuildTxError
    = GYBuildTxBalancingError !GYBalancingError
    | GYBuildTxBodyErrorAutoBalance !(Api.TxBodyErrorAutoBalance Api.S.ConwayEra)
    | GYBuildTxPPConversionError !Api.ProtocolParametersConversionError
    | GYBuildTxMissingMaxExUnitsParam
    -- ^ Missing max ex units in protocol params
    | GYBuildTxExUnitsTooBig         -- ^ Execution units required is higher than the maximum as specified by protocol params.
        (Natural, Natural)           -- ^ Tuple of maximum execution steps & memory as given by protocol parameters.
        (Natural, Natural)           -- ^ Tuple of execution steps & memory as taken by built transaction.

    | GYBuildTxSizeTooBig           -- ^ Transaction size is higher than the maximum as specified by protocol params.
        !Natural                    -- ^ Maximum size as specified by protocol parameters.
        !Natural                    -- ^ Size our built transaction took.
    | GYBuildTxCollateralShortFall  -- ^ Shortfall (in collateral inputs) for collateral requirement.
        !Natural                    -- ^ Transaction collateral requirement.
        !Natural                    -- ^ Lovelaces in given collateral UTxO.
    | GYBuildTxNoSuitableCollateral
    -- ^ Couldn't find a UTxO to use as collateral.
    | GYBuildTxCborSimplificationError !CborSimplificationError
    | GYBuildTxCollapseExtraOutError !Api.TxBodyError
  deriving stock Show

-------------------------------------------------------------------------------
-- Transaction Utilities
-------------------------------------------------------------------------------

minimumUTxO :: GYProtocolParameters -> GYTxOut v -> Natural
minimumUTxO pp txOut = fromInteger $ coerce $
  Api.calculateMinimumUTxO Api.ShelleyBasedEraConway (txOutToApi txOut) $ protocolParametersToApi pp

adjustTxOut :: (GYTxOut v -> Natural) -> GYTxOut v -> GYTxOut v
adjustTxOut minimumUTxOF = helper
  where
    helper txOut =
        let v         = gyTxOutValue txOut
            needed    = minimumUTxOF txOut
            contained = extractLovelace $ valueToApi v
        in if needed <= contained
            then txOut
            else
                let v'     = valueFromLovelace (fromIntegral $ needed - contained) <> v
                    txOut' = txOut {gyTxOutValue = v'}
                in helper txOut'

extractLovelace :: Api.Value -> Natural
extractLovelace v = case Api.selectLovelace v of Ledger.Coin n -> fromIntegral $ max 0 n
