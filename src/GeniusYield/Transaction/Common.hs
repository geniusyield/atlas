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
    BalancingError (..),
    minimumUTxO,
    adjustTxOut
) where

import qualified Cardano.Api         as Api
import qualified Cardano.Api.Shelley as Api.S

import qualified Cardano.Ledger.Alonzo.Core as Ledger

import           GeniusYield.Imports
import           GeniusYield.Types
import qualified Text.Printf         as Printf

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
    , gybtxCerts         :: ![GYTxCert v]
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

data BalancingError
    = BalancingErrorInsufficientFunds !GYValue
    | forall v. BalancingErrorNonPositiveTxOut !(GYTxOut v)
    | BalancingErrorChangeShortFall !Natural
    -- ^ Lovelace shortfall in constructing a change output. See: "Cardano.CoinSelection.Balance.UnableToConstructChangeError"
    | BalancingErrorEmptyOwnUTxOs
    -- ^ User wallet has no utxos to select.

deriving stock instance Show BalancingError

instance Printf.PrintfArg BalancingError where
    formatArg = Printf.formatArg . show

instance Eq BalancingError where
    BalancingErrorInsufficientFunds v1 == BalancingErrorInsufficientFunds v2 = v1 == v2
    BalancingErrorChangeShortFall n1 == BalancingErrorChangeShortFall n2 = n1 == n2
    BalancingErrorEmptyOwnUTxOs == BalancingErrorEmptyOwnUTxOs = True
    BalancingErrorNonPositiveTxOut out1 == BalancingErrorNonPositiveTxOut out2 = txOutToApi out1 == txOutToApi out2
    _ == _ = False

-------------------------------------------------------------------------------
-- Transaction Utilities
-------------------------------------------------------------------------------

minimumUTxO :: Ledger.PParams (Api.S.ShelleyLedgerEra Api.S.BabbageEra) -> GYTxOut v -> Natural
minimumUTxO pp txOut = fromInteger $ coerce $
  Api.calculateMinimumUTxO Api.ShelleyBasedEraBabbage (txOutToApi txOut) pp

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
extractLovelace v = case Api.selectLovelace v of Api.Lovelace n -> fromIntegral $ max 0 n
