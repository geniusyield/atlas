{- |
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
  utxoToTxInDetailed,
  GYTxExtraConfiguration (..),
  GYBuildTxError (..),
  GYBalancingError (..),
  minimumUTxO,
  minimumApiUTxO,
  adjustTxOut,
) where

import Cardano.Api qualified as Api
import Cardano.Ledger.Coin qualified as Ledger
import Data.Default (Default (..))
import GeniusYield.Imports
import GeniusYield.Transaction.CBOR
import GeniusYield.Types.Address
import GeniusYield.Types.BuildScript
import GeniusYield.Types.BuildWitness
import GeniusYield.Types.Era
import GeniusYield.Types.Governance (GYProposalProcedurePB, GYTxVotingProcedures)
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)
import GeniusYield.Types.PubKeyHash
import GeniusYield.Types.Redeemer
import GeniusYield.Types.Script
import GeniusYield.Types.Slot
import GeniusYield.Types.TxCert.Internal
import GeniusYield.Types.TxIn
import GeniusYield.Types.TxMetadata
import GeniusYield.Types.TxOut
import GeniusYield.Types.TxWdrl
import GeniusYield.Types.UTxO
import GeniusYield.Types.Value
import Text.Printf qualified as Printf

{- | An *almost* finalized Tx.

This is fully balanced _except_ potentially missing an ada change output, and missing the exact fee.
Both of these will be set by 'GeniusYield.Transaction.finalizeGYBalancedTx'.
-}
data GYBalancedTx v = GYBalancedTx
  { gybtxIns :: ![GYTxInDetailed v]
  , gybtxCollaterals :: !GYUTxOs
  , gybtxOuts :: ![GYTxOut v]
  , gybtxMint :: !(Maybe (GYValue, [(GYBuildScript v, GYRedeemer)]))
  , gybtxWdrls :: ![GYTxWdrl v]
  , gybtxCerts :: ![GYTxCert' v]
  , gybtxInvalidBefore :: !(Maybe GYSlot)
  , gybtxInvalidAfter :: !(Maybe GYSlot)
  , gybtxSigners :: !(Set GYPubKeyHash)
  , gybtxRefIns :: !GYUTxOs
  , gybtxMetadata :: !(Maybe GYTxMetadata)
  , gybtxVotingProcedures :: !(GYTxVotingProcedures v)
  , gybtxProposalProcedures :: ![(GYProposalProcedurePB, GYTxBuildWitness v)]
  }

-- | A further detailed version of 'GYTxIn', containing all information about a UTxO.
data GYTxInDetailed v = GYTxInDetailed
  { gyTxInDet :: !(GYTxIn v)
  , gyTxInDetAddress :: !GYAddress
  , gyTxInDetValue :: !GYValue
  , gyTxInDetDatum :: !GYOutDatum
  , gyTxInDetScriptRef :: !(Maybe GYAnyScript)
  }
  deriving (Eq, Show)

utxoFromTxInDetailed :: GYTxInDetailed v -> GYUTxO
utxoFromTxInDetailed (GYTxInDetailed (GYTxIn ref _witns) addr val d ms) = GYUTxO ref addr val d ms

utxoToTxInDetailed :: GYUTxO -> GYTxInWitness v -> GYTxInDetailed v
utxoToTxInDetailed utxo witns = GYTxInDetailed (GYTxIn (utxoRef utxo) witns) (utxoAddress utxo) (utxoValue utxo) (utxoOutDatum utxo) (utxoRefScript utxo)

-- | Extra configuration for transaction building.
data GYTxExtraConfiguration v = GYTxExtraConfiguration
  { gytxecUtxoInputMapper :: GYUTxO -> GYTxInDetailed v
  -- ^ When coin selection selects additional UTxOs, this function is used to map them to 'GYTxInDetailed'. This in particular is useful, when inputs are selected from a contract based wallet.
  , gytxecPreBodyContentMapper :: Api.TxBodyContent Api.BuildTx ApiEra -> Api.TxBodyContent Api.BuildTx ApiEra
  -- ^ This function is called on the 'Api.TxBodyContent' before submitting it to 'makeTransactionBodyAutoBalanceWrapper'.
  , gytxecPostBodyContentMapper :: Api.TxBodyContent Api.BuildTx ApiEra -> Api.TxBodyContent Api.BuildTx ApiEra
  -- ^ This function is called on the 'Api.TxBodyContent' after submitting it to 'makeTransactionBodyAutoBalanceWrapper'.
  , gytxecFeeUtxo :: Maybe GYUTxO
  -- ^ UTxO to use for fees.
  }

instance Default (GYTxExtraConfiguration v) where
  def =
    GYTxExtraConfiguration
      { gytxecUtxoInputMapper = \GYUTxO {utxoRef, utxoAddress, utxoValue, utxoOutDatum, utxoRefScript} ->
          GYTxInDetailed
            { -- It is assumed the 'GYUTxOs' arg designates key wallet utxos.
              gyTxInDet = GYTxIn utxoRef GYTxInWitnessKey
            , gyTxInDetAddress = utxoAddress
            , gyTxInDetValue = fst $ valueSplitSign utxoValue -- TODO: Is this split required?
            , gyTxInDetDatum = utxoOutDatum
            , gyTxInDetScriptRef = utxoRefScript
            }
      , gytxecPreBodyContentMapper = id
      , gytxecPostBodyContentMapper = id
      , gytxecFeeUtxo = Nothing
      }

-------------------------------------------------------------------------------
-- Transaction Building Errors
-------------------------------------------------------------------------------

data GYBalancingError
  = GYBalancingErrorInsufficientFunds !GYValue
  | forall v. GYBalancingErrorNonPositiveTxOut !(GYTxOut v)
  | -- | Lovelace shortfall in constructing a change output. See: "GeniusYield.Transaction.CoinSelection.Balance.UnableToConstructChangeError"
    GYBalancingErrorChangeShortFall !Natural
  | -- | User wallet has no utxos to select.
    GYBalancingErrorEmptyOwnUTxOs

deriving stock instance Show GYBalancingError

instance Printf.PrintfArg GYBalancingError where
  formatArg = Printf.formatArg . show

instance Eq GYBalancingError where
  GYBalancingErrorInsufficientFunds v1 == GYBalancingErrorInsufficientFunds v2 = v1 == v2
  GYBalancingErrorChangeShortFall n1 == GYBalancingErrorChangeShortFall n2 = n1 == n2
  GYBalancingErrorEmptyOwnUTxOs == GYBalancingErrorEmptyOwnUTxOs = True
  GYBalancingErrorNonPositiveTxOut out1 == GYBalancingErrorNonPositiveTxOut out2 = txOutToApi out1 == txOutToApi out2
  _ == _ = False

{- | 'GYBuildTxError' may be raised when building transactions, for non-trivial errors.
Insufficient funds and similar are considered trivial transaction building errors.
-}
data GYBuildTxError
  = GYBuildTxBalancingError !GYBalancingError
  | GYBuildTxBodyErrorAutoBalance !(Api.TxBodyErrorAutoBalance ApiEra)
  | -- | If fee UTxO is provided for in extra build configuration, then this error is raised if it's insufficient to cover for fees and ADA of subsequent change output.
    GYBuildTxFeeUtxoAdaInsufficient !(Api.TxBodyErrorAutoBalance ApiEra)
  | -- | Execution units required is higher than the maximum as specified by protocol params.
    GYBuildTxExUnitsTooBig
      -- | Tuple of maximum execution steps & memory as given by protocol parameters.
      (Natural, Natural)
      -- | Tuple of execution steps & memory as taken by built transaction.
      (Natural, Natural)
  | -- | Transaction size is higher than the maximum as specified by protocol params.
    GYBuildTxSizeTooBig
      -- | Maximum size as specified by protocol parameters.
      !Natural
      -- | Size our built transaction took.
      !Natural
  | -- | Shortfall (in collateral inputs) for collateral requirement.
    GYBuildTxCollateralShortFall
      -- | Transaction collateral requirement.
      !Natural
      -- | Lovelaces in given collateral UTxO.
      !Natural
  | -- | Couldn't find a UTxO to use as collateral.
    GYBuildTxNoSuitableCollateral
  | GYBuildTxCborSimplificationError !CborSimplificationError
  | GYBuildTxCollapseExtraOutError !Api.TxBodyError
  deriving stock Show

-------------------------------------------------------------------------------
-- Transaction Utilities
-------------------------------------------------------------------------------

minimumApiUTxO :: ApiProtocolParameters -> Api.TxOut Api.CtxTx ApiEra -> Natural
minimumApiUTxO pp txOut =
  fromInteger $
    coerce $
      Api.calculateMinimumUTxO Api.ShelleyBasedEraConway txOut pp

minimumUTxO :: ApiProtocolParameters -> GYTxOut v -> Natural
minimumUTxO pp = minimumApiUTxO pp . txOutToApi

adjustTxOut :: (GYTxOut v -> Natural) -> GYTxOut v -> GYTxOut v
adjustTxOut minimumUTxOF = helper
 where
  helper txOut =
    let v = gyTxOutValue txOut
        needed = minimumUTxOF txOut
        contained = extractLovelace $ valueToApi v
     in if needed <= contained
          then txOut
          else
            let v' = valueFromLovelace (fromIntegral $ needed - contained) <> v
                txOut' = txOut {gyTxOutValue = v'}
             in helper txOut'

extractLovelace :: Api.Value -> Natural
extractLovelace v = case Api.selectLovelace v of Ledger.Coin n -> fromIntegral $ max 0 n
