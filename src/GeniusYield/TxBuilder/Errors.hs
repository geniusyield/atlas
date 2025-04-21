{- |
Module      : GeniusYield.TxBuilder.Errors
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.TxBuilder.Errors (
  PlutusToCardanoError (..),
  GYConversionError (..),
  GYQueryUTxOError (..),
  GYQueryDatumError (..),
  GYObtainTxBodyContentError (..),
  GYTxMonadException (..),
  GYBuildTxError (..),
  GYBalancingError (..),
  throwAppError,
) where

import Control.Monad.Except (MonadError, throwError)

import Cardano.Slotting.Time (SystemStart)
import PlutusLedgerApi.V1.Value qualified as Plutus (Value)

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Conway qualified as Ledger
import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Transaction.Common
import GeniusYield.Types.Address (GYAddress)
import GeniusYield.Types.Datum (GYDatum, GYDatumHash)
import GeniusYield.Types.Era (ApiEra)
import GeniusYield.Types.Ledger (PlutusToCardanoError (..))
import GeniusYield.Types.Script.ScriptHash (GYScriptHash)
import GeniusYield.Types.Slot (GYSlot)
import GeniusYield.Types.Time (GYTime)
import GeniusYield.Types.TxIn (GYTxIn)
import GeniusYield.Types.TxOutRef (GYTxOutRef)
import GeniusYield.Types.UTxO (GYOutDatum, GYUTxO)
import GeniusYield.Types.Value (GYFromPlutusValueError)

-------------------------------------------------------------------------------
-- Exception
-------------------------------------------------------------------------------

-- | 'GYConversionError's may be raised during type conversions.
data GYConversionError
  = -- | An address was expected to contain a pub key hash, but it did not.
    GYNotPubKeyAddress !GYAddress
  | -- | An address was expected to contain a script hash, but it did not.
    GYNotScriptAddress !GYAddress
  | -- | Raised during Plutus Value to 'GeniusYield.Types.Value.GYValue' conversion.
    GYInvalidPlutusValue !GYFromPlutusValueError !Plutus.Value
  | -- | Raised during Plutus asset to GY asset conversion.
    GYInvalidPlutusAsset !GYFromPlutusValueError
  | -- | Raised when trying to parse 'Text' into 'GYAddress'.
    GYInvalidAddressText !Text
  | -- | Raised when trying to convert EraHistory to GYSlotConfig.
    GYEraSummariesToSlotConfigError !Text
  | -- | Errors raised during plutus-ledger -> cardano api type conversion.
    GYLedgerToCardanoError !PlutusToCardanoError
  | -- | Errors raised by "GeniusYield.Types.Value.parseAssetClassCore" and similar.
    GYInvalidAssetClass !Text
  | -- | Errors caused by "GeniusYield.Types.Slot.slotFromInteger" resulting in 'Nothing'.
    GYInvalidSlot !Integer
  deriving stock Show

-- | 'GYQueryUTxOError's may be raised during utxo related queries.
data GYQueryUTxOError
  = -- | An address was queried for one or more UTxOs but none were found.
    GYNoUtxosAtAddress ![GYAddress]
  | -- | No UTxO exists at given ref.
    GYNoUtxoAtRef !GYTxOutRef
  deriving stock Show

-- | 'GYQueryDatumError' may be raised during fetching and parsing datums.
data GYQueryDatumError
  = -- | No datum found for given hash.
    GYNoDatumForHash !GYDatumHash
  | -- | Datum parsing failed.
    GYInvalidDatum !GYDatum
  | -- | No datum hash at utxo.
    GYNoDatumHash !GYUTxO
  deriving stock Show

data GYObtainTxBodyContentError
  = -- | No script found for given hash.
    GYNoScriptForHash !GYScriptHash
  | -- | No redeemer found for given purpose.
    GYNoRedeemerForPurpose !(Ledger.ConwayPlutusPurpose Ledger.AsIx (Ledger.ConwayEra))
  | -- | 'GYCertificate' can't be obtained from given api certificate.
    GYInvalidCertificate !(Api.Certificate ApiEra)
  deriving stock Show

{- | Exceptions raised within the 'GeniusYield.TxBuilder.Class.GYTxMonad' computation.

This includes exceptions raised within the contract itself. It does not include:

- Exceptions that may be raised by the provider.
- Exceptions raised during transaction building/balancing.
- Other wildcard exceptions raised within IO.
-}
data GYTxMonadException :: Type where
  -- | Errors encountered during type conversions.
  GYConversionException :: GYConversionError -> GYTxMonadException
  -- | Errors encountered during utxo related queries.
  GYQueryUTxOException :: GYQueryUTxOError -> GYTxMonadException
  -- | Errors encountered during transaction building related functions.
  GYBuildTxException :: GYBuildTxError -> GYTxMonadException
  -- | Raised when no suitable collateral of at least 'tmeMinLovelace' amount is found at 'tmeAddress'.
  GYNoSuitableCollateralException :: {tmeMinLovelace :: Natural, tmeAddress :: GYAddress} -> GYTxMonadException
  -- | Raised if 'tmeSlot' value overflows when advancing it by 'tmeAdvanceAmount'.
  GYSlotOverflowException :: {tmeSlot :: GYSlot, tmeAdvanceAmount :: Natural} -> GYTxMonadException
  -- | Raised during time -> slot conversion, if given timestamp is before known system start.
  GYTimeUnderflowException :: SystemStart -> GYTime -> GYTxMonadException
  -- | Raised during fetching/parsing datums.
  GYQueryDatumException :: GYQueryDatumError -> GYTxMonadException
  -- | Raised when obtaining tx body content.
  GYObtainTxBodyContentException :: GYObtainTxBodyContentError -> GYTxMonadException
  -- | When actual datum in the UTxO is different than what is mentioned for in witness.
  GYDatumMismatch :: GYOutDatum -> GYTxIn v -> GYTxMonadException
  -- | Wildcard user application specific errors. This is the "plug-in" point where an application
  --     using the GY framework, can raise its own protocol specific errors within 'GeniusYield.TxBuilder.Class.GYTxMonad'.
  GYApplicationException :: (Exception e, IsGYApiError e) => e -> GYTxMonadException

deriving instance Show GYTxMonadException

instance Exception GYTxMonadException

-- | Throw an application specific exception ('GYApplicationException') within 'GeniusYield.TxBuilder.Class.GYTxMonad'.
throwAppError :: (IsGYApiError e, Exception e, MonadError GYTxMonadException m) => e -> m a
throwAppError = throwError . GYApplicationException
