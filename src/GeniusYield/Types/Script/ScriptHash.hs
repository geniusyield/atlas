{- |
Module      : GeniusYield.Types.Script.ScriptHash
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Script.ScriptHash (
  GYScriptHash,
  scriptHashFromApi,
  scriptHashToApi,
  scriptHashToLedger,
  scriptHashFromLedger,
  apiHashFromPlutus,
  apiHashToPlutus,
  scriptHashFromPlutus,
  scriptHashToPlutus,
  GYValidatorHash,
  validatorHashToPlutus,
  validatorHashFromPlutus,
  validatorHashToApi,
  validatorHashFromApi,
  GYStakeValidatorHash,
  stakeValidatorHashToPlutus,
  stakeValidatorHashFromPlutus,
  stakeValidatorHashToApi,
  stakeValidatorHashFromApi,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Internal.Script qualified as Api
import Cardano.Ledger.Hashes qualified as Ledger
import Data.Text qualified as Text
import GeniusYield.Imports
import GeniusYield.Types.Ledger (PlutusToCardanoError (..))
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Text.Printf qualified as Printf
import Web.HttpApiData qualified as Web

{- $setup

>>> import GeniusYield.Imports
-}

newtype GYScriptHash = GYScriptHash Api.ScriptHash
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

{- |

>>> "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYScriptHash
GYScriptHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
-}
instance IsString GYScriptHash where
  fromString = GYScriptHash . fromString

{- |

>>> printf "%s" ("cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYScriptHash)
cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0
-}
instance Printf.PrintfArg GYScriptHash where
  formatArg (GYScriptHash h) = formatArg $ init $ tail $ show h

-- >>> Web.toUrlPiece (GYScriptHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0")
-- "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
--
instance Web.ToHttpApiData GYScriptHash where
  toUrlPiece = Api.serialiseToRawBytesHexText . scriptHashToApi

scriptHashToApi :: GYScriptHash -> Api.ScriptHash
scriptHashToApi = coerce

scriptHashFromApi :: Api.ScriptHash -> GYScriptHash
scriptHashFromApi = coerce

-- | Convert to corresponding ledger representation.
scriptHashToLedger :: GYScriptHash -> Ledger.ScriptHash
scriptHashToLedger = scriptHashToApi >>> Api.toShelleyScriptHash

-- | Convert from corresponding ledger representation.
scriptHashFromLedger :: Ledger.ScriptHash -> GYScriptHash
scriptHashFromLedger = Api.fromShelleyScriptHash >>> scriptHashFromApi

apiHashToPlutus :: Api.ScriptHash -> PlutusV1.ScriptHash
apiHashToPlutus h = PlutusV1.ScriptHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes h

apiHashFromPlutus :: PlutusV1.ScriptHash -> Either Api.SerialiseAsRawBytesError Api.ScriptHash
apiHashFromPlutus (PlutusV1.ScriptHash (BuiltinByteString bs)) = Api.deserialiseFromRawBytes Api.AsScriptHash bs

scriptHashToPlutus :: GYScriptHash -> PlutusV1.ScriptHash
scriptHashToPlutus = scriptHashToApi >>> apiHashToPlutus

scriptHashFromPlutus :: PlutusV1.ScriptHash -> Either PlutusToCardanoError GYScriptHash
scriptHashFromPlutus = validatorHashFromPlutus

{-# DEPRECATED GYValidatorHash "Use GYScriptHash." #-}
type GYValidatorHash = GYScriptHash

validatorHashToPlutus :: GYValidatorHash -> PlutusV1.ScriptHash
validatorHashToPlutus = apiHashToPlutus . validatorHashToApi

{- |

>>> validatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
Right (GYScriptHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0")

>>> validatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7"
Left (DeserialiseRawBytesError {ptceTag = "validatorHashFromPlutus: cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7, error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise ScriptHash\"}"})
-}
validatorHashFromPlutus :: PlutusV1.ScriptHash -> Either PlutusToCardanoError GYValidatorHash
validatorHashFromPlutus vh@(PlutusV1.ScriptHash ibs) =
  bimap
    (\e -> DeserialiseRawBytesError $ Text.pack $ "validatorHashFromPlutus: " <> show vh <> ", error: " <> show e)
    validatorHashFromApi
    $ Api.deserialiseFromRawBytes Api.AsScriptHash
    $ PlutusTx.fromBuiltin ibs

validatorHashToApi :: GYValidatorHash -> Api.ScriptHash
validatorHashToApi = coerce

validatorHashFromApi :: Api.ScriptHash -> GYValidatorHash
validatorHashFromApi = coerce

{-# DEPRECATED GYStakeValidatorHash "Use GYScriptHash." #-}
type GYStakeValidatorHash = GYScriptHash

stakeValidatorHashToPlutus :: GYStakeValidatorHash -> PlutusV1.ScriptHash
stakeValidatorHashToPlutus = apiHashToPlutus . stakeValidatorHashToApi

{- |

>>> stakeValidatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
Right (GYScriptHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0")

>>> stakeValidatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7"
Left (DeserialiseRawBytesError {ptceTag = "stakeValidatorHashFromPlutus: cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7, error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise ScriptHash\"}"})
-}
stakeValidatorHashFromPlutus :: PlutusV1.ScriptHash -> Either PlutusToCardanoError GYStakeValidatorHash
stakeValidatorHashFromPlutus vh@(PlutusV1.ScriptHash ibs) =
  bimap
    (\e -> DeserialiseRawBytesError $ Text.pack $ "stakeValidatorHashFromPlutus: " <> show vh <> ", error: " <> show e)
    stakeValidatorHashFromApi
    $ Api.deserialiseFromRawBytes Api.AsScriptHash
    $ PlutusTx.fromBuiltin ibs

stakeValidatorHashToApi :: GYStakeValidatorHash -> Api.ScriptHash
stakeValidatorHashToApi = coerce

stakeValidatorHashFromApi :: Api.ScriptHash -> GYStakeValidatorHash
stakeValidatorHashFromApi = coerce
