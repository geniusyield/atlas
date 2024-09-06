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
  apiHashToPlutus,
  scriptHashToPlutus,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Script qualified as Api
import Cardano.Ledger.Hashes qualified as Ledger
import GeniusYield.Imports
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusTx.Builtins qualified as PlutusTx
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
scriptHashToLedger :: GYScriptHash -> Ledger.ScriptHash Ledger.StandardCrypto
scriptHashToLedger = scriptHashToApi >>> Api.toShelleyScriptHash

-- | Convert from corresponding ledger representation.
scriptHashFromLedger :: Ledger.ScriptHash Ledger.StandardCrypto -> GYScriptHash
scriptHashFromLedger = Api.fromShelleyScriptHash >>> scriptHashFromApi

apiHashToPlutus :: Api.ScriptHash -> PlutusV1.ScriptHash
apiHashToPlutus h = PlutusV1.ScriptHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes h

scriptHashToPlutus :: GYScriptHash -> PlutusV1.ScriptHash
scriptHashToPlutus = scriptHashToApi >>> apiHashToPlutus
