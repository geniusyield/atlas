{-|
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
) where

import qualified Cardano.Api         as Api
import           GeniusYield.Imports
import qualified Text.Printf         as Printf
import qualified Web.HttpApiData     as Web

-- $setup
--
-- >>> import GeniusYield.Imports

newtype GYScriptHash = GYScriptHash Api.ScriptHash
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

-- |
--
-- >>> "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYScriptHash
-- GYScriptHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
--
instance IsString GYScriptHash where
    fromString = GYScriptHash . fromString

-- |
--
-- >>> printf "%s" ("cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYScriptHash)
-- cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0
--
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
