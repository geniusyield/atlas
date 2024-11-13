{- |
Module      : GeniusYield.Types.Blueprint.DefinitionId
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.DefinitionId (DefinitionId, mkDefinitionId, unDefinitionId) where

import Data.Aeson (FromJSON (..), FromJSONKey, FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..), withText)
import Data.Aeson.Types (FromJSONKey (..), toJSONKeyText)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GeniusYield.Imports ((>>>))

newtype DefinitionId = DefinitionId Text
  deriving stock (Show, Eq, Ord)

-- >>> mkDefinitionId "#/definitions/baz~1ParamConstr"
-- DefinitionId "baz/ParamConstr"
mkDefinitionId :: Text -> DefinitionId
mkDefinitionId = DefinitionId . preprocessText
 where
  preprocessText = Text.replace "~1" "/" >>> \t -> fromMaybe t (Text.stripPrefix "#/definitions/" t)

unDefinitionId :: DefinitionId -> Text
unDefinitionId (DefinitionId t) = t

instance FromJSON DefinitionId where
  parseJSON = withText "DefinitionId" (pure . mkDefinitionId)

instance ToJSON DefinitionId where
  toJSON = toJSON . unDefinitionId

instance FromJSONKey DefinitionId where
  fromJSONKey = FromJSONKeyText mkDefinitionId

instance ToJSONKey DefinitionId where
  toJSONKey = toJSONKeyText unDefinitionId
