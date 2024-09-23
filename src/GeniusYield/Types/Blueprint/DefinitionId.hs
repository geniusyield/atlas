{- |
Module      : GeniusYield.Types.Blueprint.DefinitionId
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.DefinitionId (DefinitionId, mkDefinitionId, unDefinitionId) where

import Data.Aeson (FromJSON (..), FromJSONKey, FromJSONKeyFunction (..), withText)
import Data.Aeson.Types (FromJSONKey (..))
import Data.Text (Text)
import Data.Text qualified as Text

newtype DefinitionId = DefinitionId Text
  deriving stock (Show, Eq, Ord)

mkDefinitionId :: Text -> DefinitionId
mkDefinitionId t = case Text.stripPrefix "#/definitions/" t of
  Just rest -> DefinitionId rest
  Nothing -> DefinitionId t

unDefinitionId :: DefinitionId -> Text
unDefinitionId (DefinitionId t) = t

instance FromJSON DefinitionId where
  parseJSON = withText "DefinitionId" (pure . mkDefinitionId)

instance FromJSONKey DefinitionId where
  fromJSONKey = FromJSONKeyText mkDefinitionId