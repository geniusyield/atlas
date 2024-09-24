{- |
Module      : GeniusYield.Types.Blueprint.Parameter
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Parameter (ParameterBlueprint (..)) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Set (Set)
import Data.Set qualified as Set
import Deriving.Aeson
import GeniusYield.Aeson.Utils (buildObject, optionalField, requiredField)
import GeniusYield.Imports (Text)
import GeniusYield.Types.Blueprint.Argument (oneOfASet)
import GeniusYield.Types.Blueprint.Purpose (Purpose)
import GeniusYield.Types.Blueprint.Schema (Schema)

-- | Blueprint that defines validator's compile-time parameter.
data ParameterBlueprint = MkParameterBlueprint
  { parameterTitle :: Maybe Text
  -- ^ A short and descriptive name for the parameter.
  , parameterDescription :: Maybe Text
  -- ^ An informative description of the parameter.
  , parameterPurpose :: Set Purpose
  -- ^ One of "spend", "mint", "withdraw" or "publish", or a oneOf applicator of those.
  , parameterSchema :: Schema
  -- ^ A Plutus Data Schema.
  }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON ParameterBlueprint where
  parseJSON = withObject "ParameterBlueprint" $ \o ->
    MkParameterBlueprint
      <$> o .:? "title"
      <*> o .:? "description"
      <*> ((o .:? "purpose") >>= parsePurpose)
      <*> o .: "schema"
   where
    parsePurpose :: Maybe Value -> Parser (Set Purpose)
    parsePurpose Nothing = pure Set.empty
    parsePurpose (Just v) =
      (Set.singleton <$> parseJSON v)
        <|> withObject "Purpose" (\o -> Set.fromList <$> o .: "oneOf") v

instance ToJSON ParameterBlueprint where
  toJSON MkParameterBlueprint {..} =
    buildObject $
      optionalField "title" parameterTitle
        . optionalField "description" parameterDescription
        . optionalField "purpose" (oneOfASet parameterPurpose)
        . requiredField "schema" parameterSchema
