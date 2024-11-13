{- |
Module      : GeniusYield.Types.Blueprint.Argument
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Argument (ArgumentBlueprint (..), oneOfASet) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Set (Set)
import Data.Set qualified as Set
import Deriving.Aeson
import GeniusYield.Aeson.Utils (buildObject, optionalField, requiredField)
import GeniusYield.Imports (Text)
import GeniusYield.Types.Blueprint.Purpose (Purpose)
import GeniusYield.Types.Blueprint.Schema (Schema)

-- | Blueprint that defines a validator's runtime argument: datum or redeemer.
data ArgumentBlueprint = MkArgumentBlueprint
  { argumentTitle :: Maybe Text
  -- ^ A short and descriptive name for the redeemer or datum.
  , argumentDescription :: Maybe Text
  -- ^ An informative description of the redeemer or datum.
  , argumentPurpose :: Set Purpose
  -- ^ One of "spend", "mint", "withdraw" or "publish", or a oneOf applicator of those.
  , argumentSchema :: Schema
  -- ^ A Plutus Data Schema.
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON ArgumentBlueprint where
  parseJSON = withObject "ArgumentBlueprint" $ \o ->
    MkArgumentBlueprint
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

oneOfASet :: ToJSON a => Set a -> Maybe Value
oneOfASet s =
  case Set.toList s of
    [] -> Nothing
    [x] -> Just $ toJSON x
    xs -> Just $ object ["oneOf" .= xs]

instance ToJSON ArgumentBlueprint where
  toJSON MkArgumentBlueprint {..} =
    buildObject $
      optionalField "title" argumentTitle
        . optionalField "description" argumentDescription
        . optionalField "purpose" (oneOfASet argumentPurpose)
        . requiredField "schema" argumentSchema
