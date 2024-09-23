{- |
Module      : GeniusYield.Types.Blueprint.Schema
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Schema (
  Schema (..),
  PairSchema (..),
  ConstructorSchema (..),
  MapSchema (..),
  ListSchema (..),
  ListItemSchema (..),
  BytesSchema (..),
  emptyBytesSchema,
  IntegerSchema (..),
  emptyIntegerSchema,
  SchemaInfo (..),
  emptySchemaInfo,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), Value (..), withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as AesonMap
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Deriving.Aeson
import GHC.Natural (Natural)
import GeniusYield.Imports (Text)
import GeniusYield.Types.Blueprint.DefinitionId (DefinitionId)
import Maestro.Types.Common (LowerFirst)

-- | Blueprint schema definition, as defined by the CIP-0057.
data Schema
  = SchemaInteger SchemaInfo IntegerSchema
  | SchemaBytes SchemaInfo BytesSchema
  | SchemaList SchemaInfo ListSchema
  | SchemaMap SchemaInfo MapSchema
  | SchemaConstructor SchemaInfo ConstructorSchema
  | SchemaBuiltInData SchemaInfo
  | SchemaBuiltInUnit SchemaInfo
  | SchemaBuiltInBoolean SchemaInfo
  | SchemaBuiltInInteger SchemaInfo
  | SchemaBuiltInBytes SchemaInfo
  | SchemaBuiltInString SchemaInfo
  | SchemaBuiltInPair SchemaInfo PairSchema
  | SchemaBuiltInList SchemaInfo Schema
  | SchemaOneOf (NonEmpty Schema)
  | SchemaAnyOf (NonEmpty Schema)
  | SchemaAllOf (NonEmpty Schema)
  | SchemaNot Schema
  | SchemaDefinitionRef DefinitionId
  deriving stock (Eq, Ord, Show)

-- | Additional information optionally attached to any datatype schema definition.
data SchemaInfo = MkSchemaInfo
  { title :: Maybe Text
  , description :: Maybe Text
  , comment :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

emptySchemaInfo :: SchemaInfo
emptySchemaInfo = MkSchemaInfo Nothing Nothing Nothing

data IntegerSchema = MkIntegerSchema
  { isMultipleOf :: Maybe Integer
  -- ^ An instance is valid if division by this value results in an integer.
  , isMinimum :: Maybe Integer
  -- ^ An instance is valid only if it is greater than or exactly equal to "minimum".
  , isMaximum :: Maybe Integer
  -- ^ An instance is valid only if it is less than or exactly equal to "maximum".
  , isExclusiveMinimum :: Maybe Integer
  -- ^ An instance is valid only if it is strictly greater than "exclusiveMinimum".
  , isExclusiveMaximum :: Maybe Integer
  -- ^ An instance is valid only if it is strictly less than "exclusiveMaximum".
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "is", LowerFirst]] IntegerSchema

emptyIntegerSchema :: IntegerSchema
emptyIntegerSchema = MkIntegerSchema Nothing Nothing Nothing Nothing Nothing

data BytesSchema = MkBytesSchema
  { bsEnum :: [ByteString]
  -- ^ An instance validates successfully if once hex-encoded,
  -- its value matches one of the specified values.
  , bsMinLength :: Maybe Natural
  -- ^ An instance is valid if its length is greater than, or equal to, this value.
  , bsMaxLength :: Maybe Natural
  -- ^ An instance is valid if its length is less than, or equal to, this value.
  }
  deriving stock (Eq, Ord, Show)

emptyBytesSchema :: BytesSchema
emptyBytesSchema = MkBytesSchema [] Nothing Nothing

instance FromJSON BytesSchema where
  parseJSON = withObject "BytesSchema" $ \o ->
    MkBytesSchema
      <$> ((o .:? "enum") >>= maybe (pure []) (traverse fromText))
      <*> o .:? "minLength"
      <*> o .:? "maxLength"
   where
    fromText :: Text -> Parser ByteString
    fromText t = case BS16.decode (encodeUtf8 t) of
      Left e -> fail $ "Couldn't decode hex value: " <> Text.unpack t <> ", " <> e
      Right b -> pure b

data ListItemSchema = ListItemSchemaSchema Schema | ListItemSchemaSchemas [Schema]
  deriving stock (Show, Eq, Ord)

data ListSchema = MkListSchema
  { lsItems :: ListItemSchema
  -- ^ Element schema.
  , lsMinItems :: Maybe Natural
  -- ^ An array instance is valid if its size is greater than, or equal to, this value.
  , lsMaxItems :: Maybe Natural
  -- ^ An array instance is valid if its size is less than, or equal to, this value.
  , lsUniqueItems :: Maybe Bool
  -- ^ If this value is false, the instance validates successfully.
  -- If it is set to True, the instance validates successfully if all of its elements are unique.
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON ListSchema where
  parseJSON = withObject "ListSchema" $ \o ->
    MkListSchema
      <$> (ListItemSchemaSchema <$> o .: "items" <|> ListItemSchemaSchemas <$> o .: "items")
      <*> o .:? "minItems"
      <*> o .:? "maxItems"
      <*> o .:? "uniqueItems"

data MapSchema = MkMapSchema
  { msKeys :: Schema
  -- ^ Key schema.
  , msValues :: Schema
  -- ^ Value schema.
  , msMinItems :: Maybe Natural
  -- ^ A map instance is valid if its size is greater than, or equal to, this value.
  , msMaxItems :: Maybe Natural
  -- ^ A map instance is valid if its size is less than, or equal to, this value.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ms", LowerFirst]] MapSchema

data ConstructorSchema = MkConstructorSchema
  { csIndex :: Natural
  -- ^ Constructor index.
  , csFields :: [Schema]
  -- ^ Field schemas.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "cs", LowerFirst]] ConstructorSchema

data PairSchema = MkPairSchema
  { psLeft :: Schema
  -- ^ Schema of the first element.
  , psRight :: Schema
  -- ^ Schema of the second element.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "ps", LowerFirst]] PairSchema

instance FromJSON Schema where
  parseJSON = withObject "Schema" $ \o -> do
    let info = MkSchemaInfo <$> o .:? "title" <*> o .:? "description" <*> o .:? "comment"
    dt :: Maybe Text <- o .:? "dataType"
    case dt of
      Just dt' ->
        case dt' of
          "integer" -> SchemaInteger <$> info <*> parseJSON (Object o)
          "bytes" -> SchemaBytes <$> info <*> parseJSON (Object o)
          "list" -> SchemaList <$> info <*> parseJSON (Object o)
          "map" -> SchemaMap <$> info <*> parseJSON (Object o)
          "constructor" -> SchemaConstructor <$> info <*> parseJSON (Object o)
          "#unit" -> SchemaBuiltInUnit <$> info
          "#boolean" -> SchemaBuiltInBoolean <$> info
          "#integer" -> SchemaBuiltInInteger <$> info
          "#bytes" -> SchemaBuiltInBytes <$> info
          "#string" -> SchemaBuiltInString <$> info
          "#pair" -> SchemaBuiltInPair <$> info <*> parseJSON (Object o)
          "#list" -> SchemaBuiltInList <$> info <*> o .: "items"
          e -> fail $ "Invalid data type: " <> Text.unpack e
      Nothing ->
        if
          | AesonMap.member "oneOf" o -> SchemaOneOf <$> o .: "oneOf"
          | AesonMap.member "anyOf" o -> SchemaAnyOf <$> o .: "anyOf"
          | AesonMap.member "allOf" o -> SchemaAllOf <$> o .: "allOf"
          | AesonMap.member "not" o -> SchemaNot <$> o .: "not"
          | AesonMap.member "$ref" o -> SchemaDefinitionRef <$> o .: "$ref" -- TODO: Remove definitions prefix.
          | otherwise -> SchemaBuiltInData <$> info
