{-|
Module      : GeniusYield.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Utils
    ( fieldNamePrefixStrip2
    , fieldNamePrefixStrip3
    , fieldNamePrefixStrip4
    , fieldNamePrefixStripN
    , modifyException
    , serialiseToBech32WithPrefix
    , swaggerToOpenApiSchema
    , convertNamedSchema
    ) where

import           Cardano.Api          (SerialiseAsRawBytes (serialiseToRawBytes))
import           Codec.Binary.Bech32  as Bech32
import           Control.Monad.Except (ExceptT (..))
import           Data.Char            (toLower)
import           GeniusYield.Imports

import           Data.Aeson    (Value)
import           Control.Lens                     ((.~), (^.))
import qualified Data.OpenApi                     as OpenApi
import           Data.OpenApi                     (OpenApiType(..))  -- , ToSchema(..))
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal            as Swagger
-- import qualified Data.Swagger.Internal.Schema     as Swagger


-- | @fieldNamePrefixStrip2 "muAssets" == "assets"@
fieldNamePrefixStrip2 :: String -> String
fieldNamePrefixStrip2 = fieldNamePrefixStripN 2

-- | @fieldNamePrefixStrip3 "msnNumber" == "number"@
fieldNamePrefixStrip3 :: String -> String
fieldNamePrefixStrip3 = fieldNamePrefixStripN 3

-- | @fieldNamePrefixStrip4 "mmswMemory" == "memory"@
fieldNamePrefixStrip4 :: String -> String
fieldNamePrefixStrip4 = fieldNamePrefixStripN 4

-- | Strip n characters from a field name and lower case the first character (if any) of the result.
fieldNamePrefixStripN :: Int -> String -> String
fieldNamePrefixStripN n fldName = case drop n fldName of x : xs -> toLower x : xs; [] -> []

-- | Map the exception type in an 'ExceptT' with a function.
modifyException :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
modifyException f (ExceptT meith) = ExceptT $ first f <$> meith

serialiseToBech32WithPrefix :: SerialiseAsRawBytes a => Text -> a -> Text
serialiseToBech32WithPrefix prefix =
  case Bech32.humanReadablePartFromText prefix of
    Left e  -> error $ "serialiseToBech32WithPrefix: invalid prefix "
                    ++ show prefix
                    ++ ", " ++ show e
    Right p -> serialiseToRawBytes >>> Bech32.dataPartFromBytes >>> Bech32.encodeLenient p


-- Convert SwaggerType to OpenApiType
convertSwaggerType :: Maybe (Swagger.SwaggerType 'Swagger.SwaggerKindSchema) -> Maybe OpenApiType
convertSwaggerType (Just Swagger.SwaggerString)  = Just OpenApiString
convertSwaggerType (Just Swagger.SwaggerNumber)  = Just OpenApiNumber
convertSwaggerType (Just Swagger.SwaggerInteger) = Just OpenApiInteger
convertSwaggerType (Just Swagger.SwaggerBoolean) = Just OpenApiBoolean
convertSwaggerType (Just Swagger.SwaggerArray)   = Just OpenApiArray
convertSwaggerType (Just Swagger.SwaggerObject)  = Just OpenApiObject
convertSwaggerType _                             = Nothing

-- Convert Swagger.Format to Maybe Text
convertSwaggerFormat :: Maybe Swagger.Format -> Maybe Text
convertSwaggerFormat = id

-- Lift Swagger schema to OpenApi schema
swaggerToOpenApiSchema :: forall a. (Swagger.ToSchema a, Swagger.ToParamSchema a, ToJSON a, FromJSON a) => Text -> Proxy a -> Maybe Value -> OpenApi.NamedSchema
swaggerToOpenApiSchema name proxy example = OpenApi.NamedSchema (Just name) schema
  where
    swaggerSchema = Swagger.paramSchemaToSchema proxy
    schema = mempty
             & OpenApi.type_   .~ convertSwaggerType (swaggerSchema ^. Swagger.type_)
             & OpenApi.format  .~ convertSwaggerFormat (swaggerSchema ^. Swagger.format)
             & OpenApi.pattern .~ (swaggerSchema ^. Swagger.pattern)
             & OpenApi.description .~ (swaggerSchema ^. Swagger.description)
             & OpenApi.example .~ example

-- Convert a Swagger.Schema to an OpenApi.Schema
liftSwagger :: Swagger.Schema -> OpenApi.Schema
liftSwagger swaggerSchema = 
  mempty
  & OpenApi.type_       .~ convertSwaggerType (swaggerSchema ^. Swagger.type_)
  & OpenApi.format      .~ (swaggerSchema ^. Swagger.format)
  & OpenApi.description .~ (swaggerSchema ^. Swagger.description)
  & OpenApi.example     .~ (swaggerSchema ^. Swagger.example)
  & OpenApi.maxLength   .~ (swaggerSchema ^. Swagger.maxLength)
  & OpenApi.minLength   .~ (swaggerSchema ^. Swagger.minLength)

-- Convert a Swagger.NamedSchema to an OpenApi.NamedSchema
convertNamedSchema :: Swagger.NamedSchema -> OpenApi.NamedSchema
convertNamedSchema (Swagger.NamedSchema name swaggerSchema) =
  OpenApi.NamedSchema name (liftSwagger swaggerSchema)
