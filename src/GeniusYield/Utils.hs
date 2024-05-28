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
    , swaggerToOpenApiSchema'
    ) where

import           Cardano.Api                  (SerialiseAsRawBytes (serialiseToRawBytes))
import           Codec.Binary.Bech32          as Bech32
import           Control.Monad.Except         (ExceptT (..))
import           Data.Char                    (toLower)
import           Control.Lens                 ((.~), (^.))
import qualified Data.OpenApi                 as OpenApi
import           Data.OpenApi                 (OpenApiType(..))
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal        as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import           GeniusYield.Imports


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

-- Lift a 'Swagger.Schema' to an 'OpenApi.Schema'
liftSwaggerSchema :: Swagger.Schema -> OpenApi.Schema
liftSwaggerSchema swaggerSchema = 
  mempty
  & OpenApi.type_       .~ convertSwaggerType (swaggerSchema ^. Swagger.type_)
  & OpenApi.format      .~ (swaggerSchema ^. Swagger.format)
  & OpenApi.pattern     .~ (swaggerSchema ^. Swagger.pattern)
  & OpenApi.description .~ (swaggerSchema ^. Swagger.description)
  & OpenApi.example     .~ (swaggerSchema ^. Swagger.example)
  & OpenApi.maxLength   .~ (swaggerSchema ^. Swagger.maxLength)
  & OpenApi.minLength   .~ (swaggerSchema ^. Swagger.minLength)
  -- & OpenApi.additionalProperties .~ (swaggerSchema ^. Swagger.additionalProperties)  --TODO

-- Convert a Swagger.NamedSchema to an OpenApi.NamedSchema
convertNamedSchema :: Swagger.NamedSchema -> OpenApi.NamedSchema
convertNamedSchema (Swagger.NamedSchema name swaggerSchema) =
  OpenApi.NamedSchema name (liftSwaggerSchema swaggerSchema)

-- | Lift Swagger schema to OpenApi schema (without ToParamSchema instance)
swaggerToOpenApiSchema :: forall a. Swagger.ToSchema a => Proxy a -> OpenApi.NamedSchema
swaggerToOpenApiSchema proxy = convertNamedSchema $ Swagger.toNamedSchema proxy

-- | Lift Swagger schema to OpenApi schema (with ToParamSchema instance)
swaggerToOpenApiSchema' :: forall a. (Swagger.ToSchema a, Swagger.ToParamSchema a)
                        => Text -> Proxy a -> OpenApi.NamedSchema
swaggerToOpenApiSchema' name proxy = OpenApi.NamedSchema (Just name) . liftSwaggerSchema $
                                     Swagger.paramSchemaToSchema proxy

