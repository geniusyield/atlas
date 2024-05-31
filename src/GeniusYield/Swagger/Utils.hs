{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Module      : GeniusYield.Swagger.Utils
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Swagger.Utils
  ( addSwaggerExample
  , addSwaggerDescription
  , dropSymbolAndCamelToSnake
  , fromOpenApi2Schema
  , fromOpenApi2Schema'
  ) where

import           Control.Lens                 (mapped, (?~), (.~), (^.))
import           Data.Aeson                   (camelTo2)
import qualified Data.OpenApi                 as OpenApi
import           Data.OpenApi                 (OpenApiType(..))
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal        as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import           GeniusYield.Imports
import           GHC.TypeLits                 (KnownSymbol, symbolVal)

-- | Utility function to add swagger description to a schema.
addSwaggerDescription :: (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasDescription a (Maybe b2)) => b2 -> f1 (f2 b1) -> f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . Swagger.schema . Swagger.description ?~ desc

-- | Utility function to add swagger example to a schema.
addSwaggerExample :: (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasExample a (Maybe b2)) => b2 -> f1 (f2 b1) -> f1 (f2 b1)
addSwaggerExample ex = mapped . mapped . Swagger.schema . Swagger.example ?~ ex

-- | Drop the applied type symbol and convert camel case to snake case.
dropSymbolAndCamelToSnake :: forall a. (KnownSymbol a) => String -> String
dropSymbolAndCamelToSnake = camelTo2 '_' . drop (length $ symbolVal (Proxy @a))


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

-- Convert a 'Swagger.NamedSchema' to an 'OpenApi.NamedSchema'
convertNamedSchema :: Swagger.NamedSchema -> OpenApi.NamedSchema
convertNamedSchema (Swagger.NamedSchema name swaggerSchema) =
  OpenApi.NamedSchema name (liftSwaggerSchema swaggerSchema)

-- | Lift Swagger schema to OpenApi schema (without ToParamSchema instance)
fromOpenApi2Schema :: forall a. Swagger.ToSchema a => Proxy a -> OpenApi.NamedSchema
fromOpenApi2Schema proxy = convertNamedSchema $ Swagger.toNamedSchema proxy

-- | Lift Swagger schema to OpenApi schema (with ToParamSchema instance)
fromOpenApi2Schema' :: forall a. (Swagger.ToSchema a, Swagger.ToParamSchema a)
                        => Text -> Proxy a -> OpenApi.NamedSchema
fromOpenApi2Schema' name proxy = OpenApi.NamedSchema (Just name) . liftSwaggerSchema $
                                     Swagger.paramSchemaToSchema proxy

