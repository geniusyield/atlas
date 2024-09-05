{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

{- |
Module      : GeniusYield.Swagger.Utils
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Swagger.Utils (
  addSwaggerExample,
  addSwaggerDescription,
  dropSymbolAndCamelToSnake,
) where

import Control.Lens (mapped, (?~))
import Data.Aeson (camelTo2)
import Data.Data (Proxy (Proxy))
import Data.Swagger qualified as Swagger
import GHC.TypeLits (KnownSymbol, symbolVal)

-- | Utility function to add swagger description to a schema.
addSwaggerDescription :: (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasDescription a (Maybe b2)) => b2 -> f1 (f2 b1) -> f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . Swagger.schema . Swagger.description ?~ desc

-- | Utility function to add swagger example to a schema.
addSwaggerExample :: (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasExample a (Maybe b2)) => b2 -> f1 (f2 b1) -> f1 (f2 b1)
addSwaggerExample ex = mapped . mapped . Swagger.schema . Swagger.example ?~ ex

-- | Drop the applied type symbol and convert camel case to snake case.
dropSymbolAndCamelToSnake :: forall a. KnownSymbol a => String -> String
dropSymbolAndCamelToSnake = camelTo2 '_' . drop (length $ symbolVal (Proxy @a))
