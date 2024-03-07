{-# LANGUAGE UnicodeSyntax #-}

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
    , addSwaggerExample
    , addSwaggerDescription
    ) where

import           Cardano.Api          (SerialiseAsRawBytes (serialiseToRawBytes))
import           Codec.Binary.Bech32  as Bech32
import           Control.Lens         (mapped, (?~))
import           Control.Monad.Except (ExceptT (..))
import           Data.Char            (toLower)
import qualified Data.Swagger         as Swagger
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

-- | Utility function to add swagger description to a schema.
addSwaggerDescription ∷ (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasDescription a (Maybe b2)) ⇒ b2 → f1 (f2 b1) → f1 (f2 b1)
addSwaggerDescription desc = mapped . mapped . Swagger.schema . Swagger.description ?~ desc

-- | Utility function to add swagger example to a schema.
addSwaggerExample ∷ (Functor f1, Functor f2, Swagger.HasSchema b1 a, Swagger.HasExample a (Maybe b2)) ⇒ b2 → f1 (f2 b1) → f1 (f2 b1)
addSwaggerExample ex = mapped . mapped . Swagger.schema . Swagger.example ?~ ex