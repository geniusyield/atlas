{-|
Module      : GeniusYield.Types.Natural
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Natural
    ( GYNatural
    , naturalFromGHC
    , naturalToGHC
    ) where

import           GeniusYield.Imports

import           Control.Lens                 ((?~))
import qualified Data.Aeson                   as Aeson
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import qualified Data.Swagger.Lens            ()
import qualified Data.Text                    as Text
import qualified Web.HttpApiData              as Web

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Data.Proxy
-- >>> import qualified Data.Swagger               as Swagger

-------------------------------------------------------------------------------
-- GYNatural
-------------------------------------------------------------------------------

-- | Cardano allows token mint amount to be as large as @9_223_372_036_854_775_807@ which may not be represented correct in Javascript's @number@ type, consequently, such large integers are to be better given as text in JSON. This wrapper type around `Natural` gives modified `FromJSON` and `ToJSON` instances so to work with `Text` instead.
newtype GYNatural = GYNatural Natural
    deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Ord, Num, Real, Enum, Integral, PrintfArg, Web.FromHttpApiData, Web.ToHttpApiData)

naturalFromGHC :: Natural -> GYNatural
naturalFromGHC = coerce

naturalToGHC :: GYNatural -> Natural
naturalToGHC = coerce

-- |
--
-- >>> Aeson.decode @GYNatural "\"123\""
-- Just (GYNatural 123)
--
-- >>> Aeson.eitherDecode @GYNatural "\"-123\""
-- Left "Error in $: underflow: -123 (should be a non-negative integer)"
--
-- >>> Aeson.eitherDecode @GYNatural "\"+123\""
-- Right (GYNatural 123)
--
-- >>> Aeson.eitherDecode @GYNatural "\"9223372036854775807\""
-- Right (GYNatural 9223372036854775807)
--
-- >>> Aeson.eitherDecode @GYNatural "\"123456789123456789123456789123456789123456789\""
-- Right (GYNatural 123456789123456789123456789123456789123456789)
--
-- >>> Aeson.eitherDecode @GYNatural "\"0011\""
-- Right (GYNatural 11)
--
-- >>> Aeson.eitherDecode @GYNatural "\"0f11\""
-- Left "Error in $: could not parse: `0f11'"
--
-- >>> Aeson.eitherDecode @GYNatural "\"-123456789123456789123456789123456789123456789\""
-- Left "Error in $: underflow: -123456789123456789123456789123456789123456789 (should be a non-negative integer)"
--
instance Aeson.FromJSON GYNatural where
    parseJSON = Aeson.withText "GYNatural" $ \t ->
        case Web.parseUrlPiece t of
            Left err -> fail $ Text.unpack err
            Right x  -> return x

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode (1234 :: GYNatural)
-- "1234"
--
-- >>> LBS8.putStrLn $ Aeson.encode (123456789123456789123456789123456789123456789 :: GYNatural)
-- "123456789123456789123456789123456789123456789"
--
instance Aeson.ToJSON GYNatural where
    toJSON = Aeson.toJSON . Web.toUrlPiece

instance Swagger.ToParamSchema GYNatural where
  toParamSchema _ = mempty
                  & Swagger.type_  ?~ Swagger.SwaggerString

-- |
--
-- >>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy GYNatural))
-- "{\"description\":\"A natural number which is a non-negative integer. Minimum value is 0.\",\"example\":\"123456789123456789123456789123456789123456789\",\"type\":\"string\"}"
--
instance Swagger.ToSchema GYNatural where
  declareNamedSchema p =
    pure $ Swagger.named "GYNatural" $
               Swagger.paramSchemaToSchema p
             & Swagger.example ?~ toJSON ("123456789123456789123456789123456789123456789" :: String)
             & Swagger.description ?~ "A natural number which is a non-negative integer. Minimum value is 0."

