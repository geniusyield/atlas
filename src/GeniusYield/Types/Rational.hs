{-|
Module      : GeniusYield.Types.Rational
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Rational
    ( GYRational
    , rationalFromGHC
    , rationalToGHC
    , rationalFromPlutus
    , rationalToPlutus
    ) where

import           GeniusYield.Imports

import           Control.Lens                 ((?~))
import qualified Data.Aeson                   as Aeson
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import qualified Data.Swagger.Lens            ()
import qualified Data.Text                    as Text
import qualified Data.Text.Read               as Text
import qualified PlutusTx.Ratio               as Plutus
import qualified Web.HttpApiData              as Web
import qualified Web.Internal.HttpApiData     as Web

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Text.Printf                (printf)
-- >>> import qualified Web.HttpApiData            as Web

-------------------------------------------------------------------------------
-- GYRational
-------------------------------------------------------------------------------

newtype GYRational = GYRational Rational
    deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac)

rationalFromGHC :: Rational -> GYRational
rationalFromGHC = coerce

rationalToGHC :: GYRational -> Rational
rationalToGHC = coerce

rationalFromPlutus :: Plutus.Rational -> GYRational
rationalFromPlutus = rationalFromGHC . Plutus.toGHC

rationalToPlutus :: GYRational -> Plutus.Rational
rationalToPlutus = Plutus.fromGHC . rationalToGHC

-- |
--
-- >>> printf "%6.4f\n" $ fromRational @GYRational 0.123
-- 0.1230
--
instance PrintfArg GYRational where
    formatArg = formatArg . fromRational @Double . coerce

-- |
--
-- >>> Web.parseUrlPiece @GYRational "0.123"
-- Right (GYRational (123 % 1000))
--
instance Web.FromHttpApiData GYRational where
  parseUrlPiece = Web.runReader Text.rational

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode (fromRational @GYRational 0.123)
-- "0.123"
--
instance Aeson.ToJSON GYRational where
    toJSON = Aeson.toJSON . show . fromRational @Double . coerce

-- |
--
-- >>> Aeson.decode @GYRational "\"0.123\""
-- Just (GYRational (123 % 1000))
--
-- >>> Aeson.eitherDecode @GYRational "\"Haskell\""
-- Left "Error in $: could not parse: `Haskell' (input does not start with a digit)"
--
instance Aeson.FromJSON GYRational where
    parseJSON = Aeson.withText "GYRational" $ \t ->
        case Web.parseUrlPiece t of
            Left err -> fail $ Text.unpack err
            Right x  -> return x

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToSchema GYRational where
  declareNamedSchema p = Swagger.plain $ Swagger.paramSchemaToSchema p
                       & Swagger.type_   ?~ Swagger.SwaggerString
                       & Swagger.format  ?~ "float"
                       & Swagger.example ?~ toJSON ("0.125" :: String)

instance Swagger.ToParamSchema GYRational where
  toParamSchema _ = mempty
                  & Swagger.type_  ?~ Swagger.SwaggerString
                  & Swagger.format ?~ "float"
