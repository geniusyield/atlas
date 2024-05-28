{-|
Module      : GeniusYield.Types.StakeKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.StakeKeyHash (
    GYStakeKeyHash,
    stakeKeyHashToApi,
    stakeKeyHashFromApi,
) where

import           Control.Lens                 ((?~))
import           GeniusYield.Imports
import           GeniusYield.Utils            (swaggerToOpenApiSchema)

import qualified Cardano.Api                  as Api
import qualified Data.Aeson.Types             as Aeson
import qualified Data.Csv                     as Csv
import qualified Data.OpenApi                 as OpenApi
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import qualified Data.Text.Encoding           as Text
import           GeniusYield.Types.PubKeyHash (AsPubKeyHash (..), CanSignTx)
import qualified Text.Printf                  as Printf
import           Unsafe.Coerce                (unsafeCoerce)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import qualified Data.Csv                   as Csv
-- >>> import qualified Text.Printf                as Printf

newtype GYStakeKeyHash = GYStakeKeyHash (Api.Hash Api.StakeKey)
    deriving stock Show
    deriving newtype (Eq, Ord, IsString)

instance AsPubKeyHash GYStakeKeyHash where
  toPubKeyHash = unsafeCoerce
  fromPubKeyHash = unsafeCoerce

instance CanSignTx GYStakeKeyHash

-- |
--
-- >>> let Just skh = Aeson.decode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
-- >>> stakeKeyHashToApi skh
-- "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
--
stakeKeyHashToApi :: GYStakeKeyHash -> Api.Hash Api.StakeKey
stakeKeyHashToApi = coerce

-- |
--
-- >>> stakeKeyHashFromApi "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-- GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
--
stakeKeyHashFromApi :: Api.Hash Api.StakeKey -> GYStakeKeyHash
stakeKeyHashFromApi = coerce

-- |
--
-- >>> let Just skh = Aeson.decode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
-- >>> LBS8.putStrLn $ Aeson.encode skh
-- "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
--
instance Aeson.ToJSON GYStakeKeyHash where
    toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . stakeKeyHashToApi

-- |
--
-- >>> Aeson.eitherDecode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
-- Right (GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d")
--
-- Invalid characters:
--
-- >>> Aeson.eitherDecode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab2340386323zzz\""
-- Left "Error in $: RawBytesHexErrorBase16DecodeFail \"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab2340386323zzz\" \"invalid character at offset: 53\""
--
instance Aeson.FromJSON GYStakeKeyHash where
    parseJSON = Aeson.withText "GYStakeKeyHash" $
        either
            (fail . show)
            (return . GYStakeKeyHash)
        . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsStakeKey)
        . Text.encodeUtf8

-- |
--
-- >>> Printf.printf "%s\n" $ stakeKeyHashFromApi "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-- 7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d
--
instance Printf.PrintfArg GYStakeKeyHash where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . stakeKeyHashToApi

-- |
--
-- >>> Csv.toField @GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-- "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
--
instance Csv.ToField GYStakeKeyHash where
    toField = Api.serialiseToRawBytesHex . stakeKeyHashToApi

-- |
--
-- >>> Csv.runParser $ Csv.parseField @GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-- Right (GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d")
--
-- >>> Csv.runParser $ Csv.parseField @GYStakeKeyHash "not a pub stake key hash"
-- Left "RawBytesHexErrorBase16DecodeFail \"not a pub stake key hash\" \"invalid character at offset: 0\""
--
instance Csv.FromField GYStakeKeyHash where
    parseField = either (fail . show) (return . stakeKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsStakeKey)

instance OpenApi.ToSchema GYStakeKeyHash where
  declareNamedSchema _ = pure $ swaggerToOpenApiSchema (Proxy @GYStakeKeyHash)

instance Swagger.ToSchema GYStakeKeyHash where
  declareNamedSchema _ = pure $ Swagger.named "GYStakeKeyHash" $ mempty
                       & Swagger.type_           ?~ Swagger.SwaggerString
                       & Swagger.format          ?~ "hex"
                       & Swagger.description     ?~ "The hash of a public stake key."
                       & Swagger.example         ?~ toJSON ("7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d" :: Text)
                       & Swagger.maxLength       ?~ 56
                       & Swagger.minLength ?~ 56
