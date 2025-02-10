{- |
Module      : GeniusYield.Types.StakePoolId
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.StakePoolId (
  GYStakePoolId,
  stakePoolIdToApi,
  stakePoolIdFromApi,
  stakePoolIdToLedger,
  stakePoolIdFromLedger,
  stakePoolIdFromTextMaybe,
  unsafeStakePoolIdFromText,
  stakePoolIdToText,
  GYStakePoolIdBech32,
  stakePoolIdFromBech32,
  stakePoolIdToBech32,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Control.Lens ((?~))
import Data.Aeson.Types qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as Text
import GeniusYield.Imports
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import Web.HttpApiData qualified as Web

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import qualified Text.Printf                as Printf
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
>>> import qualified Web.HttpApiData            as Web

>>> spId :: GYStakePoolId = "c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27"
-}

-- | @type GYStakePoolId = GYKeyHash 'GYKeyRoleStakePool@
type GYStakePoolId = GYKeyHash 'GYKeyRoleStakePool

{- |

>>> let Just spid = Aeson.decode @GYStakePoolId "\"c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27\""
>>> stakePoolIdToApi spid
"c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27"
-}
stakePoolIdToApi :: GYStakePoolId -> Api.Hash Api.StakePoolKey
stakePoolIdToApi = keyHashToApi

{- |

>>> stakePoolIdFromApi "c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27"
GYKeyHash (GYKeyRoleStakePool) "c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27"
-}
stakePoolIdFromApi :: Api.Hash Api.StakePoolKey -> GYStakePoolId
stakePoolIdFromApi = keyHashFromApi

-- | Convert to corresponding ledger type.
stakePoolIdToLedger :: GYStakePoolId -> Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto
stakePoolIdToLedger = keyHashToLedger

-- | Convert from corresponding ledger type.
stakePoolIdFromLedger :: Ledger.KeyHash Ledger.StakePool Ledger.StandardCrypto -> GYStakePoolId
stakePoolIdFromLedger = keyHashFromLedger

{- | Obtain `GYStakePoolId` from bech32 encoding of stake pool id.

>>> stakePoolIdFromTextMaybe "pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt"
Just (GYKeyHash (GYKeyRoleStakePool) "c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27")
>>> stakePoolIdFromTextMaybe "c485ab20bd3f105e59f3c50a0d3fbaf615a51f70a1c6d29d00a1fd27"
Nothing
-}
stakePoolIdFromTextMaybe :: Text.Text -> Maybe GYStakePoolId
stakePoolIdFromTextMaybe t = case Api.deserialiseFromBech32 (Api.AsHash Api.AsStakePoolKey) t of
  Left _ -> Nothing
  Right h -> Just $ stakePoolIdFromApi h

-- | Like `stakePoolIdFromTextMaybe` but errors on `Nothing` case.
unsafeStakePoolIdFromText :: Text.Text -> GYStakePoolId
unsafeStakePoolIdFromText t =
  fromMaybe
    (error $ "Not a stake pool id: " ++ show t)
    (stakePoolIdFromTextMaybe t)

{- | Serialises `GYStakePoolId` to it's bech32 representation.

>>> stakePoolIdToText spId
"pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt"
-}
stakePoolIdToText :: GYStakePoolId -> Text.Text
stakePoolIdToText = Api.serialiseToBech32 . stakePoolIdToApi

{- | 'GYStakePoolIdBech32' which uses "bech32" format

>>> Web.toUrlPiece $ stakePoolIdToBech32 spId
"pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt"
-}
newtype GYStakePoolIdBech32 = GYStakePoolIdBech32 GYStakePoolId
  deriving newtype (Eq, Ord)

instance Show GYStakePoolIdBech32 where
  show = Web.toUrlPiece >>> Text.unpack

stakePoolIdToBech32 :: GYStakePoolId -> GYStakePoolIdBech32
stakePoolIdToBech32 = coerce

stakePoolIdFromBech32 :: GYStakePoolIdBech32 -> GYStakePoolId
stakePoolIdFromBech32 = coerce

instance Web.ToHttpApiData GYStakePoolIdBech32 where
  toUrlPiece = coerce stakePoolIdToText

instance IsString GYStakePoolIdBech32 where
  fromString = fromRight (error "invalid stake pool id") . Web.parseUrlPiece . Text.pack

{- |

>>> Web.parseUrlPiece @GYStakePoolIdBech32 "pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt"
Right pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt
-}
instance Web.FromHttpApiData GYStakePoolIdBech32 where
  parseUrlPiece t = case stakePoolIdFromTextMaybe t of
    Just stakePoolId -> Right $ coerce stakePoolId
    Nothing -> Left $ "Not a stake pool id: " <> t

{- |

>>> LBS8.putStrLn $ Aeson.encode $ stakePoolIdToBech32 spId
"pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt"
-}
instance ToJSON GYStakePoolIdBech32 where
  toJSON (GYStakePoolIdBech32 stakePoolId) = Aeson.toJSON $ stakePoolIdToText stakePoolId

{- |

>>> Aeson.decode @GYStakePoolIdBech32 "\"pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt\""
Just pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt
-}
instance FromJSON GYStakePoolIdBech32 where
  parseJSON = Aeson.withText "GYStakePoolIdBech32" $ \t ->
    case stakePoolIdFromTextMaybe t of
      Just stakePoolId -> return $ GYStakePoolIdBech32 stakePoolId
      Nothing -> fail "cannot deserialise stake pool id"

instance Aeson.FromJSONKey GYStakePoolIdBech32 where
  fromJSONKey = Aeson.FromJSONKeyTextParser (either (fail . Text.unpack) pure . Web.parseUrlPiece)

{- |

>>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy GYStakePoolIdBech32))
"{\"description\":\"A stake pool id, serialised as Bech32.\",\"example\":\"pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt\",\"format\":\"bech32\",\"type\":\"string\"}"
-}
instance Swagger.ToSchema GYStakePoolIdBech32 where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYStakePoolIdBech32" $
        Swagger.paramSchemaToSchema (Proxy @GYStakePoolIdBech32)
          & Swagger.description
          ?~ "A stake pool id, serialised as Bech32."
            & Swagger.example
          ?~ toJSON ("pool1cjz6kg9a8ug9uk0nc59q60a67c2628ms58rd98gq587jwa2x5qt" :: GYStakePoolIdBech32)

instance Swagger.ToParamSchema GYStakePoolIdBech32 where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "bech32"
