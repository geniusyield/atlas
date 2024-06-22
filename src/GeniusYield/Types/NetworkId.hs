{-|
Module      : GeniusYield.Types.NetworkId
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.NetworkId
    ( GYNetworkId (..)
    , networkIdToApi
    , networkIdToLedger
    , networkIdToEpochSlots
    , networkIdToEra
    ) where

import qualified Cardano.Api              as Api
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Data.Aeson.Types         as Aeson
import qualified Data.Text                as T
import           Data.Word               (Word32, Word64)

import           GeniusYield.Types.Era

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8

data GYNetworkId
    = GYMainnet         -- ^ cardano mainnet
    | GYTestnetPreprod  -- ^ cardano preprod testnet
    | GYTestnetPreview  -- ^ cardano preview testnet
    | GYTestnetLegacy   -- ^ cardano legacy testnet
    | GYPrivnet
        { gyNetworkMagic :: Word32
        , gyNetworkEpochSlots :: Word64
        , gyNetworkEra :: GYEra
        }
      -- ^ local private network
    deriving (Show, Read, Eq, Ord)

networkIdToApi :: GYNetworkId -> Api.NetworkId
networkIdToApi GYMainnet                 = Api.Mainnet
networkIdToApi GYTestnetPreprod          = Api.Testnet $ Api.NetworkMagic 1
networkIdToApi GYTestnetPreview          = Api.Testnet $ Api.NetworkMagic 2
networkIdToApi GYTestnetLegacy           = Api.Testnet $ Api.NetworkMagic 1097911063
networkIdToApi GYPrivnet{gyNetworkMagic} = Api.Testnet $ Api.NetworkMagic gyNetworkMagic

networkIdToLedger :: GYNetworkId -> Ledger.Network
networkIdToLedger nid = case networkIdToApi nid of
    Api.Mainnet        -> Ledger.Mainnet
    Api.Testnet _magic -> Ledger.Testnet

networkIdToEpochSlots :: GYNetworkId -> Api.EpochSlots
networkIdToEpochSlots GYPrivnet{gyNetworkEpochSlots} = Api.EpochSlots gyNetworkEpochSlots
networkIdToEpochSlots GYMainnet                      = Api.EpochSlots 432000
networkIdToEpochSlots GYTestnetPreprod               = Api.EpochSlots 432000
networkIdToEpochSlots GYTestnetPreview               = Api.EpochSlots 86400
networkIdToEpochSlots GYTestnetLegacy                = Api.EpochSlots 432000

-- This needs to be updated whenever a hardfork happens.
networkIdToEra :: GYNetworkId -> GYEra
networkIdToEra GYPrivnet{gyNetworkEra} = gyNetworkEra
networkIdToEra GYMainnet               = GYBabbage
networkIdToEra GYTestnetPreprod        = GYBabbage
networkIdToEra GYTestnetPreview        = GYBabbage
networkIdToEra GYTestnetLegacy         = GYBabbage

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

-- |
--
-- >>> mapM_ LBS8.putStrLn $ Aeson.encode <$> [GYMainnet, GYTestnetPreprod, GYTestnetPreview, GYTestnetLegacy, GYPrivnet]
-- "mainnet"
-- "testnet-preprod"
-- "testnet-preview"
-- "testnet"
-- "privnet"
--
instance Aeson.ToJSON GYNetworkId where
    toJSON GYMainnet        = Aeson.toJSON ("mainnet" :: T.Text)
    toJSON GYTestnetPreprod = Aeson.toJSON ("testnet-preprod" :: T.Text)
    toJSON GYTestnetPreview = Aeson.toJSON ("testnet-preview" :: T.Text)
    toJSON GYTestnetLegacy  = Aeson.toJSON ("testnet" :: T.Text)
    toJSON GYPrivnet{}      = Aeson.toJSON ("privnet" :: T.Text)

    toEncoding GYMainnet        = Aeson.toEncoding ("mainnet" :: T.Text)
    toEncoding GYTestnetPreprod = Aeson.toEncoding ("testnet-preprod" :: T.Text)
    toEncoding GYTestnetPreview = Aeson.toEncoding ("testnet-preview" :: T.Text)
    toEncoding GYTestnetLegacy  = Aeson.toEncoding ("testnet" :: T.Text)
    toEncoding GYPrivnet{}      = Aeson.toEncoding ("privnet" :: T.Text)

-- |
--
-- >>> Aeson.eitherDecode @GYNetworkId <$> ["\"mainnet\"", "\"testnet-preprod\"", "\"preprod\"", "\"testnet-preview\"", "\"preview\"", "\"testnet\"", "\"privnet\"", "\"no-such-net\""]
-- [Right GYMainnet,Right GYTestnetPreprod,Right GYTestnetPreprod,Right GYTestnetPreview,Right GYTestnetPreview,Right GYTestnetLegacy,Right GYPrivnet,Left "Error in $: Expected mainnet, testnet-preprod, preprod, testnet-preview, preview, testnet or privnet"]
--
instance Aeson.FromJSON GYNetworkId where
    parseJSON "mainnet"         = pure GYMainnet
    parseJSON "testnet-preprod" = pure GYTestnetPreprod
    parseJSON "preprod"         = pure GYTestnetPreprod
    parseJSON "testnet-preview" = pure GYTestnetPreview
    parseJSON "preview"         = pure GYTestnetPreview
    parseJSON "testnet"         = pure GYTestnetLegacy
    parseJSON _                 = fail "Expected mainnet, testnet-preprod, preprod, testnet-preview, preview or testnet"
