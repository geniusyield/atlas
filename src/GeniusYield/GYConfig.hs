{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.GYConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.GYConfig (
  GYCoreConfig (..),
  Confidential (..),
  GYCoreProviderInfo (..),
  withCfgProviders,
  coreConfigIO,
  coreProviderIO,
  findMaestroTokenAndNetId,
  isNodeKupo,
  isMaestro,
  isBlockfrost,
) where

import Control.Exception (SomeException, bracket, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower)
import Data.Text qualified as Text
import Data.Time (
  NominalDiffTime,
  diffUTCTime,
  getCurrentTime,
 )

import Cardano.Api qualified as Api

import GeniusYield.Imports
import GeniusYield.Providers.Blockfrost qualified as Blockfrost

-- import qualified GeniusYield.Providers.CachedQueryUTxOs as CachedQuery
import GeniusYield.Providers.Kupo qualified as KupoApi
import GeniusYield.Providers.Maestro qualified as MaestroApi
import GeniusYield.Providers.Node (nodeStakeAddressInfo)
import GeniusYield.Providers.Node qualified as Node
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types

-- | How many seconds to keep slots cached, before refetching the data.
slotCachingTime :: NominalDiffTime
slotCachingTime = 5

-- | Newtype with a custom show instance that prevents showing the contained data.
newtype Confidential a = Confidential a
  deriving newtype (Eq, Ord, FromJSON, ToJSON)

instance Show (Confidential a) where
  showsPrec _ _ = showString "<Confidential>"

{- |
The supported providers. The options are:

- Local node.socket and a maestro API key
- Cardano db sync alongside cardano submit api
- Maestro blockchain API, provided its API token.

In JSON format, this essentially corresponds to:

= { socketPath: FilePath, kupoUrl: string }
| { maestroToken: string, turboSubmit: boolean }
| { blockfrostKey: string }

The constructor tags don't need to appear in the JSON.
-}
data GYCoreProviderInfo
  = GYNodeKupo {cpiSocketPath :: !FilePath, cpiKupoUrl :: !Text}
  | GYMaestro {cpiMaestroToken :: !(Confidential Text), cpiTurboSubmit :: !(Maybe Bool)}
  | GYBlockfrost {cpiBlockfrostKey :: !(Confidential Text)}
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYCoreProviderInfo
 )

coreProviderIO :: FilePath -> IO GYCoreProviderInfo
coreProviderIO = readJSON

isNodeKupo :: GYCoreProviderInfo -> Bool
isNodeKupo GYNodeKupo {} = True
isNodeKupo _ = False

isMaestro :: GYCoreProviderInfo -> Bool
isMaestro GYMaestro {} = True
isMaestro _ = False

isBlockfrost :: GYCoreProviderInfo -> Bool
isBlockfrost GYBlockfrost {} = True
isBlockfrost _ = False

findMaestroTokenAndNetId :: [GYCoreConfig] -> IO (Text, GYNetworkId)
findMaestroTokenAndNetId configs = do
  let config = find (isMaestro . cfgCoreProvider) configs
  case config of
    Nothing -> throwIO $ userError "Missing Maestro Configuration"
    Just conf -> do
      let netId = cfgNetworkId conf
      case cfgCoreProvider conf of
        GYMaestro (Confidential token) _ -> return (token, netId)
        _ -> throwIO $ userError "Missing Maestro Token"

{- |
The config to initialize the GY framework with.
Should include information on the providers to use, as well as the network id.

In JSON format, this essentially corresponds to:

= { coreProvider: GYCoreProviderInfo, networkId: NetworkId, logging: [GYLogScribeConfig], utxoCacheEnable: boolean }
-}
data GYCoreConfig = GYCoreConfig
  { cfgCoreProvider :: !GYCoreProviderInfo
  , cfgNetworkId :: !GYNetworkId
  , cfgLogging :: ![GYLogScribeConfig]
  -- ^ List of scribes to register.
  , cfgLogTiming :: !(Maybe Bool)
  -- ^ Optional switch to enable timing and logging of requests sent to provider.
  }
  -- , cfgUtxoCacheEnable :: !Bool

  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      }
    ''GYCoreConfig
 )

coreConfigIO :: FilePath -> IO GYCoreConfig
coreConfigIO file = do
  bs <- LBS.readFile file
  case Aeson.eitherDecode' bs of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg

nodeConnectInfo :: FilePath -> GYNetworkId -> Api.LocalNodeConnectInfo
nodeConnectInfo path netId = Node.networkIdToLocalNodeConnectInfo netId path

withCfgProviders :: GYCoreConfig -> GYLogNamespace -> (GYProviders -> IO a) -> IO a
withCfgProviders
  GYCoreConfig
    { cfgCoreProvider
    , cfgNetworkId
    , cfgLogging
    , cfgLogTiming
    }
  ns
  f =
    do
      (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTx, gyAwaitTxConfirmed, gyGetStakeAddressInfo) <- case cfgCoreProvider of
        GYNodeKupo path kupoUrl -> do
          let info = nodeConnectInfo path cfgNetworkId
          kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl
          nodeSlotActions <- makeSlotActions slotCachingTime $ Node.nodeGetSlotOfCurrentBlock info
          nodeGetParams <- Node.nodeGetParameters info
          pure
            ( nodeGetParams
            , nodeSlotActions
            , KupoApi.kupoQueryUtxo kEnv
            , KupoApi.kupoLookupDatum kEnv
            , Node.nodeSubmitTx info
            , KupoApi.kupoAwaitTxConfirmed kEnv
            , nodeStakeAddressInfo info
            )
        GYMaestro (Confidential apiToken) turboSubmit -> do
          maestroApiEnv <- MaestroApi.networkIdToMaestroEnv apiToken cfgNetworkId
          maestroSlotActions <- makeSlotActions slotCachingTime $ MaestroApi.maestroGetSlotOfCurrentBlock maestroApiEnv
          maestroGetParams <-
            makeGetParameters
              (MaestroApi.maestroProtocolParams maestroApiEnv)
              (MaestroApi.maestroSystemStart maestroApiEnv)
              (MaestroApi.maestroEraHistory maestroApiEnv)
              (MaestroApi.maestroStakePools maestroApiEnv)
          pure
            ( maestroGetParams
            , maestroSlotActions
            , MaestroApi.maestroQueryUtxo maestroApiEnv
            , MaestroApi.maestroLookupDatum maestroApiEnv
            , MaestroApi.maestroSubmitTx (Just True == turboSubmit) maestroApiEnv
            , MaestroApi.maestroAwaitTxConfirmed maestroApiEnv
            , MaestroApi.maestroStakeAddressInfo maestroApiEnv
            )
        GYBlockfrost (Confidential key) -> do
          let proj = Blockfrost.networkIdToProject cfgNetworkId key
          blockfrostSlotActions <- makeSlotActions slotCachingTime $ Blockfrost.blockfrostGetSlotOfCurrentBlock proj
          blockfrostGetParams <-
            makeGetParameters
              (Blockfrost.blockfrostProtocolParams cfgNetworkId proj)
              (Blockfrost.blockfrostSystemStart proj)
              (Blockfrost.blockfrostEraHistory proj)
              (Blockfrost.blockfrostStakePools proj)
          pure
            ( blockfrostGetParams
            , blockfrostSlotActions
            , Blockfrost.blockfrostQueryUtxo proj
            , Blockfrost.blockfrostLookupDatum proj
            , Blockfrost.blockfrostSubmitTx proj
            , Blockfrost.blockfrostAwaitTxConfirmed proj
            , Blockfrost.blockfrostStakeAddressInfo proj
            )

      bracket (mkLogEnv ns cfgLogging) closeScribes $ \logEnv -> do
        let gyLog' =
              GYLogConfiguration
                { cfgLogNamespace = mempty
                , cfgLogContexts = mempty
                , cfgLogDirector = Left logEnv
                }
        (gyQueryUTxO, gySlotActions) <-
          {-if cfgUtxoCacheEnable
          then do
              (gyQueryUTxO, purgeCache) <- CachedQuery.makeCachedQueryUTxO gyQueryUTxO' gyLog'
              -- waiting for the next block will purge the utxo cache.
              let gySlotActions = gySlotActions' { gyWaitForNextBlock' = purgeCache >> gyWaitForNextBlock' gySlotActions'}
              pure (gyQueryUTxO, gySlotActions, f)
          else -} pure (gyQueryUTxO', gySlotActions')
        let f' =
              maybe
                f
                ( \case
                    True -> f . logTiming
                    False -> f
                )
                cfgLogTiming
        e <- try $ f' GYProviders {..}
        case e of
          Right a -> pure a
          Left (err :: SomeException) -> do
            logRun gyLog' GYError ((printf "ERROR: %s" $ show err) :: String)
            throwIO err

logTiming :: GYProviders -> GYProviders
logTiming providers@GYProviders {..} =
  GYProviders
    { gyLookupDatum = gyLookupDatum'
    , gySubmitTx = gySubmitTx'
    , gyAwaitTxConfirmed = gyAwaitTxConfirmed'
    , gySlotActions = gySlotActions'
    , gyGetParameters = gyGetParameters'
    , gyQueryUTxO = gyQueryUTxO'
    , gyGetStakeAddressInfo = gyGetStakeAddressInfo'
    , gyLog' = gyLog'
    }
 where
  wrap :: String -> IO a -> IO a
  wrap msg m = do
    (!a, !t) <- duration m
    gyLog providers "" GYDebug $ msg <> " took " <> show t
    pure a

  gyLookupDatum' :: GYLookupDatum
  gyLookupDatum' = wrap "gyLookupDatum" . gyLookupDatum

  gySubmitTx' :: GYSubmitTx
  gySubmitTx' = wrap "gySubmitTx" . gySubmitTx

  gyAwaitTxConfirmed' :: GYAwaitTx
  gyAwaitTxConfirmed' p = wrap "gyAwaitTxConfirmed" . gyAwaitTxConfirmed p

  gySlotActions' :: GYSlotActions
  gySlotActions' =
    GYSlotActions
      { gyGetSlotOfCurrentBlock' = wrap "gyGetSlotOfCurrentBlock" $ gyGetSlotOfCurrentBlock providers
      , gyWaitForNextBlock' = wrap "gyWaitForNextBlock" $ gyWaitForNextBlock providers
      , gyWaitUntilSlot' = wrap "gyWaitUntilSlot" . gyWaitUntilSlot providers
      }

  gyGetParameters' :: GYGetParameters
  gyGetParameters' =
    GYGetParameters
      { gyGetProtocolParameters' = wrap "gyGetProtocolParameters" $ gyGetProtocolParameters providers
      , gyGetSystemStart' = wrap "gyGetSystemStart" $ gyGetSystemStart providers
      , gyGetEraHistory' = wrap "gyGetEraHistory" $ gyGetEraHistory providers
      , gyGetStakePools' = wrap "gyGetStakePools" $ gyGetStakePools providers
      , gyGetSlotConfig' = wrap "gyGetSlotConfig" $ gyGetSlotConfig providers
      }

  gyQueryUTxO' :: GYQueryUTxO
  gyQueryUTxO' =
    GYQueryUTxO
      { gyQueryUtxosAtTxOutRefs' = wrap "gyQueryUtxosAtTxOutRefs" . gyQueryUtxosAtTxOutRefs providers
      , gyQueryUtxosAtTxOutRefsWithDatums' = case gyQueryUtxosAtTxOutRefsWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtTxOutRefsWithDatums" . q
      , gyQueryUtxoAtTxOutRef' = wrap "gyQueryUtxoAtTxOutRef" . gyQueryUtxoAtTxOutRef providers
      , gyQueryUtxoRefsAtAddress' = wrap "gyQueryUtxoRefsAtAddress" . gyQueryUtxoRefsAtAddress providers
      , gyQueryUtxosAtAddress' = \addr mac -> wrap "gyQueryUtxosAtAddress'" $ gyQueryUtxosAtAddress providers addr mac
      , gyQueryUtxosAtAddressWithDatums' = case gyQueryUtxosAtAddressWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ \addr mac -> wrap "gyQueryUtxosAtAddressWithDatums'" $ q addr mac
      , gyQueryUtxosAtAddresses' = wrap "gyQueryUtxosAtAddresses" . gyQueryUtxosAtAddresses providers
      , gyQueryUtxosAtAddressesWithDatums' = case gyQueryUtxosAtAddressesWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtAddressesWithDatums" . q
      , gyQueryUtxosAtPaymentCredential' = \cred -> wrap "gyQueryUtxosAtPaymentCredential" . gyQueryUtxosAtPaymentCredential providers cred
      , gyQueryUtxosAtPaymentCredWithDatums' = case gyQueryUtxosAtPaymentCredWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ \cred mac -> wrap "gyQueryUtxosAtPaymentCredWithDatums" $ q cred mac
      , gyQueryUtxosAtPaymentCredentials' = wrap "gyQueryUtxosAtPaymentCredentials" . gyQueryUtxosAtPaymentCredentials providers
      , gyQueryUtxosAtPaymentCredsWithDatums' = case gyQueryUtxosAtPaymentCredsWithDatums' gyQueryUTxO of
          Nothing -> Nothing
          Just q -> Just $ wrap "gyQueryUtxosAtPaymentCredsWithDatums" . q
      }

  gyGetStakeAddressInfo' :: GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
  gyGetStakeAddressInfo' = wrap "gyGetStakeAddressInfo" . gyGetStakeAddressInfo

duration :: IO a -> IO (a, NominalDiffTime)
duration m = do
  start <- getCurrentTime
  a <- m
  end <- getCurrentTime
  pure (a, end `diffUTCTime` start)
