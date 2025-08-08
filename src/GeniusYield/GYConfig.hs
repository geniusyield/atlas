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
  GYLayer1ProviderInfo (..),
  GYLayer2ProviderInfo (..),
  GYCoreProviderInfo (..),
  withCfgProviders,
  coreConfigIO,
  coreProviderIO,
  findMaestroTokenAndNetId,
  isNodeKupo,
  isOgmiosKupo,
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

import Control.Applicative ((<|>))
import Data.Sequence qualified as Seq
import GeniusYield.Providers.CacheLocal
import GeniusYield.Providers.CacheMempool (augmentQueryUTxOWithMempool)
import GeniusYield.Providers.Hydra
import GeniusYield.Providers.Hydra qualified as Hydra
import GeniusYield.Providers.Kupo qualified as KupoApi
import GeniusYield.Providers.Maestro qualified as MaestroApi
import GeniusYield.Providers.Node (nodeGetDRepState, nodeGetDRepsState, nodeStakeAddressInfo)
import GeniusYield.Providers.Node qualified as Node
import GeniusYield.Providers.Ogmios qualified as OgmiosApi
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

newtype MempoolCacheSettings = MempoolCacheSettings
  { mcsCacheInterval :: NominalDiffTime
  }
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''MempoolCacheSettings
 )

newtype LocalTxSubmissionCacheSettings = LocalTxSubmissionCacheSettings
  { lcsCacheInterval :: NominalDiffTime
  }
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''LocalTxSubmissionCacheSettings
 )

data GYLayer1ProviderInfo
  = GYNodeKupo {cpiSocketPath :: !FilePath, cpiKupoUrl :: !Text, cpiMempoolCache :: !(Maybe MempoolCacheSettings), cpiLocalTxSubmissionCache :: !(Maybe LocalTxSubmissionCacheSettings)}
  | GYOgmiosKupo {cpiOgmiosUrl :: !Text, cpiKupoUrl :: !Text, cpiMempoolCache :: !(Maybe MempoolCacheSettings), cpiLocalTxSubmissionCache :: !(Maybe LocalTxSubmissionCacheSettings)}
  | GYMaestro {cpiMaestroToken :: !(Confidential Text), cpiTurboSubmit :: !(Maybe Bool)}
  | GYBlockfrost {cpiBlockfrostKey :: !(Confidential Text)}
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYLayer1ProviderInfo
 )

data GYLayer2ProviderInfo
  = GYHydraNodeKupo {l2piHydraHeadNodeUrl :: !Text, l2piHydraKupoUrl :: !Text, l2piLayer1ProviderInfo :: !GYLayer1ProviderInfo}
  deriving stock Show

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 4 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYLayer2ProviderInfo
 )

-- TODO: Update this haddock? Or just share link to atlas docs.

{- |
The supported providers. The options are:

- Local node.socket along with Kupo
- Ogmios node instance along with Kupo
- Maestro blockchain API, provided its API token.
- Blockfrost API, provided its API key.

In JSON format, this essentially corresponds to:

= { socketPath: FilePath, kupoUrl: string, mempoolCache: { cacheInterval: number }, localTxSubmissionCache: { cacheInterval: number } }
| { ogmiosUrl: string, kupoUrl: string, mempoolCache: { cacheInterval: number }, localTxSubmissionCache: { cacheInterval: number } }
| { maestroToken: string, turboSubmit: boolean }
| { blockfrostKey: string }

The constructor tags don't need to appear in the JSON.
-}
data GYCoreProviderInfo
  = GYCoreLayer1ProviderInfo GYLayer1ProviderInfo
  | GYCoreLayer2ProviderInfo GYLayer2ProviderInfo
  deriving stock Show

instance FromJSON GYCoreProviderInfo where
  parseJSON v = (GYCoreLayer1ProviderInfo <$> parseJSON v) <|> (GYCoreLayer2ProviderInfo <$> parseJSON v)

coreProviderIO :: FilePath -> IO GYCoreProviderInfo
coreProviderIO = readJSON

isNodeKupo :: GYCoreProviderInfo -> Bool
isNodeKupo (GYCoreLayer1ProviderInfo GYNodeKupo {}) = True
isNodeKupo _ = False

isOgmiosKupo :: GYCoreProviderInfo -> Bool
isOgmiosKupo (GYCoreLayer1ProviderInfo GYOgmiosKupo {}) = True
isOgmiosKupo _ = False

isMaestro :: GYCoreProviderInfo -> Bool
isMaestro (GYCoreLayer1ProviderInfo GYMaestro {}) = True
isMaestro _ = False

isBlockfrost :: GYCoreProviderInfo -> Bool
isBlockfrost (GYCoreLayer1ProviderInfo GYBlockfrost {}) = True
isBlockfrost _ = False

findMaestroTokenAndNetId :: [GYCoreConfig] -> IO (Text, GYNetworkId)
findMaestroTokenAndNetId configs = do
  let config = find (isMaestro . cfgCoreProvider) configs
  case config of
    Nothing -> throwIO $ userError "Missing Maestro Configuration"
    Just conf -> do
      let netId = cfgNetworkId conf
      case cfgCoreProvider conf of
        GYCoreLayer1ProviderInfo (GYMaestro (Confidential token) _) -> return (token, netId)
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
      (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTx, gyAwaitTxConfirmed, gyGetStakeAddressInfo, gyGetDRepState, gyGetDRepsState, gyGetStakePools, gyGetConstitution, gyGetProposals, gyGetMempoolTxs) <- case cfgCoreProvider of
        GYCoreLayer1ProviderInfo l1ProviderInfo -> resolveLayer1ProviderInfo l1ProviderInfo
        GYCoreLayer2ProviderInfo (GYHydraNodeKupo headNodeUrl kupoUrl l1i) -> do
          (l1gyGetParameters, l1gySlotActions, _l1gyQueryUTxO, _l1gyLookupDatum, _l1gySubmitTx, _l1gyAwaitTxConfirmed, l1gyGetStakeAddressInfo, l1gyGetDRepState, l1gyGetDRepsState, l1gyGetStakePools, l1gyGetConstitution, l1gyGetProposals, l1gyGetMempoolTxs) <- resolveLayer1ProviderInfo l1i
          henv <- newHydraApiEnv $ Text.unpack headNodeUrl
          kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl
          let queryUtxo = KupoApi.kupoQueryUtxo kEnv
          pure
            ( l1gyGetParameters {gyGetProtocolParameters' = Hydra.hydraProtocolParameters henv} -- Hack for now.
            , l1gySlotActions
            , queryUtxo
            , KupoApi.kupoLookupDatum kEnv
            , Hydra.hydraSubmitTx henv
            , KupoApi.kupoAwaitTxConfirmed kEnv
            , l1gyGetStakeAddressInfo
            , l1gyGetDRepState
            , l1gyGetDRepsState
            , l1gyGetStakePools
            , l1gyGetConstitution
            , l1gyGetProposals
            , l1gyGetMempoolTxs
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
   where
    resolveLayer1ProviderInfo cfgCoreL1Provider = do
      case cfgCoreL1Provider of
        (GYNodeKupo path kupoUrl mmempoolCache mlocalTxSubCache) -> do
          let info = nodeConnectInfo path cfgNetworkId
          kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl
          nodeSlotActions <- makeSlotActions slotCachingTime $ Node.nodeGetSlotOfCurrentBlock info
          nodeGetParams <- Node.nodeGetParameters info
          queryUtxo <- case mmempoolCache of
            Nothing -> pure $ KupoApi.kupoQueryUtxo kEnv
            Just (MempoolCacheSettings cacheInterval) -> do
              augmentQueryUTxOWithMempool (KupoApi.kupoQueryUtxo kEnv) (Node.nodeMempoolTxs info) cacheInterval
          (queryUtxo', submitTx) <- case mlocalTxSubCache of
            Nothing -> pure (queryUtxo, Node.nodeSubmitTx info)
            Just (LocalTxSubmissionCacheSettings cacheInterval) -> do
              locallySubmittedTxsVar <- mkLocallySubmittedTxsVar cacheInterval
              let augmentedSubmitTx = augmentTxSubmission (Node.nodeSubmitTx info) locallySubmittedTxsVar
              pure (augmentQueryUTxOWithLocalSubmission queryUtxo locallySubmittedTxsVar, augmentedSubmitTx)
          pure
            ( nodeGetParams
            , nodeSlotActions
            , queryUtxo'
            , KupoApi.kupoLookupDatum kEnv
            , submitTx
            , KupoApi.kupoAwaitTxConfirmed kEnv
            , nodeStakeAddressInfo info
            , nodeGetDRepState info
            , nodeGetDRepsState info
            , Node.nodeStakePools info
            , Node.nodeConstitution info
            , Node.nodeProposals info
            , Node.nodeMempoolTxs info
            )
        (GYOgmiosKupo ogmiosUrl kupoUrl mmempoolCache mlocalTxSubCache) -> do
          oEnv <- OgmiosApi.newOgmiosApiEnv $ Text.unpack ogmiosUrl
          kEnv <- KupoApi.newKupoApiEnv $ Text.unpack kupoUrl
          ogmiosSlotActions <- makeSlotActions slotCachingTime $ OgmiosApi.ogmiosGetSlotOfCurrentBlock oEnv
          ogmiosGetParams <-
            makeGetParameters
              (OgmiosApi.ogmiosProtocolParameters oEnv)
              (OgmiosApi.ogmiosStartTime oEnv)
              (OgmiosApi.ogmiosEraSummaries oEnv)
              (OgmiosApi.ogmiosGetSlotOfCurrentBlock oEnv)
          queryUtxo <- case mmempoolCache of
            Nothing -> pure $ KupoApi.kupoQueryUtxo kEnv
            Just (MempoolCacheSettings cacheInterval) -> do
              augmentQueryUTxOWithMempool (KupoApi.kupoQueryUtxo kEnv) (OgmiosApi.ogmiosMempoolTxsWs oEnv) cacheInterval
          (queryUtxo', submitTx) <- case mlocalTxSubCache of
            Nothing -> pure (queryUtxo, OgmiosApi.ogmiosSubmitTx oEnv)
            Just (LocalTxSubmissionCacheSettings cacheInterval) -> do
              locallySubmittedTxsVar <- mkLocallySubmittedTxsVar cacheInterval
              let augmentedSubmitTx = augmentTxSubmission (OgmiosApi.ogmiosSubmitTx oEnv) locallySubmittedTxsVar
              pure (augmentQueryUTxOWithLocalSubmission queryUtxo locallySubmittedTxsVar, augmentedSubmitTx)
          pure
            ( ogmiosGetParams
            , ogmiosSlotActions
            , queryUtxo'
            , KupoApi.kupoLookupDatum kEnv
            , submitTx
            , KupoApi.kupoAwaitTxConfirmed kEnv
            , OgmiosApi.ogmiosStakeAddressInfo oEnv
            , OgmiosApi.ogmiosGetDRepState oEnv
            , OgmiosApi.ogmiosGetDRepsState oEnv
            , OgmiosApi.ogmiosStakePools oEnv
            , OgmiosApi.ogmiosConstitution oEnv
            , OgmiosApi.ogmiosProposals oEnv
            , OgmiosApi.ogmiosMempoolTxsWs oEnv
            )
        (GYMaestro (Confidential apiToken) turboSubmit) -> do
          maestroApiEnv <- MaestroApi.networkIdToMaestroEnv apiToken cfgNetworkId
          maestroSlotActions <- makeSlotActions slotCachingTime $ MaestroApi.maestroGetSlotOfCurrentBlock maestroApiEnv
          maestroGetParams <-
            makeGetParameters
              (MaestroApi.maestroProtocolParams maestroApiEnv)
              (MaestroApi.maestroSystemStart maestroApiEnv)
              (MaestroApi.maestroEraHistory maestroApiEnv)
              (MaestroApi.maestroGetSlotOfCurrentBlock maestroApiEnv)
          pure
            ( maestroGetParams
            , maestroSlotActions
            , MaestroApi.maestroQueryUtxo maestroApiEnv
            , MaestroApi.maestroLookupDatum maestroApiEnv
            , MaestroApi.maestroSubmitTx (Just True == turboSubmit) maestroApiEnv
            , MaestroApi.maestroAwaitTxConfirmed maestroApiEnv
            , MaestroApi.maestroStakeAddressInfo maestroApiEnv
            , MaestroApi.maestroDRepState maestroApiEnv
            , MaestroApi.maestroDRepsState maestroApiEnv
            , MaestroApi.maestroStakePools maestroApiEnv
            , MaestroApi.maestroConstitution maestroApiEnv
            , MaestroApi.maestroProposals maestroApiEnv
            , MaestroApi.maestroMempoolTxs maestroApiEnv
            )
        (GYBlockfrost (Confidential key)) -> do
          let proj = Blockfrost.networkIdToProject cfgNetworkId key
          blockfrostSlotActions <- makeSlotActions slotCachingTime $ Blockfrost.blockfrostGetSlotOfCurrentBlock proj
          blockfrostGetParams <-
            makeGetParameters
              (Blockfrost.blockfrostProtocolParams proj)
              (Blockfrost.blockfrostSystemStart proj)
              (Blockfrost.blockfrostEraHistory proj)
              (Blockfrost.blockfrostGetSlotOfCurrentBlock proj)
          pure
            ( blockfrostGetParams
            , blockfrostSlotActions
            , Blockfrost.blockfrostQueryUtxo proj
            , Blockfrost.blockfrostLookupDatum proj
            , Blockfrost.blockfrostSubmitTx proj
            , Blockfrost.blockfrostAwaitTxConfirmed proj
            , Blockfrost.blockfrostStakeAddressInfo proj
            , Blockfrost.blockfrostDRepState proj
            , Blockfrost.blockfrostDRepsState proj
            , Blockfrost.blockfrostStakePools proj
            , Blockfrost.blockfrostConstitution proj
            , Blockfrost.blockfrostProposals proj
            , Blockfrost.blockfrostMempoolTxs proj
            )

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
    , gyGetDRepState = gyGetDRepState'
    , gyGetDRepsState = gyGetDRepsState'
    , gyLog' = gyLog'
    , gyGetStakePools = gyGetStakePools'
    , gyGetConstitution = gyGetConstitution'
    , gyGetProposals = gyGetProposals'
    , gyGetMempoolTxs = gyGetMempoolTxs'
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
      , gyGetSlotConfig' = wrap "gyGetSlotConfig" $ gyGetSlotConfig providers
      }

  gyGetStakePools' = wrap "gyGetStakePools" gyGetStakePools

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
      , gyQueryUtxosWithAsset' = wrap "gyQueryUtxosWithAsset'" . gyQueryUtxosWithAsset providers
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

  gyGetDRepState' :: GYCredential 'GYKeyRoleDRep -> IO (Maybe GYDRepState)
  gyGetDRepState' = wrap "gyGetDRepState" . gyGetDRepState

  gyGetDRepsState' :: Set (GYCredential 'GYKeyRoleDRep) -> IO (Map (GYCredential 'GYKeyRoleDRep) (Maybe GYDRepState))
  gyGetDRepsState' = wrap "gyGetDRepsState" . gyGetDRepsState

  gyGetConstitution' :: IO GYConstitution
  gyGetConstitution' = wrap "gyGetConstitution" gyGetConstitution

  gyGetProposals' :: Set GYGovActionId -> IO (Seq.Seq GYGovActionState)
  gyGetProposals' = wrap "gyGetProposals" . gyGetProposals

  gyGetMempoolTxs' = wrap "gyGetMempoolTxs" gyGetMempoolTxs

duration :: IO a -> IO (a, NominalDiffTime)
duration m = do
  start <- getCurrentTime
  a <- m
  end <- getCurrentTime
  pure (a, end `diffUTCTime` start)
