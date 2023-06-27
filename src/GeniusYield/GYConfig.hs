{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.GYConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.GYConfig
    ( GYCoreConfig (..)
    , GYCoreProviderInfo (..)
    , withCfgProviders
    , coreConfigIO
    , coreProviderIO
    , findMaestroTokenAndNetId
    , isNodeChainIx
    , isMaestro
    , isBlockfrost
    , isDbSync
    ) where

import           Control.Exception                      (SomeException, bracket,
                                                         try)
import qualified Data.Aeson                             as Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy                   as LBS
import           Data.Char                              (toLower)
import qualified Data.Text                              as T
import           Data.Time                              (NominalDiffTime)
import qualified Database.PostgreSQL.Simple             as PQ
import qualified Database.PostgreSQL.Simple.URL         as PQ

import qualified Cardano.Api                            as Api

import           GeniusYield.Imports
import qualified GeniusYield.Providers.Blockfrost       as Blockfrost
import qualified GeniusYield.Providers.CachedQueryUTxOs as CachedQuery
import qualified GeniusYield.Providers.CardanoDbSync    as DbSync
import qualified GeniusYield.Providers.Katip            as Katip
import qualified GeniusYield.Providers.Maestro          as MaestroApi
import qualified GeniusYield.Providers.Node             as Node
import qualified GeniusYield.Providers.SubmitApi        as SubmitApi
import           GeniusYield.Types

-- | How many seconds to keep slots cached, before refetching the data.
slotCachingTime :: NominalDiffTime
slotCachingTime = 5

-- | Newtype with a custom show instance that prevents showing the contained data.
newtype Confidential a = Confidential a
  deriving newtype (Eq, Ord, FromJSON)

instance Show (Confidential a) where
  showsPrec _ _ = showString "<Confidential>"

{- |
The supported providers. The options are:

- Local node.socket and a maestro API key
- Cardano db sync alongside cardano submit api
- Maestro blockchain API, provided its API token.

In JSON format, this essentially corresponds to:

= { socketPath: FilePath, maestroToken: string }
| { cardanoDbSync: PQ.ConnectInfo, cardanoSubmitApiUrl: string }
| { maestroToken: string }
| { blockfrostKey: string }

The constructor tags don't need to appear in the JSON.
-}
data GYCoreProviderInfo
  = GYNodeChainIx {cpiSocketPath :: !FilePath, cpiMaestroToken :: !(Confidential Text)}
  | GYDbSync {cpiCardanoDbSync :: !PQConnInf, cpiCardanoSubmitApiUrl :: !String}
  | GYMaestro {cpiMaestroToken :: !(Confidential Text)}
  | GYBlockfrost {cpiBlockfrostKey :: !(Confidential Text)}
  deriving stock (Show)

coreProviderIO :: FilePath -> IO GYCoreProviderInfo
coreProviderIO filePath = do
  bs <- LBS.readFile filePath
  case Aeson.eitherDecode' bs of
    Left err  -> throwIO $ userError err
    Right cfg -> pure cfg

isNodeChainIx :: GYCoreProviderInfo -> Bool
isNodeChainIx GYNodeChainIx{} = True
isNodeChainIx _               = False

isDbSync :: GYCoreProviderInfo -> Bool
isDbSync GYDbSync{} = True
isDbSync _          = False

isMaestro :: GYCoreProviderInfo -> Bool
isMaestro GYMaestro{} = True
isMaestro _           = False

isBlockfrost :: GYCoreProviderInfo -> Bool
isBlockfrost GYBlockfrost{} = True
isBlockfrost _              = False

findMaestroTokenAndNetId :: [GYCoreConfig] -> IO (Text, GYNetworkId)
findMaestroTokenAndNetId configs = do
    let config = find (isMaestro . cfgCoreProvider) configs
    case config of
        Nothing -> throwIO $ userError "Missing Maestro Configuration"
        Just conf -> do
            let netId = cfgNetworkId conf
            case cfgCoreProvider conf of
              GYMaestro (Confidential token) -> return (token, netId)
              _ -> throwIO $ userError "Missing Maestro Token"

{- |
The config to initialize the GY framework with.
Should include information on the providers to use, as well as the network id.

In JSON format, this essentially corresponds to:

= { coreProvider: GYCoreProviderInfo, networkId: NetworkId, logging: [GYLogScribeConfig], utxoCacheEnable: boolean }
-}
data GYCoreConfig = GYCoreConfig
  { cfgCoreProvider    :: !GYCoreProviderInfo
  , cfgNetworkId       :: !GYNetworkId
  , cfgLogging         :: ![GYLogScribeConfig]
  , cfgUtxoCacheEnable :: !Bool
  }
  deriving stock (Show)

coreConfigIO :: FilePath -> IO GYCoreConfig
coreConfigIO file = do
  bs <- LBS.readFile file
  case Aeson.eitherDecode' bs of
    Left err  -> throwIO $ userError err
    Right cfg -> pure cfg

nodeConnectInfo :: FilePath -> GYNetworkId -> Api.LocalNodeConnectInfo Api.CardanoMode
nodeConnectInfo path netId = Node.networkIdToLocalNodeConnectInfo netId path

withCfgProviders :: GYCoreConfig  -> GYLogNamespace -> (GYProviders -> IO a) -> IO a
withCfgProviders
  GYCoreConfig
    { cfgCoreProvider
    , cfgNetworkId
    , cfgLogging
    , cfgUtxoCacheEnable
    }
    ns
    f =
    do
      (gyGetParameters, gySlotActions', gyQueryUTxO', gyLookupDatum, gySubmitTx) <- case cfgCoreProvider of
        GYNodeChainIx path (Confidential key) -> do
          let info = nodeConnectInfo path cfgNetworkId
              era = networkIdToEra cfgNetworkId
          mEnv <- MaestroApi.networkIdToMaestroEnv key cfgNetworkId
          nodeSlotActions <- makeSlotActions slotCachingTime $ Node.nodeGetCurrentSlot info
          pure
            ( Node.nodeGetParameters era info
            , nodeSlotActions
            , Node.nodeQueryUTxO era info
            , MaestroApi.maestroLookupDatum  mEnv
            , Node.nodeSubmitTx info
            )
        GYDbSync (PQConnInf ci) submitUrl -> do
          -- NOTE: This provider generally does not support anything other than private testnets.
          conn              <- DbSync.openDbSyncConn ci
          submitApiEnv      <- SubmitApi.newSubmitApiEnv submitUrl
          dbSyncSlotActions <- makeSlotActions slotCachingTime $ DbSync.dbSyncSlotNumber conn
          pure
            ( DbSync.dbSyncGetParameters conn
            , dbSyncSlotActions
            , DbSync.dbSyncQueryUtxo conn
            , DbSync.dbSyncLookupDatum conn
            , SubmitApi.submitApiSubmitTxDefault submitApiEnv
            )
        GYMaestro (Confidential apiToken) -> do
          maestroApiEnv <- MaestroApi.networkIdToMaestroEnv apiToken cfgNetworkId
          maestroGetParams <- makeGetParameters
            (MaestroApi.maestroGetCurrentSlot maestroApiEnv)
            (MaestroApi.maestroProtocolParams maestroApiEnv)
            (MaestroApi.maestroSystemStart maestroApiEnv)
            (MaestroApi.maestroEraHistory maestroApiEnv)
            (MaestroApi.maestroStakePools maestroApiEnv)
          maestroSlotActions <- makeSlotActions slotCachingTime $ MaestroApi.maestroGetCurrentSlot maestroApiEnv
          pure
            ( maestroGetParams
            , maestroSlotActions
            , MaestroApi.maestroQueryUtxo maestroApiEnv
            , MaestroApi.maestroLookupDatum maestroApiEnv
            , MaestroApi.maestroSubmitTx maestroApiEnv
            )
        GYBlockfrost (Confidential key) -> do
          let proj = Blockfrost.networkIdToProject cfgNetworkId key
          blockfrostGetParams <- makeGetParameters
            (Blockfrost.blockfrostGetCurrentSlot proj)
            (Blockfrost.blockfrostProtocolParams proj)
            (Blockfrost.blockfrostSystemStart proj)
            (Blockfrost.blockfrostEraHistory proj)
            (Blockfrost.blockfrostStakePools proj)
          blockfrostSlotActions <- makeSlotActions slotCachingTime $ Blockfrost.blockfrostGetCurrentSlot proj
          pure
            ( blockfrostGetParams
            , blockfrostSlotActions
            , Blockfrost.blockfrostQueryUtxo proj
            , Blockfrost.blockfrostLookupDatum proj
            , Blockfrost.blockfrostSubmitTx proj
            )

      bracket (Katip.mkKatipLog ns cfgLogging) logCleanUp $ \gyLog' -> do
        (gyQueryUTxO, gySlotActions) <-
            if cfgUtxoCacheEnable
            then do
                (gyQueryUTxO, purgeCache) <- CachedQuery.makeCachedQueryUTxO gyQueryUTxO' gyLog'
                -- waiting for the next block will purge the utxo cache.
                let gySlotActions = gySlotActions' { gyWaitForNextBlock' = purgeCache >> gyWaitForNextBlock' gySlotActions'}
                pure (gyQueryUTxO, gySlotActions)
            else pure (gyQueryUTxO', gySlotActions')
        e <- try $ f GYProviders {..}
        case e of
            Right a                     -> pure a
            Left (err :: SomeException) -> do
                logRun gyLog' mempty GYError $ printf "ERROR: %s" $ show err
                throwIO err

newtype PQConnInf = PQConnInf PQ.ConnectInfo
  deriving newtype (Show)

instance FromJSON PQConnInf where
  parseJSON = Aeson.withText "ConnectInfo URL" $ \t ->
    case PQConnInf <$> PQ.parseDatabaseUrl (T.unpack t) of
      Nothing -> fail "Invalid PostgreSQL URL"
      Just ci -> pure ci

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      , sumEncoding = UntaggedValue
      }
    ''GYCoreProviderInfo
 )

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \fldName -> case drop 3 fldName of x : xs -> toLower x : xs; [] -> []
      }
    ''GYCoreConfig
 )
