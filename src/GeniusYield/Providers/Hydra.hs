module GeniusYield.Providers.Hydra (
  HydraApiEnv,
  newHydraApiEnv,
  HydraProviderException (..),
  hydraSubmitTx,
  hydraProtocolParameters,
) where

import Cardano.Api qualified as Api
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import GeniusYield.Imports (Exception, Generic, throwIO, (&))
import GeniusYield.Providers.Common (
  newManager,
  newServantClientEnv,
 )
import GeniusYield.Types
import Network.HTTP.Client qualified as HttpClient
import Network.WebSockets qualified as WS
import Servant.API (
  Get,
  JSON,
  OctetStream,
  Post,
  ReqBody,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  BaseUrl (..),
  ClientEnv,
  ClientError,
  ClientM,
  baseUrl,
  client,
  runClientM,
  showBaseUrl,
 )

newtype HydraApiEnv = HydraApiEnv {clientEnv :: ClientEnv}

{- | Returns a new 'HydraApiEnv' given the base url to query from.

>>> env <- newHydraApiEnv "http://localhost:4002"
-}
newHydraApiEnv :: String -> IO HydraApiEnv
newHydraApiEnv baseUrl = HydraApiEnv <$> newServantClientEnv baseUrl

-- | Exceptions.
data HydraProviderException
  = -- | Error from the Hydra API.
    HydraApiError !Text !(Either ClientError Text)
  | -- | Unable to decode response given by Hydra under Websocket connection.
    HydraWebsocketDecodeError
      -- | Received response.
      !Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

{-# INLINEABLE runHydraClient #-}
runHydraClient :: HydraApiEnv -> ClientM a -> IO (Either ClientError a)
runHydraClient (HydraApiEnv cEnv) c = runClientM c cEnv

{-# INLINEABLE handleHydraError #-}
handleHydraError :: Text -> Either ClientError a -> IO a
handleHydraError locationInfo =
  either
    (throwIO . HydraApiError locationInfo . Left)
    pure

processWSResponse :: Aeson.FromJSON a => Text -> IO a
processWSResponse msg =
  case Aeson.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 msg) of
    Left err -> throwIO . HydraWebsocketDecodeError $ "error: " <> Text.pack err <> " while processing message: " <> msg
    Right a -> pure a

type HydraApi = "protocol-parameters" :> Get '[JSON] ApiProtocolParameters

protocolParams :: ClientM ApiProtocolParameters
protocolParams = client @HydraApi Proxy

hydraProtocolParameters :: HydraApiEnv -> IO ApiProtocolParameters
hydraProtocolParameters env = do
  -- Due to a big, following has been commented out and we have a separate logic. Bug: https://github.com/cardano-scaling/hydra/issues/2094. Once it is fixed, `HydraApiError` should also just be returning for `ClientError`.
  -- runHydraClient env protocolParams >>= handleHydraError "hydraProtocolParameters"
  manager <- newManager (baseUrl (clientEnv env))
  request <- HttpClient.parseRequest $ showBaseUrl (baseUrl (clientEnv env)) ++ "/protocol-parameters"
  response <- HttpClient.httpLbs request manager
  case Aeson.eitherDecode $ HttpClient.responseBody response of
    Left err -> throwIO $ HydraApiError "hydraProtocolParameters" $ Right $ Text.pack err
    Right params -> pure params

newtype HydraTransactionId = HydraTransactionId {transactionId :: GYTxId}
  deriving newtype Show
  deriving stock Generic
  deriving anyclass Aeson.FromJSON

hydraSubmitTx :: HydraApiEnv -> GYSubmitTx
hydraSubmitTx (baseUrl . clientEnv -> BaseUrl {..}) tx = do
  let newTxJson =
        Aeson.object
          [ "tag" Aeson..= ("NewTx" :: Text)
          , "transaction" Aeson..= Aeson.toJSON (txToApi tx & Api.serialiseToTextEnvelope Nothing)
          ]
  WS.runClient baseUrlHost baseUrlPort baseUrlPath $ \conn -> do
    WS.sendTextData conn $ TE.decodeUtf8 $ BSL.toStrict $ Aeson.encode newTxJson
    -- ignore the greetings response.
    (_ :: Text) <- WS.receiveData conn
    txResponse <- WS.receiveData conn
    txId <- processWSResponse @HydraTransactionId txResponse
    pure $ transactionId txId
