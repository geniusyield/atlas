{- |
Module      : GeniusYield.Providers.Ogmios
Description : Ogmios provider for remote node connection
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Ogmios (
  OgmiosApiEnv,
  newOgmiosApiEnv,
  OgmiosProviderException (..),
  ogmiosSubmitTx,
  ogmiosProtocolParameters,
) where

import Cardano.Api qualified as Api
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<))
import Data.Aeson (Value (Null), encode, object, withObject, (.:), (.=))
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Deriving.Aeson
import GeniusYield.Imports
import GeniusYield.Providers.Common (
  SubmitTxException (..),
  datumFromCBOR,
  extractAssetClass,
  newServantClientEnv,
 )
import GeniusYield.Types
import GeniusYield.Types qualified as GYTypes (PlutusVersion (..))
import GeniusYield.Types.Script (GYAnyScript (..))
import Servant.API (
  Capture,
  Get,
  Header,
  Headers (getResponse),
  JSON,
  Post,
  QueryFlag,
  QueryParam,
  ReqBody,
  ResponseHeader (Header),
  lookupResponseHeader,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  ClientEnv,
  ClientError,
  ClientM,
  client,
  runClientM,
 )

newtype OgmiosApiEnv = OgmiosApiEnv ClientEnv

-- | Returns a new 'OgmiosApiEnv' given the base url to query from.
newOgmiosApiEnv :: String -> IO OgmiosApiEnv
newOgmiosApiEnv baseUrl = OgmiosApiEnv <$> newServantClientEnv baseUrl

-- | Exceptions.
data OgmiosProviderException
  = -- | Error from the Ogmios API.
    OgmiosApiError !Text !ClientError
  | -- TODO: Is OgmiosAbsurdResponse needed?

    -- | Received an absurd response from Ogmios. This shouldn't ever happen.
    OgmiosAbsurdResponse !Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

{-# INLINEABLE runOgmiosClient #-}
runOgmiosClient :: OgmiosApiEnv -> ClientM a -> IO (Either ClientError a)
runOgmiosClient (OgmiosApiEnv cEnv) c = runClientM c cEnv

{-# INLINEABLE handleOgmiosError #-}
handleOgmiosError :: Text -> Either ClientError a -> IO a
handleOgmiosError locationInfo = either (throwIO . OgmiosApiError locationInfo) pure

-- TODO: Remove these comments.
class ToJSONRPC a where
  toMethod :: a -> Text

  -- TODO: Does empty map work same as Nothing?
  toParams :: a -> Maybe Value

instance ToJSONRPC GYTx where
  toMethod = const "submitTransaction"
  toParams tx = Just $ toJSON $ Map.fromList [("transaction" :: Text, Map.fromList [("cbor" :: Text, txToHex tx)])]

newtype OgmiosRequest a = OgmiosRequest a

instance ToJSONRPC a => ToJSON (OgmiosRequest a) where
  toJSON (OgmiosRequest a) =
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= toMethod a
      , "params" .= toParams a
      ]

newtype OgmiosResponse a = OgmiosResponse
  { response :: Either Value a
  }
  deriving stock Show

instance FromJSON a => FromJSON (OgmiosResponse a) where
  parseJSON = withObject "OgmiosResponse" $ \o -> do
    result <- o .: "result"
    case result of
      Null -> OgmiosResponse . Left <$> o .: "error"
      _ -> OgmiosResponse . Right <$> parseJSON result

newtype TxIdResponse = TxIdResponse
  { id :: GYTxId
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

newtype TxSubmissionResponse = TxSubmissionResponse
  { transaction :: TxIdResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

submitTx :: OgmiosRequest GYTx -> ClientM (OgmiosResponse TxSubmissionResponse)
protocolParams :: OgmiosRequest OgmiosPP -> ClientM Value

data OgmiosPP = OgmiosPP

instance ToJSONRPC OgmiosPP where
  toMethod = const "queryLedgerState/protocolParameters"
  toParams = const Nothing

type OgmiosApi = ReqBody '[JSON] (OgmiosRequest GYTx) :> Post '[JSON] (OgmiosResponse TxSubmissionResponse) :<|> ReqBody '[JSON] (OgmiosRequest OgmiosPP) :> Post '[JSON] Value

submitTx :<|> protocolParams = client @OgmiosApi Proxy

-- | Submit a transaction to the node via Ogmios.
ogmiosSubmitTx :: OgmiosApiEnv -> GYSubmitTx
ogmiosSubmitTx env tx = do
  let debreq = OgmiosRequest tx
  putStrLn $ "Request body: " <> show (encode debreq)
  OgmiosResponse rs <-
    handleOgmiosError fn
      <=< runOgmiosClient env
      $ submitTx (OgmiosRequest tx)
  case rs of
    -- TODO: Does error message look similar in case of say Maestro?
    Left err -> throwIO . SubmitTxException . Text.pack . show $ err
    Right (TxSubmissionResponse (TxIdResponse txId)) -> pure txId
 where
  fn = "ogmiosSubmitTx"

-- | Fetch protocol parameters.
ogmiosProtocolParameters :: OgmiosApiEnv -> IO (Value)
ogmiosProtocolParameters env = do
  val <-
    handleOgmiosError fn
      <=< runOgmiosClient env
      $ protocolParams (OgmiosRequest OgmiosPP)
  putStrLn $ "Response body: " <> show val
  pure val
 where
  -- case val of
  --   -- TODO: Does error message look similar in case of say Maestro?
  --   Left err -> throwIO . SubmitTxException . Text.pack . show $ err
  --   Right (TxSubmissionResponse (TxIdResponse txId)) -> pure txId

  fn = "ogmiosProtocolParameters"