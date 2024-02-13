module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..),
  GYTxMetadataValue,
  gyTxMetaMap,
  gyTxMetaList,
  gyTxMetaNumber,
  gyTxMetaBytes,
  gyTxMetaText,
  makeApiTransactionMetadataValue,
  metadataFromApi,
  metadataToApi,
  metadataMsg,
  metadataMsgs
) where

import qualified Cardano.Api as Api
import qualified Data.Map as Map
import           Data.ByteString     (ByteString)
import           Data.Word           (Word64)
import           GeniusYield.Imports (Text, coerce)


newtype GYTxMetadata = GYTxMetadata (Api.TxMetadataInEra Api.BabbageEra)
  deriving Show

type GYTxMetadataValue = Api.TxMetadataValue

gyTxMetaMap :: [(GYTxMetadataValue, GYTxMetadataValue)] -> GYTxMetadataValue
gyTxMetaMap = Api.TxMetaMap

gyTxMetaList :: [GYTxMetadataValue] -> GYTxMetadataValue
gyTxMetaList = Api.TxMetaList

gyTxMetaNumber :: Integer -> GYTxMetadataValue
gyTxMetaNumber = Api.TxMetaNumber

gyTxMetaBytes :: ByteString -> GYTxMetadataValue
gyTxMetaBytes = Api.TxMetaBytes

gyTxMetaText :: Text -> GYTxMetadataValue
gyTxMetaText = Api.TxMetaText

makeApiTransactionMetadataValue :: Map.Map Word64 GYTxMetadataValue -> Api.TxMetadata
makeApiTransactionMetadataValue = Api.TxMetadata



metadataFromApi :: Api.TxMetadata -> GYTxMetadata
metadataFromApi md@(Api.TxMetadata kvm)
  | Map.null kvm = GYTxMetadata Api.TxMetadataNone
  | otherwise    = GYTxMetadata (Api.TxMetadataInEra Api.TxMetadataInBabbageEra md)

metadataToApi :: GYTxMetadata -> Api.TxMetadata
metadataToApi gymd = case coerce gymd of
  Api.TxMetadataNone -> mempty
  Api.TxMetadataInEra Api.TxMetadataInBabbageEra md -> md



-- | Adds a message (comment/memo) as transaction metadata following CIP 020 specification.
metadataMsg :: Text -> Maybe GYTxMetadata
metadataMsg msg = metadataMsgs [msg]

-- | Adds messages (comments/memos) as transaction metadata following CIP 020 specification.
metadataMsgs :: [Text] -> Maybe GYTxMetadata
metadataMsgs msgs = case repleteMsgs of
  [] -> Nothing
  _  -> Just . metadataFromApi . makeApiTransactionMetadataValue $
        Map.fromList [(674, gyTxMetaMap [(gyTxMetaText "msg", gyTxMetaList $ gyTxMetaText <$> repleteMsgs)])]
  where
    repleteMsgs = filter (/= "") msgs
