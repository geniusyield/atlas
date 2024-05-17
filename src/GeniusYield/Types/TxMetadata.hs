{-|
Module      : GeniusYield.Types.TxMetadata
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Transaction metadata types and functions for working with [transaction metadata](https://developers.cardano.org/docs/transaction-metadata/).
-}
module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..),
  txMetadataFromApi,
  txMetadataToApi,
  GYTxMetadataValue,
  txMetadataValueToApi,
  txMetadataValueFromApi,
  constructTxMetadataMap,
  constructTxMetadataList,
  constructTxMetadataNumber,
  constructTxMetadataBytes,
  constructTxMetadataText,
  constructTxMetadataBytesChunks,
  constructTxMetadataTextChunks,
  mergeTxMetadata,
  metadataMsg,
  metadataMsgs
) where

import qualified Cardano.Api                           as Api
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as BS
import qualified Data.Map.Strict                       as Map
import qualified Data.Text.Encoding                    as TE
import           Data.Word                             (Word64)
import           GeniusYield.Imports                   (Text)
import           GeniusYield.Types.TxMetadata.Internal (GYTxMetadataValue (..),
                                                        txMetadataValueFromApi,
                                                        txMetadataValueToApi)


newtype GYTxMetadata = GYTxMetadata (Map.Map Word64 GYTxMetadataValue)
  deriving newtype (Eq, Semigroup, Show)

txMetadataToApi :: GYTxMetadata -> Api.TxMetadata
txMetadataToApi (GYTxMetadata m) = Api.TxMetadata (Map.map txMetadataValueToApi m)

txMetadataFromApi :: Api.TxMetadata -> GYTxMetadata
txMetadataFromApi (Api.TxMetadata m) = GYTxMetadata (Map.map txMetadataValueFromApi m)

-- | Construct a 'GYTxMetadataValue' from a list of key-value pairs.
constructTxMetadataMap :: [(GYTxMetadataValue, GYTxMetadataValue)] -> GYTxMetadataValue
constructTxMetadataMap = GYTxMetaMap

-- | Construct a 'GYTxMetadataValue' from a list of 'GYTxMetadataValue's.
constructTxMetadataList :: [GYTxMetadataValue] -> GYTxMetadataValue
constructTxMetadataList = GYTxMetaList

-- | Construct a 'GYTxMetadataValue' from an 'Integer'. Returning 'Nothing' if the given 'Integer' is not in the range @-(2^64-1) .. 2^64-1@.
constructTxMetadataNumber :: Integer -> Maybe GYTxMetadataValue
constructTxMetadataNumber n =
  let bound :: Integer = fromIntegral (maxBound :: Word64) in
  if n >= negate bound && n <= bound
  then Just $ GYTxMetaNumber n
  else Nothing

-- | Construct a 'GYTxMetadataValue' from a 'ByteString'. Returning 'Nothing' if the given 'ByteString' is longer than 64 bytes.
constructTxMetadataBytes :: ByteString -> Maybe GYTxMetadataValue
constructTxMetadataBytes bs =
  let len = BS.length bs in
  if len <= 64
    then Just $ GYTxMetaBytes bs
    else Nothing

-- | Construct a 'GYTxMetadataValue' from a 'Text'. Returning 'Nothing' if the given 'Text' is longer than 64 bytes when UTF-8 encoded.
constructTxMetadataText :: Text -> Maybe GYTxMetadataValue
constructTxMetadataText txt =
  let len = BS.length $ TE.encodeUtf8 txt in
  if len <= 64
    then Just $ GYTxMetaText txt
    else Nothing

-- | Construct a 'GYTxMetadataValue' from a 'ByteString' as a list of chunks (bytestrings) of acceptable sizes, splitting at 64th byte as maximum length allowed is 64 bytes.
constructTxMetadataBytesChunks :: ByteString -> GYTxMetadataValue
constructTxMetadataBytesChunks = txMetadataValueFromApi . Api.metaBytesChunks

-- | Construct a 'GYTxMetadataValue' from a 'Text' as a list of chunks (strings) of acceptable sizes. Note that maximum length allowed is 64 bytes, when given string is UTF-8 encoded. Splitting is slightly involved here as single character (in a text string) may be represented by multiple bytes in UTF-8 encoding, and hence, we split if adding the byte representation of the char exceeds the 64 byte limit.
constructTxMetadataTextChunks :: Text -> GYTxMetadataValue
constructTxMetadataTextChunks = txMetadataValueFromApi . Api.TxMetaList . constructTxMetadataTextChunks'

constructTxMetadataTextChunks' :: Text -> [Api.TxMetadataValue]
constructTxMetadataTextChunks' txt = case Api.metaTextChunks txt of
  Api.TxMetaList xs -> xs
  _                 -> error "GeniusYield.Types.TxMetadata.constructTxMetadataTextChunks': Absurd, expected TxMetaList"

-- | Merge two 'GYTxMetadata's, controlling how to handle the respective 'GYTxMetadataValue's in case of label collision.
mergeTxMetadata :: (GYTxMetadataValue -> GYTxMetadataValue -> GYTxMetadataValue) -> GYTxMetadata -> GYTxMetadata -> GYTxMetadata
mergeTxMetadata f (GYTxMetadata m1) (GYTxMetadata m2) = GYTxMetadata $ Map.unionWith f m1 m2

----------------------------------------------------------------------------------------------
-- Convenience functions for adding messages (comments/memos) following CIP 020 specification.
----------------------------------------------------------------------------------------------

-- | Adds a single message (comment/memo) as transaction metadata following CIP 020 specification.
--
-- See 'metadataMsgs' for examples.
metadataMsg :: Text -> Maybe GYTxMetadata
metadataMsg msg = metadataMsgs [msg]

-- | Adds multiple messages (comments/memos) as transaction metadata following CIP 020 specification.
--
-- >>> metadataMsgs ["Hello, World!", "This is a test message."]
-- Just (fromList [(674,GYTxMetaMap [(GYTxMetaText "msg",GYTxMetaList [GYTxMetaText "Hello, World!",GYTxMetaText "This is a test message."])])])
--
-- >>> metadataMsgs [""]
-- Nothing
--
-- >>> metadataMsgs []
-- Nothing
--
-- >>> metadataMsgs ["Hello, World!", "This one is a reaaaaaaaally long message, so long that it exceeds the 64 byte limit, so it will be split into multiple chunks.", "do you see that?"]
-- Just (fromList [(674,GYTxMetaMap [(GYTxMetaText "msg",GYTxMetaList [GYTxMetaText "Hello, World!",GYTxMetaText "This one is a reaaaaaaaally long message, so long that it exceed",GYTxMetaText "s the 64 byte limit, so it will be split into multiple chunks.",GYTxMetaText "do you see that?"])])])
--
metadataMsgs :: [Text] -> Maybe GYTxMetadata
metadataMsgs msgs = case metaValue of
  GYTxMetaList [] -> Nothing
  _               -> Just $ GYTxMetadata $ Map.fromList [(674, GYTxMetaMap [(GYTxMetaText "msg", metaValue)])]
  where
    metaValue :: GYTxMetadataValue
    metaValue = txMetadataValueFromApi $ Api.TxMetaList $ concatMap constructTxMetadataTextChunks' msgs
