{-|
Module      : GeniusYield.Types.TxMetadata
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.TxMetadata (
  GYTxMetadata,
  GYTxMetadataValue,
  gyTxMetaMap,
  gyTxMetaList,
  gyTxMetaNumber,
  gyTxMetaBytes,
  gyTxMetaText,
  gyMetaTextChunks,
  gyMetaBytesChunks,
  mergeGYTransactionMetadata,
  makeApiTransactionMetadata,
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


newtype GYTxMetadata = GYTxMetadata Api.TxMetadata
  deriving newtype (Semigroup, Show)

type GYTxMetadataValue = Api.TxMetadataValue

--------------------------------------------------------------------------------
-- Isomorphism between 'Cardano.Api.TxMetadata' and 'GYTxMetadata'
--------------------------------------------------------------------------------

-- | Convert 'Cardano.Api.TxMetadata' to 'GYTxMetadata'.
metadataFromApi :: Api.TxMetadata -> GYTxMetadata
metadataFromApi = GYTxMetadata  

-- | Convert 'GYTxMetadata' to 'Cardano.Api.TxMetadata'.
metadataToApi :: GYTxMetadata -> Api.TxMetadata
metadataToApi = coerce


--------------------------------------------------------------------------------
-- Convenience functions for dealing with 'GYTxMetadataValue's and 'TxMetadata'
--------------------------------------------------------------------------------

-- | Apply 'Map' constructor of 'GYTxMetadataValue'.
gyTxMetaMap :: [(GYTxMetadataValue, GYTxMetadataValue)] -> GYTxMetadataValue
gyTxMetaMap = Api.TxMetaMap

-- | Apply 'List' constructor of 'GYTxMetadataValue'.
gyTxMetaList :: [GYTxMetadataValue] -> GYTxMetadataValue
gyTxMetaList = Api.TxMetaList

-- | Apply 'Number' constructor of 'GYTxMetadataValue'.
gyTxMetaNumber :: Integer -> GYTxMetadataValue
gyTxMetaNumber = Api.TxMetaNumber

-- | Apply 'Bytes' constructor of 'GYTxMetadataValue'.
gyTxMetaBytes :: ByteString -> GYTxMetadataValue
gyTxMetaBytes = Api.TxMetaBytes

-- | Apply 'Text' constructor of 'GYTxMetadataValue'.
gyTxMetaText :: Text -> GYTxMetadataValue
gyTxMetaText = Api.TxMetaText

-- | Create a 'GYTxMetadataValue' from a Text as a list of chunks of an acceptable size.
gyMetaTextChunks :: Text -> GYTxMetadataValue
gyMetaTextChunks = Api.metaTextChunks

-- | Create a 'GYTxMetadataValue' from a 'ByteString' as a list of chunks of an accaptable size.
gyMetaBytesChunks :: ByteString -> GYTxMetadataValue
gyMetaBytesChunks = Api.metaBytesChunks

-- | Merge two 'GYTxMetadata's, controlling how to handle the respective 'GYTxMetadataValue's in case of label collision.
mergeGYTransactionMetadata :: (GYTxMetadataValue -> GYTxMetadataValue -> GYTxMetadataValue) -> GYTxMetadata -> GYTxMetadata -> GYTxMetadata
mergeGYTransactionMetadata f gymd1 gymd2 = metadataFromApi $ Api.mergeTransactionMetadata f (metadataToApi gymd1) (metadataToApi gymd2)

-- | Apply 'TxMetadata' wrapper to a 'Map Word64 GYTxMetadataValue'.
makeApiTransactionMetadata :: Map.Map Word64 GYTxMetadataValue -> Api.TxMetadata
makeApiTransactionMetadata = Api.TxMetadata


----------------------------------------------------------------------------------------------
-- Convenience functions for adding messages (comments/memos) following CIP 020 specification.
----------------------------------------------------------------------------------------------

-- | Adds a single message (comment/memo) as transaction metadata following CIP 020 specification.
metadataMsg :: Text -> Maybe GYTxMetadata
metadataMsg msg = metadataMsgs [msg]

-- | Adds multiple messages (comments/memos) as transaction metadata following CIP 020 specification.
metadataMsgs :: [Text] -> Maybe GYTxMetadata
metadataMsgs msgs = case metaValue of
  Api.TxMetaList [] -> Nothing
  _                 -> Just . metadataFromApi . makeApiTransactionMetadata $
                       Map.fromList [(674, gyTxMetaMap [(gyTxMetaText "msg", metaValue)])]
  where
    metaValue :: GYTxMetadataValue
    metaValue = foldr metaAppend (Api.TxMetaList []) (gyMetaTextChunks <$> msgs)
    
    metaAppend :: GYTxMetadataValue -> GYTxMetadataValue -> GYTxMetadataValue
    metaAppend (Api.TxMetaList xs) (Api.TxMetaList ys) = Api.TxMetaList (xs ++ ys)
    metaAppend _ _ = error "Unexpected error while using 'gyMetaTextChunks'"

