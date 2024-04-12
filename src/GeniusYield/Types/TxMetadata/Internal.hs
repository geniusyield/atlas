{-|
Module      : GeniusYield.Types.TxMetadata.Internal
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Internal module defining the 'GYTxMetadataValue' type and exposing all it's constructors.
-}

module GeniusYield.Types.TxMetadata.Internal (
  GYTxMetadataValue (..),
  txMetadataValueToApi,
  txMetadataValueFromApi
) where

import qualified Cardano.Api         as Api
import           Data.ByteString     (ByteString)
import           GeniusYield.Imports (Text, bimap)

-- | A value in the transaction metadata.
data GYTxMetadataValue =
    GYTxMetaMap [(GYTxMetadataValue, GYTxMetadataValue)]
  | GYTxMetaList [GYTxMetadataValue]
  | GYTxMetaNumber Integer
  | GYTxMetaBytes ByteString
  | GYTxMetaText Text
  deriving stock (Show, Eq, Ord)

txMetadataValueToApi :: GYTxMetadataValue -> Api.TxMetadataValue
txMetadataValueToApi (GYTxMetaMap m) = Api.TxMetaMap $ bimap txMetadataValueToApi txMetadataValueToApi <$> m
txMetadataValueToApi (GYTxMetaList l) = Api.TxMetaList $ txMetadataValueToApi <$> l
txMetadataValueToApi (GYTxMetaNumber i) = Api.TxMetaNumber i
txMetadataValueToApi (GYTxMetaBytes b) = Api.TxMetaBytes b
txMetadataValueToApi (GYTxMetaText t) = Api.TxMetaText t

txMetadataValueFromApi :: Api.TxMetadataValue -> GYTxMetadataValue
txMetadataValueFromApi (Api.TxMetaMap m) = GYTxMetaMap $ bimap txMetadataValueFromApi txMetadataValueFromApi <$> m
txMetadataValueFromApi (Api.TxMetaList l) = GYTxMetaList $ txMetadataValueFromApi <$> l
txMetadataValueFromApi (Api.TxMetaNumber i) = GYTxMetaNumber i
txMetadataValueFromApi (Api.TxMetaBytes b) = GYTxMetaBytes b
txMetadataValueFromApi (Api.TxMetaText t) = GYTxMetaText t
