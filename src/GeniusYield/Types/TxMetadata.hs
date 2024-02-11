module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..),
  metadataFromApi
) where

import qualified Cardano.Api as Api
import qualified Data.Map as Map
-- import           Data.Word (Word64)


newtype GYTxMetadata = GYTxMetadata (Api.TxMetadataInEra Api.BabbageEra)
  deriving Show

metadataFromApi :: Api.TxMetadata -> Maybe GYTxMetadata
metadataFromApi md
  | Map.null kvMap = Nothing
  | otherwise      = Just . GYTxMetadata . Api.TxMetadataInEra Api.TxMetadataInBabbageEra $ md
  where
    Api.TxMetadata kvMap = md

-- TODO  other direction :  metadataToApi  (coerce)


-- TODO GY version of Api.TxMetadtaValue

-- Keep Maybe in skeleton
