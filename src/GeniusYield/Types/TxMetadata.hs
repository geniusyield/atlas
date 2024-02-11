module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..),
  metadataFromApi,
  metadataMsg
) where

import qualified Cardano.Api as Api
import qualified Data.Map as Map
import           Data.Text (Text)


newtype GYTxMetadata = GYTxMetadata (Api.TxMetadataInEra Api.BabbageEra)
  deriving Show

metadataFromApi :: Api.TxMetadata -> GYTxMetadata
metadataFromApi md@(Api.TxMetadata kvm)
  | Map.null kvm = GYTxMetadata Api.TxMetadataNone
  | otherwise    = GYTxMetadata . Api.TxMetadataInEra Api.TxMetadataInBabbageEra $ md

metadataMsg :: Text -> GYTxMetadata
metadataMsg msg = metadataFromApi . Api.makeTransactionMetadata $
                  Map.fromList [(674, Api.TxMetaMap [(Api.TxMetaText "msg", Api.TxMetaList [Api.TxMetaText msg])])]


-- TODO  other direction :  metadataToApi  (coerce)

-- Keep Maybe in skeleton
