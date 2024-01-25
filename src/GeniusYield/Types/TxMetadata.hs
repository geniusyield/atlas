module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..),
) where

import Cardano.Api qualified as Api

newtype GYTxMetadata = GYTxMetadata (Api.TxMetadataInEra Api.BabbageEra)
