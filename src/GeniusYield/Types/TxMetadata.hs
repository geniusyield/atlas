module GeniusYield.Types.TxMetadata (
  GYTxMetadata (..)
) where

import qualified Cardano.Api as Api


newtype GYTxMetadata = GYTxMetadata (Api.TxMetadataInEra Api.BabbageEra)
  deriving Show
