{-|
Module      : GeniusYield.Types.Key.Class
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Key.Class (
  ToShelleyWitnessSigningKey,
  toShelleyWitnessSigningKey,
) where

import qualified Cardano.Api as Api

class ToShelleyWitnessSigningKey a where
  toShelleyWitnessSigningKey :: a -> Api.ShelleyWitnessSigningKey
