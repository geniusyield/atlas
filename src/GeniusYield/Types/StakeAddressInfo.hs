{-|
Module      : GeniusYield.Types.StakeAddressInfo
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.StakeAddressInfo (
    GYStakeAddressInfo (..),
) where

import           GeniusYield.Imports
import           GeniusYield.Types.StakePoolId (GYStakePoolId)

data GYStakeAddressInfo = GYStakeAddressInfo
  { gyStakeAddressInfoDelegatedPool    :: Maybe GYStakePoolId
  , gyStakeAddressInfoAvailableRewards :: Natural
  } deriving stock (Eq, Show)
