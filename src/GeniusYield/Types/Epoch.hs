{- |
Module      : GeniusYield.Types.Epoch
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Epoch (
  GYEpochNo (..),
  epochNoFromApi,
  epochNoToApi,
  epochNoFromLedger,
  epochNoToLedger,
  GYEpochSize (..),
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Data.Word (Word64)
import GeniusYield.Imports (coerce)

newtype GYEpochNo = GYEpochNo Word64
  deriving (Show, Read, Eq, Ord)

epochNoFromApi :: Api.EpochNo -> GYEpochNo
epochNoFromApi = coerce

epochNoToApi :: GYEpochNo -> Api.EpochNo
epochNoToApi = coerce

epochNoToLedger :: GYEpochNo -> Ledger.EpochNo
epochNoToLedger = coerce

epochNoFromLedger :: Ledger.EpochNo -> GYEpochNo
epochNoFromLedger = coerce

newtype GYEpochSize = GYEpochSize Word64
  deriving (Show, Read, Eq, Ord)
