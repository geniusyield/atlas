{- |
Module      : GeniusYield.Types.StakeKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.StakeKeyHash (
  GYStakeKeyHash,
  stakeKeyHashToApi,
  stakeKeyHashFromApi,
  stakeKeyHashToLedger,
  stakeKeyHashFromLedger,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole

-- | @type GYStakeKeyHash = GYKeyHash 'GYKeyRoleStaking@.
type GYStakeKeyHash = GYKeyHash 'GYKeyRoleStaking

stakeKeyHashToApi :: GYStakeKeyHash -> Api.Hash Api.StakeKey
stakeKeyHashToApi = keyHashToApi

stakeKeyHashFromApi :: Api.Hash Api.StakeKey -> GYStakeKeyHash
stakeKeyHashFromApi = keyHashFromApi

stakeKeyHashToLedger :: GYStakeKeyHash -> Ledger.KeyHash Ledger.Staking
stakeKeyHashToLedger = keyHashToLedger

stakeKeyHashFromLedger :: Ledger.KeyHash Ledger.Staking -> GYStakeKeyHash
stakeKeyHashFromLedger = keyHashFromLedger
