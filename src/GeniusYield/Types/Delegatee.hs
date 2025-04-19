{- |
Module      : GeniusYield.Types.Delegatee
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Delegatee (
  GYDelegatee (..),
  delegateeToLedger,
  delegateeFromLedger,
) where

import Cardano.Api.Ledger qualified as Ledger
import GeniusYield.Types.DRep
import GeniusYield.Types.StakePoolId

data GYDelegatee
  = GYDelegStake !GYStakePoolId
  | GYDelegVote !GYDRep
  | GYDelegStakeVote !GYStakePoolId !GYDRep
  deriving stock (Eq, Ord, Show)

delegateeToLedger :: GYDelegatee -> Ledger.Delegatee
delegateeToLedger del = case del of
  GYDelegStake sp -> Ledger.DelegStake $ stakePoolIdToLedger sp
  GYDelegVote drep -> Ledger.DelegVote $ drepToLedger drep
  GYDelegStakeVote sp drep -> Ledger.DelegStakeVote (stakePoolIdToLedger sp) (drepToLedger drep)

delegateeFromLedger :: Ledger.Delegatee -> GYDelegatee
delegateeFromLedger del = case del of
  Ledger.DelegStake sp -> GYDelegStake $ stakePoolIdFromLedger sp
  Ledger.DelegVote drep -> GYDelegVote $ drepFromLedger drep
  Ledger.DelegStakeVote sp drep -> GYDelegStakeVote (stakePoolIdFromLedger sp) (drepFromLedger drep)
