{- |
Module      : GeniusYield.Types.KeyRole
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.KeyRole (
  GYKeyRole (..),
  SingGYKeyRole (..),
  fromSingGYKeyRole,
  SingGYKeyRoleI (..),
  GYKeyRoleToLedger,
  GYKeyRoleVRF (..),
) where

import Cardano.Api.Ledger qualified as Ledger

-- | Role of a key.
data GYKeyRole
  = GYKeyRolePayment
  | GYKeyRoleStaking
  | GYKeyRoleDRep
  | GYKeyRoleStakePool
  | GYKeyRoleHotCommittee
  | GYKeyRoleColdCommittee
  deriving (Show, Eq, Ord)

data SingGYKeyRole (kr :: GYKeyRole) where
  SingGYKeyRolePayment :: SingGYKeyRole 'GYKeyRolePayment
  SingGYKeyRoleStaking :: SingGYKeyRole 'GYKeyRoleStaking
  SingGYKeyRoleDRep :: SingGYKeyRole 'GYKeyRoleDRep
  SingGYKeyRoleStakePool :: SingGYKeyRole 'GYKeyRoleStakePool
  SingGYKeyRoleHotCommittee :: SingGYKeyRole 'GYKeyRoleHotCommittee
  SingGYKeyRoleColdCommittee :: SingGYKeyRole 'GYKeyRoleColdCommittee

fromSingGYKeyRole :: SingGYKeyRole kr -> GYKeyRole
fromSingGYKeyRole SingGYKeyRolePayment = GYKeyRolePayment
fromSingGYKeyRole SingGYKeyRoleStaking = GYKeyRoleStaking
fromSingGYKeyRole SingGYKeyRoleDRep = GYKeyRoleDRep
fromSingGYKeyRole SingGYKeyRoleStakePool = GYKeyRoleStakePool
fromSingGYKeyRole SingGYKeyRoleHotCommittee = GYKeyRoleHotCommittee
fromSingGYKeyRole SingGYKeyRoleColdCommittee = GYKeyRoleColdCommittee

class SingGYKeyRoleI (kr :: GYKeyRole) where singGYKeyRole :: SingGYKeyRole kr

instance SingGYKeyRoleI 'GYKeyRolePayment where singGYKeyRole = SingGYKeyRolePayment
instance SingGYKeyRoleI 'GYKeyRoleStaking where singGYKeyRole = SingGYKeyRoleStaking
instance SingGYKeyRoleI 'GYKeyRoleDRep where singGYKeyRole = SingGYKeyRoleDRep
instance SingGYKeyRoleI 'GYKeyRoleStakePool where singGYKeyRole = SingGYKeyRoleStakePool
instance SingGYKeyRoleI 'GYKeyRoleHotCommittee where singGYKeyRole = SingGYKeyRoleHotCommittee
instance SingGYKeyRoleI 'GYKeyRoleColdCommittee where singGYKeyRole = SingGYKeyRoleColdCommittee

type family GYKeyRoleToLedger (kr :: GYKeyRole) :: Ledger.KeyRole where
  GYKeyRoleToLedger 'GYKeyRolePayment = Ledger.Payment
  GYKeyRoleToLedger 'GYKeyRoleStaking = Ledger.Staking
  GYKeyRoleToLedger 'GYKeyRoleDRep = Ledger.DRepRole
  GYKeyRoleToLedger 'GYKeyRoleStakePool = Ledger.StakePool
  GYKeyRoleToLedger 'GYKeyRoleHotCommittee = Ledger.HotCommitteeRole
  GYKeyRoleToLedger 'GYKeyRoleColdCommittee = Ledger.ColdCommitteeRole

-- | Role of a VRF key.
data GYKeyRoleVRF
  = GYKeyRoleVRFStakePool
