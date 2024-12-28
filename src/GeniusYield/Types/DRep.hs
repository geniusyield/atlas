{- |
Module      : GeniusYield.Types.DRep
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.DRep (
  GYDRep (..),
  drepToLedger,
  drepFromLedger,
) where

import Cardano.Api.Ledger qualified as Ledger
import GeniusYield.Imports (Natural, Set)
import GeniusYield.Types.Anchor
import GeniusYield.Types.Credential (GYCredential, credentialFromLedger, credentialToLedger)
import GeniusYield.Types.Epoch (GYEpochNo)
import GeniusYield.Types.KeyRole (GYKeyRole (..))

data GYDRep
  = GYDRepCredential !(GYCredential 'GYKeyRoleDRep)
  | GYDRepAlwaysAbstain
  | GYDRepAlwaysNoConfidence
  deriving stock (Show, Eq, Ord)

drepToLedger :: GYDRep -> Ledger.DRep Ledger.StandardCrypto
drepToLedger drep = case drep of
  GYDRepCredential c -> Ledger.DRepCredential $ credentialToLedger c
  GYDRepAlwaysAbstain -> Ledger.DRepAlwaysAbstain
  GYDRepAlwaysNoConfidence -> Ledger.DRepAlwaysNoConfidence

drepFromLedger :: Ledger.DRep Ledger.StandardCrypto -> GYDRep
drepFromLedger drep = case drep of
  Ledger.DRepCredential c -> GYDRepCredential $ credentialFromLedger c
  Ledger.DRepAlwaysAbstain -> GYDRepAlwaysAbstain
  Ledger.DRepAlwaysNoConfidence -> GYDRepAlwaysNoConfidence

data DRepState = DRepState
  { drepExpiry :: !GYEpochNo
  , drepAnchor :: !(Maybe GYAnchor)
  , drepDeposit :: !Natural
  , drepDelegs :: !(Set (GYCredential 'GYKeyRoleStaking))
  }
  deriving (Show, Eq, Ord)

-- drepStateToLedger :: DRepState -> Ledger.DRepState Ledger.StandardCrypto
-- drepStateToLedger DRepState {..} =
--   Ledger.DRepState
--     { Ledger.drepExpiry = epochNoToLedger drepExpiry
--     , Ledger.drepAnchor = anchorToLedger <$> drepAnchor
--     , Ledger.drepDeposit = drepDeposit
--     , Ledger.drepDelegs = credentialToLedger <$> drepDelegs
--     }
