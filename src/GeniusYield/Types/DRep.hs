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
  GYDRepState (..),
  drepStateToLedger,
  drepStateFromLedger,
) where

import Cardano.Api.Ledger (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Ledger
import Data.Set qualified as Set
import GeniusYield.Imports (Natural, Set)
import GeniusYield.Types.Anchor
import GeniusYield.Types.Credential (GYCredential, credentialFromLedger, credentialToLedger)
import GeniusYield.Types.Epoch (GYEpochNo, epochNoFromLedger, epochNoToLedger)
import GeniusYield.Types.KeyRole (GYKeyRole (..))

-- | DRep.
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

-- | DRep state.
data GYDRepState = GYDRepState
  { drepExpiry :: !GYEpochNo
  , drepAnchor :: !(Maybe GYAnchor)
  , drepDeposit :: !Natural
  , drepDelegs :: !(Set (GYCredential 'GYKeyRoleStaking))
  }
  deriving (Show, Eq, Ord)

drepStateToLedger :: GYDRepState -> Ledger.DRepState Ledger.StandardCrypto
drepStateToLedger GYDRepState {..} =
  Ledger.DRepState
    { Ledger.drepExpiry = epochNoToLedger drepExpiry
    , Ledger.drepAnchor = maybeToStrictMaybe drepAnchor
    , Ledger.drepDeposit = fromIntegral drepDeposit
    , Ledger.drepDelegs = Set.map credentialToLedger drepDelegs
    }

drepStateFromLedger :: Ledger.DRepState Ledger.StandardCrypto -> GYDRepState
drepStateFromLedger Ledger.DRepState {..} =
  GYDRepState
    { drepExpiry = epochNoFromLedger drepExpiry
    , drepAnchor = strictMaybeToMaybe drepAnchor
    , drepDeposit = fromIntegral drepDeposit
    , drepDelegs = Set.map credentialFromLedger drepDelegs
    }