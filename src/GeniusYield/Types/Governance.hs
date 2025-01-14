{- |
Module      : GeniusYield.Types.Governance
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Governance (
  GYVote (..),
  voteFromLedger,
  voteToLedger,
  GYVoter (..),
  voterFromLedger,
  voterToLedger,
  GYGovActionId (..),
  govActionIdFromLedger,
  govActionIdToLedger,
  GYVotingProcedure (..),
  votingProcedureFromLedger,
  votingProcedureToLedger,
  GYVotingProcedures,
  votingProceduresFromLedger,
  votingProceduresToLedger,
  combineVotingProcedures,
  GYTxVotingProcedures,
  combineTxVotingProcedures,
  GYProposalProcedurePB (..),
  GYProposalProcedure (..),
  completeProposalProcedure,
  propProcToLedger,
  GYConstitution (..),
  constitutionToLedger,
  GYGovAction (..),
  govActionToLedger,
) where

import Cardano.Api.Ledger (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word16)
import GeniusYield.Imports (Map, Natural, Set, (&))
import GeniusYield.Types.Address (GYStakeAddress, stakeAddressToLedger)
import GeniusYield.Types.Anchor
import GeniusYield.Types.BuildWitness
import GeniusYield.Types.Credential (GYCredential, credentialFromLedger, credentialToLedger)
import GeniusYield.Types.Epoch (GYEpochNo, epochNoToLedger)
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole (GYKeyRole (..))
import GeniusYield.Types.Reexpose (ProtVer, UnitInterval)
import GeniusYield.Types.Script (GYScriptHash, scriptHashToLedger)
import GeniusYield.Types.Tx (GYTxId, txIdFromApi, txIdToApi)
import Ouroboros.Consensus.Shelley.Eras qualified as Consensus

-- | Vote on a governance proposal.
data GYVote = Yes | No | Abstain
  deriving (Eq, Show, Ord, Enum, Bounded)

voteToLedger :: GYVote -> Ledger.Vote
voteToLedger Yes = Ledger.VoteYes
voteToLedger No = Ledger.VoteNo
voteToLedger Abstain = Ledger.Abstain

voteFromLedger :: Ledger.Vote -> GYVote
voteFromLedger Ledger.VoteYes = Yes
voteFromLedger Ledger.VoteNo = No
voteFromLedger Ledger.Abstain = Abstain

-- | Voter.
data GYVoter
  = CommitteeVoter !(GYCredential 'GYKeyRoleHotCommittee)
  | DRepVoter !(GYCredential 'GYKeyRoleDRep)
  | StakePoolVoter !(GYKeyHash 'GYKeyRoleStakePool)
  deriving (Eq, Show, Ord)

type Era = Ledger.EraCrypto Consensus.StandardConway

voterToLedger :: GYVoter -> Ledger.Voter Era
voterToLedger (CommitteeVoter c) = Ledger.CommitteeVoter (credentialToLedger c)
voterToLedger (DRepVoter c) = Ledger.DRepVoter (credentialToLedger c)
voterToLedger (StakePoolVoter k) = Ledger.StakePoolVoter (keyHashToLedger k)

voterFromLedger :: Ledger.Voter (Ledger.EraCrypto Consensus.StandardConway) -> GYVoter
voterFromLedger (Ledger.CommitteeVoter c) = CommitteeVoter (credentialFromLedger c)
voterFromLedger (Ledger.DRepVoter c) = DRepVoter (credentialFromLedger c)
voterFromLedger (Ledger.StakePoolVoter k) = StakePoolVoter (keyHashFromLedger k)

data GYGovActionId = GYGovActionId
  {gaidTxId :: !GYTxId, gaidIx :: !Word16}
  deriving (Eq, Show, Ord)

govActionIdToLedger :: GYGovActionId -> Ledger.GovActionId Era
govActionIdToLedger (GYGovActionId txId ix) = Ledger.GovActionId (txIdToApi txId & Api.toShelleyTxId) (Ledger.GovActionIx ix)

govActionIdFromLedger :: Ledger.GovActionId Era -> GYGovActionId
govActionIdFromLedger (Ledger.GovActionId txId (Ledger.GovActionIx ix)) = GYGovActionId (txIdFromApi (Api.fromShelleyTxId txId)) ix

-- | Voting procedure.
data GYVotingProcedure = GYVotingProcedure
  { vpVote :: !GYVote
  , vpAnchor :: !(Maybe GYAnchor)
  }
  deriving stock (Show, Eq, Ord)

votingProcedureToLedger :: GYVotingProcedure -> Ledger.VotingProcedure Consensus.StandardConway
votingProcedureToLedger (GYVotingProcedure v a) = Ledger.VotingProcedure (voteToLedger v) (maybeToStrictMaybe (anchorToLedger <$> a))

votingProcedureFromLedger :: Ledger.VotingProcedure Consensus.StandardConway -> GYVotingProcedure
votingProcedureFromLedger (Ledger.VotingProcedure v a) = GYVotingProcedure (voteFromLedger v) (strictMaybeToMaybe (anchorFromLedger <$> a))

type GYVotingProcedures = Map GYVoter (Map GYGovActionId GYVotingProcedure)

votingProceduresToLedger :: GYVotingProcedures -> Ledger.VotingProcedures Consensus.StandardConway
votingProceduresToLedger vp = Ledger.VotingProcedures $ Map.mapKeys voterToLedger $ Map.map (Map.mapKeys govActionIdToLedger . Map.map votingProcedureToLedger) vp

votingProceduresFromLedger :: Ledger.VotingProcedures Consensus.StandardConway -> GYVotingProcedures
votingProceduresFromLedger (Ledger.VotingProcedures vp) = Map.mapKeys voterFromLedger $ Map.map (Map.mapKeys govActionIdFromLedger . Map.map votingProcedureFromLedger) vp

-- | Combine two voting procedures. Here if a voter has voted on the same proposal in both procedures, the vote from the second procedure is taken.
combineVotingProcedures :: GYVotingProcedures -> GYVotingProcedures -> GYVotingProcedures
combineVotingProcedures = Map.unionWith (flip Map.union)

type GYTxVotingProcedures v = Map GYVoter (GYTxBuildWitness v, Map GYGovActionId GYVotingProcedure)

-- | Combine two voting procedures. Here if a voter has voted on the same proposal in both procedures, the vote from the second procedure is taken. Likewise, witness from the second map is taken in case of conflicts.
combineTxVotingProcedures :: GYTxVotingProcedures v -> GYTxVotingProcedures v -> GYTxVotingProcedures v
combineTxVotingProcedures = Map.unionWith (\(_w1, vp1) (w2, vp2) -> (w2, Map.union vp2 vp1))

data GYProposalProcedurePB = GYProposalProcedurePB
  { propProcPBReturnAddr :: !GYStakeAddress
  , propProcPBGovAction :: !GYGovAction
  , propProcPBAnchor :: !GYAnchor
  }
  deriving stock (Show, Eq, Ord)

data GYProposalProcedure = GYProposalProcedure
  { propProcDeposit :: !Natural
  , propProcReturnAddr :: !GYStakeAddress
  , propProcGovAction :: !GYGovAction
  , propProcAnchor :: !GYAnchor
  }
  deriving stock (Show, Eq, Ord)

completeProposalProcedure :: GYProposalProcedurePB -> Natural -> GYProposalProcedure
completeProposalProcedure (GYProposalProcedurePB {..}) dep =
  GYProposalProcedure
    { propProcDeposit = dep
    , propProcReturnAddr = propProcPBReturnAddr
    , propProcGovAction = propProcPBGovAction
    , propProcAnchor = propProcPBAnchor
    }

propProcToLedger :: GYProposalProcedure -> Ledger.ProposalProcedure Consensus.StandardConway
propProcToLedger (GYProposalProcedure {..}) =
  Ledger.ProposalProcedure
    { Ledger.pProcDeposit = fromIntegral propProcDeposit
    , Ledger.pProcReturnAddr = stakeAddressToLedger propProcReturnAddr
    , Ledger.pProcGovAction = govActionToLedger propProcGovAction
    , Ledger.pProcAnchor = anchorToLedger propProcAnchor
    }

data GYConstitution = GYConstitution
  { constitutionAnchor :: !GYAnchor
  , constitutionScript :: !(Maybe GYScriptHash)
  }
  deriving stock (Eq, Ord, Show)

constitutionToLedger :: GYConstitution -> Ledger.Constitution Consensus.StandardConway
constitutionToLedger (GYConstitution {..}) = Ledger.Constitution (anchorToLedger constitutionAnchor) (maybeToStrictMaybe $ scriptHashToLedger <$> constitutionScript)

data GYGovAction
  = ParameterChange
      -- | Previous governance action id of `ParameterChange` type.
      !(Maybe GYGovActionId)
      -- | Proposed changes to PParams
      !(Ledger.PParamsUpdate Consensus.StandardConway)
      -- | Policy hash protection
      !(Maybe GYScriptHash)
  | HardForkInitiation
      -- | Previous governance action id of `HardForkInitiation` type
      !(Maybe GYGovActionId)
      -- | Proposed new protocol version
      !ProtVer
  | TreasuryWithdrawals
      -- | Proposed treasury withdrawals
      !(Map GYStakeAddress Natural)
      -- | Policy hash protection
      !(Maybe GYScriptHash)
  | NoConfidence
      -- | Previous governance action id of `NoConfidence` or `UpdateCommittee` type
      !(Maybe GYGovActionId)
  | UpdateCommittee
      -- | Previous governance action id of `UpdateCommittee` or `NoConfidence` type
      !(Maybe GYGovActionId)
      -- | Constitutional Committe members to be removed
      !(Set (GYCredential 'GYKeyRoleColdCommittee))
      -- | Constitutional committee members to be added
      !(Map (GYCredential 'GYKeyRoleColdCommittee) GYEpochNo)
      -- | New Threshold
      !UnitInterval
  | NewConstitution
      -- | Previous governance action id of `NewConstitution` type
      !(Maybe GYGovActionId)
      !GYConstitution
  | InfoAction
  deriving stock (Eq, Show, Ord)

govActionToLedger :: GYGovAction -> Ledger.GovAction Consensus.StandardConway
govActionToLedger ga = case ga of
  ParameterChange mgaid ppup msh -> Ledger.ParameterChange (castPurposeM mgaid) ppup (castScriptHashM msh)
  HardForkInitiation mgaid pv -> Ledger.HardForkInitiation (castPurposeM mgaid) pv
  TreasuryWithdrawals tw msh -> Ledger.TreasuryWithdrawals (Map.mapKeys stakeAddressToLedger $ Map.map fromIntegral tw) (castScriptHashM msh)
  NoConfidence mgaid -> Ledger.NoConfidence (castPurposeM mgaid)
  UpdateCommittee mgaid rm add thr -> Ledger.UpdateCommittee (castPurposeM mgaid) (Set.map credentialToLedger rm) (Map.mapKeys credentialToLedger $ Map.map epochNoToLedger add) thr
  NewConstitution mgaid c -> Ledger.NewConstitution (castPurposeM mgaid) (constitutionToLedger c)
  InfoAction -> Ledger.InfoAction
 where
  ms = maybeToStrictMaybe

  castPurpose :: GYGovActionId -> Ledger.GovPurposeId p Consensus.StandardConway
  castPurpose = Ledger.GovPurposeId . govActionIdToLedger

  castPurposeM mgid = ms $ castPurpose <$> mgid

  castScriptHashM sh = ms $ scriptHashToLedger <$> sh
