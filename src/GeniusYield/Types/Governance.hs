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
) where

import Cardano.Api.Ledger (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import GeniusYield.Imports (Map, (&))
import GeniusYield.Types.Anchor
import GeniusYield.Types.BuildWitness
import GeniusYield.Types.Credential (GYCredential, credentialFromLedger, credentialToLedger)
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole (GYKeyRole (..))
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
