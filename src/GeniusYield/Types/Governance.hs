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
  voteFromPlutus,
  voteToPlutus,
  GYVoter (..),
  voterFromLedger,
  voterToLedger,
  GYGovActionId (..),
  govActionIdFromLedger,
  govActionIdToLedger,
  govActionIdFromPlutus,
  govActionIdToPlutus,
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
  propProcFromLedger,
  GYConstitution (..),
  constitutionToLedger,
  constitutionFromLedger,
  GYGovAction (..),
  govActionToLedger,
  govActionFromLedger,
  GYGovActionState (..),
  govActionStateToLedger,
  govActionStateFromLedger,
  GYGovState,
  govStateFromLedger,
  govStateToLedger,
) where

import Cardano.Api.Ledger (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Governance qualified as ConwayGovernance
import Cardano.Ledger.Conway.State qualified as ConwayState
import Data.Aeson (FromJSON, ToJSON, object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Either.Combinators (mapLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word16)
import GHC.Generics (Generic)
import GeniusYield.Imports (Map, Natural, Set, (&))
import GeniusYield.Types.Address (GYStakeAddress, stakeAddressFromLedger, stakeAddressToLedger)
import GeniusYield.Types.Anchor
import GeniusYield.Types.BuildWitness
import GeniusYield.Types.Credential (GYCredential, credentialFromLedger, credentialToLedger)
import GeniusYield.Types.Epoch (GYEpochNo, epochNoFromLedger, epochNoToLedger)
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole (GYKeyRole (..))
import GeniusYield.Types.Ledger (PlutusToCardanoError (UnknownPlutusToCardanoError))
import GeniusYield.Types.Reexpose (ProtVer, UnitInterval)
import GeniusYield.Types.Script (GYScriptHash, scriptHashFromLedger, scriptHashToLedger)
import GeniusYield.Types.Tx (GYTxId, txIdFromApi, txIdToApi)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))

-- | Vote on a governance proposal.
data GYVote = Yes | No | Abstain
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance FromJSON GYVote where
  parseJSON = withText "GYVote" $
    \case
      "yes" -> return Yes
      "no" -> return No
      "abstain" -> return Abstain
      _ -> fail "string is not one of known GYVote enum values"

instance ToJSON GYVote where
  toJSON Yes = "yes"
  toJSON No = "no"
  toJSON Abstain = "abstain"

voteToPlutus :: GYVote -> PlutusV3.Vote
voteToPlutus Yes = PlutusV3.VoteYes
voteToPlutus No = PlutusV3.VoteNo
voteToPlutus Abstain = PlutusV3.Abstain

voteFromPlutus :: PlutusV3.Vote -> GYVote
voteFromPlutus PlutusV3.VoteYes = Yes
voteFromPlutus PlutusV3.VoteNo = No
voteFromPlutus PlutusV3.Abstain = Abstain

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

voterToLedger :: GYVoter -> Ledger.Voter
voterToLedger (CommitteeVoter c) = Ledger.CommitteeVoter (credentialToLedger c)
voterToLedger (DRepVoter c) = Ledger.DRepVoter (credentialToLedger c)
voterToLedger (StakePoolVoter k) = Ledger.StakePoolVoter (keyHashToLedger k)

voterFromLedger :: Ledger.Voter -> GYVoter
voterFromLedger (Ledger.CommitteeVoter c) = CommitteeVoter (credentialFromLedger c)
voterFromLedger (Ledger.DRepVoter c) = DRepVoter (credentialFromLedger c)
voterFromLedger (Ledger.StakePoolVoter k) = StakePoolVoter (keyHashFromLedger k)

data GYGovActionId = GYGovActionId
  {gaidTxId :: !GYTxId, gaidIx :: !Word16}
  deriving (Eq, Show, Ord, Generic)

instance FromJSON GYGovActionId where
  parseJSON = withObject "GYGovActionId" $ \v -> do
    gaidTxId <- v .: "txId"
    gaidIx <- v .: "ix"
    return GYGovActionId {gaidTxId, gaidIx}

instance ToJSON GYGovActionId where
  toJSON (GYGovActionId {gaidTxId, gaidIx}) =
    object
      [ "txId" .= gaidTxId
      , "ix" .= gaidIx
      ]

govActionIdToLedger :: GYGovActionId -> Ledger.GovActionId
govActionIdToLedger (GYGovActionId txId ix) = Ledger.GovActionId (txIdToApi txId & Api.toShelleyTxId) (Ledger.GovActionIx ix)

govActionIdFromLedger :: Ledger.GovActionId -> GYGovActionId
govActionIdFromLedger (Ledger.GovActionId txId (Ledger.GovActionIx ix)) = GYGovActionId (txIdFromApi (Api.fromShelleyTxId txId)) ix

govActionIdFromPlutus :: PlutusV3.GovernanceActionId -> Either PlutusToCardanoError GYGovActionId
govActionIdFromPlutus (PlutusV3.GovernanceActionId tid@(PlutusV3.TxId (BuiltinByteString bs)) ix) = GYGovActionId <$> etid <*> eix
 where
  etid :: Either PlutusToCardanoError GYTxId
  etid =
    mapLeft (\e -> UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: invalid txOutRefId " <> show tid <> ", error: " <> show e) $
      txIdFromApi
        <$> Api.deserialiseFromRawBytes Api.AsTxId bs

  eix :: Either PlutusToCardanoError Word16
  eix
    | ix < 0 = Left $ UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: negative txOutRefIdx " ++ show ix
    | ix > toInteger (maxBound @Word) = Left $ UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: txOutRefIdx " ++ show ix ++ " too large"
    | otherwise = Right $ fromInteger ix

govActionIdToPlutus :: GYGovActionId -> PlutusV3.GovernanceActionId
govActionIdToPlutus (GYGovActionId tid ix) =
  PlutusV3.GovernanceActionId
    (PlutusV3.TxId . BuiltinByteString . Api.serialiseToRawBytes . txIdToApi $ tid)
    (toInteger ix)

-- | Voting procedure.
data GYVotingProcedure = GYVotingProcedure
  { vpVote :: !GYVote
  , vpAnchor :: !(Maybe GYAnchor)
  }
  deriving stock (Show, Eq, Ord)

votingProcedureToLedger :: GYVotingProcedure -> Ledger.VotingProcedure Conway.ConwayEra
votingProcedureToLedger (GYVotingProcedure v a) = Ledger.VotingProcedure (voteToLedger v) (maybeToStrictMaybe (anchorToLedger <$> a))

votingProcedureFromLedger :: Ledger.VotingProcedure Conway.ConwayEra -> GYVotingProcedure
votingProcedureFromLedger (Ledger.VotingProcedure v a) = GYVotingProcedure (voteFromLedger v) (strictMaybeToMaybe (anchorFromLedger <$> a))

type GYVotingProcedures = Map GYVoter (Map GYGovActionId GYVotingProcedure)

votingProceduresToLedger :: GYVotingProcedures -> Ledger.VotingProcedures Conway.ConwayEra
votingProceduresToLedger vp = Ledger.VotingProcedures $ Map.mapKeys voterToLedger $ Map.map (Map.mapKeys govActionIdToLedger . Map.map votingProcedureToLedger) vp

votingProceduresFromLedger :: Ledger.VotingProcedures Conway.ConwayEra -> GYVotingProcedures
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
completeProposalProcedure GYProposalProcedurePB {..} dep =
  GYProposalProcedure
    { propProcDeposit = dep
    , propProcReturnAddr = propProcPBReturnAddr
    , propProcGovAction = propProcPBGovAction
    , propProcAnchor = propProcPBAnchor
    }

propProcToLedger :: GYProposalProcedure -> Ledger.ProposalProcedure Conway.ConwayEra
propProcToLedger GYProposalProcedure {..} =
  Ledger.ProposalProcedure
    { Ledger.pProcDeposit = fromIntegral propProcDeposit
    , Ledger.pProcReturnAddr = stakeAddressToLedger propProcReturnAddr
    , Ledger.pProcGovAction = govActionToLedger propProcGovAction
    , Ledger.pProcAnchor = anchorToLedger propProcAnchor
    }

propProcFromLedger :: Ledger.ProposalProcedure Conway.ConwayEra -> GYProposalProcedure
propProcFromLedger Ledger.ProposalProcedure {..} =
  GYProposalProcedure
    { propProcDeposit = fromIntegral pProcDeposit
    , propProcReturnAddr = stakeAddressFromLedger pProcReturnAddr
    , propProcGovAction = govActionFromLedger pProcGovAction
    , propProcAnchor = anchorFromLedger pProcAnchor
    }

data GYConstitution = GYConstitution
  { constitutionAnchor :: !GYAnchor
  , constitutionScript :: !(Maybe GYScriptHash)
  }
  deriving stock (Eq, Ord, Show)

constitutionToLedger :: GYConstitution -> Ledger.Constitution Conway.ConwayEra
constitutionToLedger GYConstitution {..} = Ledger.Constitution (anchorToLedger constitutionAnchor) (maybeToStrictMaybe $ scriptHashToLedger <$> constitutionScript)

constitutionFromLedger :: Ledger.Constitution Conway.ConwayEra -> GYConstitution
constitutionFromLedger (Ledger.Constitution a s) = GYConstitution (anchorFromLedger a) (strictMaybeToMaybe $ scriptHashFromLedger <$> s)

data GYGovAction
  = ParameterChange
      -- | Previous governance action id of `ParameterChange` type.
      !(Maybe GYGovActionId)
      -- | Proposed changes to PParams
      !(Ledger.PParamsUpdate Conway.ConwayEra)
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
      -- | Constitutional committee members to be removed
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

govActionToLedger :: GYGovAction -> Ledger.GovAction Conway.ConwayEra
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

  castPurpose :: GYGovActionId -> Ledger.GovPurposeId p Conway.ConwayEra
  castPurpose = Ledger.GovPurposeId . govActionIdToLedger

  castPurposeM mgid = ms $ castPurpose <$> mgid

  castScriptHashM sh = ms $ scriptHashToLedger <$> sh

govActionFromLedger :: Ledger.GovAction Conway.ConwayEra -> GYGovAction
govActionFromLedger ga = case ga of
  Ledger.ParameterChange mgaid ppup msh -> ParameterChange (govActionIdFromLedger' <$> strictMaybeToMaybe mgaid) ppup (scriptHashFromLedger <$> strictMaybeToMaybe msh)
  Ledger.HardForkInitiation mgaid pv -> HardForkInitiation (govActionIdFromLedger' <$> strictMaybeToMaybe mgaid) pv
  Ledger.TreasuryWithdrawals tw msh -> TreasuryWithdrawals (Map.mapKeys stakeAddressFromLedger $ Map.map fromIntegral tw) (scriptHashFromLedger <$> strictMaybeToMaybe msh)
  Ledger.NoConfidence mgaid -> NoConfidence (govActionIdFromLedger' <$> strictMaybeToMaybe mgaid)
  Ledger.UpdateCommittee mgaid rm add thr -> UpdateCommittee (govActionIdFromLedger' <$> strictMaybeToMaybe mgaid) (Set.map credentialFromLedger rm) (Map.mapKeys credentialFromLedger $ Map.map epochNoFromLedger add) thr
  Ledger.NewConstitution mgaid c -> NewConstitution (govActionIdFromLedger' <$> strictMaybeToMaybe mgaid) (constitutionFromLedger c)
  Ledger.InfoAction -> InfoAction
 where
  govActionIdFromLedger' (Ledger.GovPurposeId gid) = govActionIdFromLedger gid

data GYGovActionState = GYGovActionState
  { gasId :: !GYGovActionId
  , gasCommitteeVotes :: !(Map (GYCredential 'GYKeyRoleHotCommittee) GYVote)
  , gasDRepVotes :: !(Map (GYCredential 'GYKeyRoleDRep) GYVote)
  , gasStakePoolVotes :: !(Map (GYKeyHash 'GYKeyRoleStakePool) GYVote)
  , gasProposalProcedure :: !GYProposalProcedure
  , gasProposedIn :: !GYEpochNo
  , gasExpiresAfter :: !GYEpochNo
  }
  deriving stock (Eq, Show, Ord)

govActionStateToLedger :: GYGovActionState -> Ledger.GovActionState Conway.ConwayEra
govActionStateToLedger GYGovActionState {..} =
  Ledger.GovActionState
    { Ledger.gasId = govActionIdToLedger gasId
    , Ledger.gasCommitteeVotes = Map.mapKeys credentialToLedger $ Map.map voteToLedger gasCommitteeVotes
    , Ledger.gasDRepVotes = Map.mapKeys credentialToLedger $ Map.map voteToLedger gasDRepVotes
    , Ledger.gasStakePoolVotes = Map.mapKeys keyHashToLedger $ Map.map voteToLedger gasStakePoolVotes
    , Ledger.gasProposalProcedure = propProcToLedger gasProposalProcedure
    , Ledger.gasProposedIn = epochNoToLedger gasProposedIn
    , Ledger.gasExpiresAfter = epochNoToLedger gasExpiresAfter
    }

govActionStateFromLedger :: Ledger.GovActionState Conway.ConwayEra -> GYGovActionState
govActionStateFromLedger Ledger.GovActionState {..} =
  GYGovActionState
    { gasId = govActionIdFromLedger gasId
    , gasCommitteeVotes = Map.mapKeys credentialFromLedger $ Map.map voteFromLedger gasCommitteeVotes
    , gasDRepVotes = Map.mapKeys credentialFromLedger $ Map.map voteFromLedger gasDRepVotes
    , gasStakePoolVotes = Map.mapKeys keyHashFromLedger $ Map.map voteFromLedger gasStakePoolVotes
    , gasProposalProcedure = propProcFromLedger gasProposalProcedure
    , gasProposedIn = epochNoFromLedger gasProposedIn
    , gasExpiresAfter = epochNoFromLedger gasExpiresAfter
    }

data GYGovState = GYGovState
  { cgsProposals :: !(ConwayGovernance.Proposals Conway.ConwayEra)
  , cgsCommittee :: !(Maybe (Ledger.Committee Conway.ConwayEra))
  , cgsConstitution :: !(Ledger.Constitution Conway.ConwayEra)
  , cgsCurPParams :: !(Ledger.PParams Conway.ConwayEra)
  , cgsPrevPParams :: !(Ledger.PParams Conway.ConwayEra)
  , cgsFuturePParams :: !(ConwayState.FuturePParams Conway.ConwayEra)
  , cgsDRepPulsingState :: !(ConwayGovernance.DRepPulsingState Conway.ConwayEra)
  }
  deriving stock (Eq, Show)

govStateToLedger :: GYGovState -> Ledger.GovState Conway.ConwayEra
govStateToLedger = undefined

govStateFromLedger :: Ledger.GovState Conway.ConwayEra -> GYGovState
govStateFromLedger Ledger.ConwayGovState {..} =
  GYGovState
    { cgsProposals = cgsProposals
    , cgsCommittee = strictMaybeToMaybe cgsCommittee
    , cgsConstitution = cgsConstitution
    , cgsCurPParams = cgsCurPParams
    , cgsPrevPParams = cgsPrevPParams
    , cgsFuturePParams = cgsFuturePParams
    , cgsDRepPulsingState = cgsDRepPulsingState
    }
