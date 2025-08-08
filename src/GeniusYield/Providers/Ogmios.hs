{- |
Module      : GeniusYield.Providers.Ogmios
Description : Ogmios provider for remote node connection.
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Ogmios (
  OgmiosApiEnv,
  newOgmiosApiEnv,
  OgmiosProviderException (..),
  ogmiosSubmitTx,
  ogmiosProtocolParameters,
  ogmiosGetSlotOfCurrentBlock,
  ogmiosStakePools,
  ogmiosGetDRepsState,
  ogmiosGetDRepState,
  ogmiosStakeAddressInfo,
  ogmiosStartTime,
  ogmiosEraSummaries,
  ogmiosConstitution,
  ogmiosProposals,
  ogmiosMempoolTxsWs,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Api.L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
 )
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Plutus qualified as Ledger
import Cardano.Slotting.Slot qualified as CSlot
import Cardano.Slotting.Time qualified as CTime
import Control.Monad ((<=<))
import Data.Aeson (Value (Null), object, withArray, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word16, Word32, Word64)
import Deriving.Aeson
import GHC.Int (Int64)
import GeniusYield.Imports
import GeniusYield.Providers.Common (
  SubmitTxException (..),
  newServantClientEnv,
  parseEraHist,
 )
import GeniusYield.Types hiding (poolId)
import Maestro.Types.V1 (AsAda (..), AsBytes, AsLovelace (..), CostModel, EpochNo, EpochSize, EpochSlotLength, EraBound, MaestroRational, MemoryCpuWith, MinFeeReferenceScripts, ProtocolParametersUpdateStakePool, ProtocolVersion)
import Maestro.Types.V1 qualified as Maestro
import Network.WebSockets qualified as WS
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Servant.API (
  JSON,
  Post,
  ReqBody,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  BaseUrl (..),
  ClientEnv,
  ClientError,
  ClientM,
  baseUrl,
  client,
  runClientM,
 )
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
-}

newtype OgmiosApiEnv = OgmiosApiEnv ClientEnv

ogmiosToClientEnv :: OgmiosApiEnv -> ClientEnv
ogmiosToClientEnv (OgmiosApiEnv cEnv) = cEnv

{- | Returns a new 'OgmiosApiEnv' given the base url to query from.

>>> env <- newOgmiosApiEnv "http://localhost:1337"
-}
newOgmiosApiEnv :: String -> IO OgmiosApiEnv
newOgmiosApiEnv baseUrl = OgmiosApiEnv <$> newServantClientEnv baseUrl

-- | Exceptions.
data OgmiosProviderException
  = -- | Error from the Ogmios API.
    OgmiosApiError !Text !ClientError
  | -- | Received error response.
    OgmiosErrorResponse !Text !Value
  | -- | The API returned an unexpected number of era summaries.
    OgmiosIncorrectEraHistoryLength ![EraSummary]
  | -- | Unable to deserialise transaction returned by Ogmios.
    OgmiosTransactionDeserialisationError
      -- | Transaction CBOR.
      !String
  | -- | Unable to decode response given by Ogmios under Websocket connection.
    OgmiosWebsocketDecodeError
      -- | Received response.
      !String
  deriving stock (Eq, Show)
  deriving anyclass Exception

{-# INLINEABLE runOgmiosClient #-}
runOgmiosClient :: OgmiosApiEnv -> ClientM a -> IO (Either ClientError a)
runOgmiosClient (OgmiosApiEnv cEnv) c = runClientM c cEnv

{-# INLINEABLE handleOgmiosError #-}
handleOgmiosError :: Text -> Either ClientError (OgmiosResponse a) -> IO a
handleOgmiosError locationInfo =
  either
    (throwIO . OgmiosApiError locationInfo)
    -- `OgmiosResponse` would likely be `Right` as in case of error, we are in `ClientError` case. We need to make use of something like `WithStatus` for `OgmiosErrorResponse` to be actually useful.
    (`reduceOgmiosResponse` (throwIO . OgmiosErrorResponse locationInfo))

{-# INLINEABLE reduceOgmiosResponse #-}
reduceOgmiosResponse :: Applicative f => OgmiosResponse a -> (Value -> f a) -> f a
reduceOgmiosResponse res e = case response res of
  Left err -> e err
  Right a -> pure a

class ToJSONRPC a where
  toMethod :: a -> Text
  toParams :: a -> Maybe Value

instance ToJSONRPC GYTx where
  toMethod = const "submitTransaction"
  toParams tx = Just $ toJSON $ Map.fromList [("transaction" :: Text, Map.fromList [("cbor" :: Text, txToHex tx)])]

newtype OgmiosRequest a = OgmiosRequest a

instance ToJSONRPC a => ToJSON (OgmiosRequest a) where
  toJSON (OgmiosRequest a) =
    object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= toMethod a
      , "params" .= toParams a
      ]

newtype OgmiosResponse a = OgmiosResponse
  { response :: Either Value a
  }
  deriving stock Show

instance FromJSON a => FromJSON (OgmiosResponse a) where
  parseJSON = withObject "OgmiosResponse" $ \o -> do
    result <- o .: "result"
    case result of
      Null -> OgmiosResponse . Left <$> o .: "error"
      _ -> OgmiosResponse . Right <$> parseJSON result

newtype TxIdResponse = TxIdResponse
  { txid :: GYTxId
  }
  deriving stock (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "tx"]] TxIdResponse

newtype TxSubmissionResponse = TxSubmissionResponse
  { transaction :: TxIdResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data OgmiosPP = OgmiosPP

instance ToJSONRPC OgmiosPP where
  toMethod = const "queryLedgerState/protocolParameters"
  toParams = const Nothing

data OgmiosTip = OgmiosTip

instance ToJSONRPC OgmiosTip where
  toMethod = const "queryLedgerState/tip"
  toParams = const Nothing

newtype OgmiosTipResponse = OgmiosTipResponse {slot :: GYSlot}
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data OgmiosStakePools = OgmiosStakePools

instance ToJSONRPC OgmiosStakePools where
  toMethod = const "queryLedgerState/stakePools"
  toParams = const Nothing

type OgmiosStakePoolsResponse = Map GYStakePoolIdBech32 Value

instance ToJSONRPC (Set.Set (GYCredential 'GYKeyRoleDRep)) where
  toMethod = const "queryLedgerState/delegateRepresentatives"
  toParams creds =
    let (scriptCreds, keyCreds) =
          Set.foldl'
            ( \(!scriptCredsAcc, !keyCredsAcc) -> \case
                GYCredentialByKey kh -> (scriptCredsAcc, kh : keyCredsAcc)
                GYCredentialByScript sh -> (sh : scriptCredsAcc, keyCredsAcc)
            )
            (mempty, mempty)
            creds
     in Just $ object ["scripts" .= scriptCreds, "keys" .= keyCreds]

newtype AsEpoch = AsEpoch
  { asEpochEpoch :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asEpoch", LowerFirst]] AsEpoch

epochFromOgmios :: AsEpoch -> GYEpochNo
epochFromOgmios asEpoch = asEpoch & asEpochEpoch & fromIntegral & GYEpochNo

data OgmiosMetadata = OgmiosMetadata
  { metadataUrl :: !GYUrl
  , metadataHash :: !GYAnchorDataHash
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "metadata", LowerFirst]] OgmiosMetadata

anchorFromOgmiosMetadata :: OgmiosMetadata -> GYAnchor
anchorFromOgmiosMetadata OgmiosMetadata {..} = GYAnchor metadataUrl metadataHash

data OgmiosDRepStateResponse = OgmiosDRepStateResponse
  { ogDRepStateDeposit :: !AsAda
  , ogDRepStateMandate :: !AsEpoch
  , ogDRepStateCred :: !(GYCredential 'GYKeyRoleDRep)
  , ogDRepStateDelegs :: !(Set (GYCredential 'GYKeyRoleStaking))
  , ogDRepStateAnchor :: !(Maybe OgmiosMetadata)
  }
  deriving stock Show

data OgCredType = OgCredTypeVerificationKey | OgCredTypeScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving FromJSON via CustomJSON '[ConstructorTagModifier '[StripPrefix "OgCredType", LowerFirst]] OgCredType

-- >>> Aeson.eitherDecode @(OgmiosResponse [OgmiosDRepStateResponse]) "{\"jsonrpc\":\"2.0\",\"method\":\"queryLedgerState/delegateRepresentatives\",\"result\":[{\"type\":\"registered\",\"from\":\"verificationKey\",\"id\":\"03ccae794affbe27a5f5f74da6266002db11daa6ae446aea783b972d\",\"mandate\":{\"epoch\":214},\"deposit\":{\"ada\":{\"lovelace\":500000000}},\"stake\":{\"ada\":{\"lovelace\":36452103822}},\"delegators\":[{\"from\":\"verificationKey\",\"credential\":\"053560b718fc0983281ac64fd56449b744a38b067bfccee6a4a2f403\"},{\"from\":\"verificationKey\",\"credential\":\"5a3f8c1b91adc65e2faf7fee12dcd9fce630703f99a09df6a331b9ea\"},{\"from\":\"verificationKey\",\"credential\":\"7c30635e2d876b90c1d505851683201081e122fd6ac665d0c36c8593\"},{\"from\":\"verificationKey\",\"credential\":\"9e22d4e534f80dfbd5d23ab6f21a53999f0a2e97f83926d56f9ec3eb\"},{\"from\":\"verificationKey\",\"credential\":\"bcac8a19f17801a28c2a352be936018ec96b67c147a5030218810896\"},{\"from\":\"verificationKey\",\"credential\":\"d0f4075a0ab29c0f71c8310f0b3151fbfee6fdc2de910e2f3471455a\"}]}]}"
-- Right (OgmiosResponse {response = Right [OgmiosDRepStateResponse {ogDRepStateDeposit = AsAda {asAdaAda = AsLovelace {asLovelaceLovelace = 500000000}}, ogDRepStateMandate = AsEpoch {asEpochEpoch = 214}, ogDRepStateCred = GYCredentialByKey (GYKeyHash (GYKeyRoleDRep) "03ccae794affbe27a5f5f74da6266002db11daa6ae446aea783b972d"), ogDRepStateDelegs = fromList [GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "053560b718fc0983281ac64fd56449b744a38b067bfccee6a4a2f403"),GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "5a3f8c1b91adc65e2faf7fee12dcd9fce630703f99a09df6a331b9ea"),GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "7c30635e2d876b90c1d505851683201081e122fd6ac665d0c36c8593"),GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "9e22d4e534f80dfbd5d23ab6f21a53999f0a2e97f83926d56f9ec3eb"),GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "bcac8a19f17801a28c2a352be936018ec96b67c147a5030218810896"),GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "d0f4075a0ab29c0f71c8310f0b3151fbfee6fdc2de910e2f3471455a")], ogDRepStateAnchor = Nothing}]})
instance {-# OVERLAPPING #-} FromJSON [OgmiosDRepStateResponse] where
  parseJSON = withArray "[OgmiosDRepStateResponse]" $ \arr -> do
    catMaybes <$> traverse parseDRepStateResponse (toList arr)
   where
    parseDRepStateResponse = withObject "OgmiosDRepStateResponse" $ \o -> do
      drepType :: String <- o .: "type"
      if drepType /= "registered"
        then pure Nothing
        else
          Just <$> do
            ogDRepStateDeposit <- o .: "deposit"
            ogDRepStateMandate <- o .: "mandate"
            credType <- o .: "from"
            ogDRepStateCred <- case credType of
              OgCredTypeVerificationKey -> GYCredentialByKey <$> o .: "id"
              OgCredTypeScript -> GYCredentialByScript <$> o .: "id"
            ogDRepStateDelegs <- do
              delegs <- o .: "delegators"
              Set.fromList
                <$> traverse
                  ( \d -> do
                      delegCredType <- d .: "from"
                      case delegCredType of
                        OgCredTypeVerificationKey -> GYCredentialByKey <$> d .: "credential"
                        OgCredTypeScript -> GYCredentialByScript <$> d .: "credential"
                  )
                  delegs
            ogDRepStateAnchor <- o .:? "metadata"
            pure $ OgmiosDRepStateResponse {..}

instance ToJSONRPC GYStakeAddress where
  toMethod = const "queryLedgerState/rewardAccountSummaries"
  toParams (stakeAddressToCredential -> sc) = Just $ case sc of
    GYCredentialByKey kh -> object ["keys" .= [kh]]
    GYCredentialByScript sh -> object ["scripts" .= [sh]]

newtype PoolId = PoolId
  { poolId :: GYStakePoolIdBech32
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "pool", LowerFirst]] PoolId
data OgmiosStakeAddressInfo = OgmiosStakeAddressInfo
  { delegate :: PoolId
  , rewards :: AsAda
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

data OgmiosStartTime = OgmiosStartTime
instance ToJSONRPC OgmiosStartTime where
  toMethod = const "queryNetwork/startTime"
  toParams = const Nothing

data OgmiosEraSummaries = OgmiosEraSummaries
instance ToJSONRPC OgmiosEraSummaries where
  toMethod = const "queryLedgerState/eraSummaries"
  toParams = const Nothing

data OgmiosConstitution = OgmiosConstitution
instance ToJSONRPC OgmiosConstitution where
  toMethod = const "queryLedgerState/constitution"
  toParams = const Nothing

newtype AsHash = AsHash
  { asHashHash :: GYScriptHash
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "asHash", LowerFirst]] AsHash

data OgmiosConstitutionResponse = OgmiosConstitutionResponse
  { metadata :: !OgmiosMetadata
  , guardrails :: !(Maybe AsHash)
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

constitutionFromOgmios :: OgmiosConstitutionResponse -> GYConstitution
constitutionFromOgmios OgmiosConstitutionResponse {..} = GYConstitution {constitutionAnchor = anchorFromOgmiosMetadata metadata, constitutionScript = asHashHash <$> guardrails}

newtype OgmiosProposals = OgmiosProposals (Set GYGovActionId)

instance ToJSONRPC OgmiosProposals where
  toMethod = const "queryLedgerState/governanceProposals"
  toParams (OgmiosProposals proposalsSet) = Just $ object ["proposals" .= Set.map encodeProposal proposalsSet]
   where
    encodeProposal govActionId =
      object ["transaction" .= object ["id" .= gaidTxId govActionId], "index" .= gaidIx govActionId]

data OgmiosProposalResponse = OgmiosProposalResponse
  { oprProposal :: GYGovActionId
  , oprDeposit :: AsAda
  , oprReturnAccount :: GYStakeAddressBech32
  , oprMetadata :: OgmiosMetadata
  , oprAction :: GYGovAction
  , oprSince :: AsEpoch
  , oprUntil :: AsEpoch
  , oprVotes :: [OgmiosVote]
  }

instance FromJSON OgmiosProposalResponse where
  parseJSON = withObject "OgmiosProposalResponse" $ \o -> do
    oprProposal <- do
      oproposal <- o .: "proposal"
      parseGovActionId oproposal
    oprDeposit <- o .: "deposit"
    oprReturnAccount <- o .: "returnAccount"
    oprMetadata <- o .: "metadata"
    oprAction <- do
      obj <- o .: "action"
      parseAction obj
    oprSince <- o .: "since"
    oprUntil <- o .: "until"
    oprVotes <- o .: "votes"
    pure OgmiosProposalResponse {..}
   where
    parseAction :: Aeson.Object -> Aeson.Parser GYGovAction
    parseAction obj = do
      actionType :: Text <- obj .: "type"
      case actionType of
        "information" -> pure InfoAction
        "noConfidence" -> do
          mancestor <- parseAncestor obj
          pure $ NoConfidence mancestor
        "constitution" -> do
          mancestor <- parseAncestor obj
          ogmiosConstitutionResp <- parseJSON @OgmiosConstitutionResponse (Aeson.Object obj)
          pure $ NewConstitution mancestor (constitutionFromOgmios ogmiosConstitutionResp)
        "constitutionalCommittee" -> do
          mancestor <- parseAncestor obj
          removedMembers <- obj .: "members" >>= (.: "removed")
          removedMembersSet :: Set (GYCredential 'GYKeyRoleColdCommittee) <- Set.fromList <$> traverse getCredential removedMembers
          addedMembers <- obj .: "members" >>= (.: "added")
          addedMembersMap :: Map (GYCredential 'GYKeyRoleColdCommittee) GYEpochNo <- Map.fromList <$> traverse (\o -> (,) <$> getCredential o <*> (o .: "mandate" >>= (.: "epoch") <&> GYEpochNo)) addedMembers
          quorum :: MaestroRational <- obj .: "quorum"
          let quorumUnitInterval = (\r -> fromMaybe (error $ "parseJSON (OgmiosProposalResponse): unable to bound rational " <> show r) r) $ Ledger.boundRational $ Maestro.unMaestroRational quorum
          pure $ UpdateCommittee mancestor removedMembersSet addedMembersMap quorumUnitInterval
        "treasuryWithdrawals" -> do
          withdrawals :: Map GYStakeAddressBech32 AsAda <- obj .: "withdrawals"
          guardrails :: Maybe AsHash <- obj .: "guardrails"
          pure $ TreasuryWithdrawals (Map.mapKeys stakeAddressFromBech32 $ Map.map (asLovelaceLovelace . asAdaAda) withdrawals) (asHashHash <$> guardrails)
        "hardForkInitiation" -> do
          mancestor <- parseAncestor obj
          version <- obj .: "version" >>= parseJSON @ProtocolVersion
          pure $ HardForkInitiation mancestor (protocolVersionFromOgmios "parseJSON (ogmiosProposalResponse)" version)
        "protocolParametersUpdate" -> do
          mancestor <- parseAncestor obj
          guardrails :: Maybe AsHash <- obj .: "guardrails"
          pparamsUpd :: ProtocolParametersM <- obj .: "parameters"
          pure $ ParameterChange mancestor (Ledger.PParamsUpdate $ pparamsFromOgmios "parseJSON (OgmiosProposalResponse)" pparamsUpd) (asHashHash <$> guardrails)
        anyOther -> fail $ "Invalid action type: " <> show anyOther
    parseAncestor obj = do
      ancestor <- obj .:? "ancestor"
      case ancestor of
        Nothing -> pure Nothing
        Just a -> do
          ancestorGovActionId <- parseGovActionId a
          pure $ Just ancestorGovActionId
    parseGovActionId :: Aeson.Object -> Aeson.Parser GYGovActionId
    parseGovActionId obj = do
      GYGovActionId
        <$> (obj .: "transaction" >>= (.: "id"))
        <*> (obj .: "index")

data OgmiosVoter = OgmiosVoterCommittee !(GYCredential 'GYKeyRoleHotCommittee) | OgmiosVoterDRep !(GYCredential 'GYKeyRoleDRep) | OgmiosVoterStakePool !GYStakePoolIdBech32

data OgmiosVote = OgmiosVote
  { ovVoter :: !OgmiosVoter
  , ovVote :: !GYVote
  }

instance FromJSON OgmiosVote where
  parseJSON =
    withObject "OgmiosVote" $
      \o -> do
        ovVoter <- do
          issuer <- o .: "issuer"
          voterRole :: Text <- issuer .: "role"
          case voterRole of
            "delegateRepresentative" -> do
              cred <- getCredential issuer
              pure $ OgmiosVoterDRep cred
            "constitutionalCommittee" -> do
              cred <- getCredential issuer
              pure $ OgmiosVoterCommittee cred
            "stakePoolOperator" -> do
              poolId <- issuer .: "id"
              pure $ OgmiosVoterStakePool poolId
            anyOther -> fail $ "Invalid voter role: " <> show anyOther
        voteResult :: Text <- o .: "vote"
        ovVote <- case voteResult of
          "yes" -> pure Yes
          "no" -> pure No
          "abstain" -> pure Abstain
          anyOther -> fail $ "Invalid vote result: " <> show anyOther
        pure OgmiosVote {..}

getCredential :: SingGYKeyRoleI kr => Aeson.Object -> Aeson.Parser (GYCredential kr)
getCredential o = do
  credType <- o .: "from"
  case credType of
    OgCredTypeVerificationKey -> GYCredentialByKey <$> o .: "id"
    OgCredTypeScript -> GYCredentialByScript <$> o .: "id"

govActionStateFromOgmiosProposalResponse :: OgmiosProposalResponse -> GYGovActionState
govActionStateFromOgmiosProposalResponse OgmiosProposalResponse {..} =
  let (gasStakePoolVotes, gasDRepVotes, gasCommitteeVotes) =
        foldl'
          ( \(!accGasStakePoolVotes, !accGasDRepVotes, !accGasCommitteeVotes) OgmiosVote {..} ->
              case ovVoter of
                OgmiosVoterCommittee cred -> (accGasStakePoolVotes, accGasDRepVotes, Map.insert cred ovVote accGasCommitteeVotes)
                OgmiosVoterDRep cred -> (accGasStakePoolVotes, Map.insert cred ovVote accGasDRepVotes, accGasCommitteeVotes)
                OgmiosVoterStakePool (stakePoolIdFromBech32 -> poolId) -> (Map.insert poolId ovVote accGasStakePoolVotes, accGasDRepVotes, accGasCommitteeVotes)
          )
          (mempty, mempty, mempty)
          oprVotes
      gasProposalProcedure = GYProposalProcedure {propProcDeposit = oprDeposit & asAdaAda & asLovelaceLovelace, propProcReturnAddr = oprReturnAccount & stakeAddressFromBech32, propProcGovAction = oprAction, propProcAnchor = oprMetadata & anchorFromOgmiosMetadata}
   in GYGovActionState {gasStakePoolVotes = gasStakePoolVotes, gasProposedIn = epochFromOgmios oprSince, gasProposalProcedure = gasProposalProcedure, gasId = oprProposal, gasExpiresAfter = epochFromOgmios oprUntil, gasDRepVotes = gasDRepVotes, gasCommitteeVotes = gasCommitteeVotes}

data OgmiosMempoolAcquire = OgmiosMempoolAcquire
data OgmiosMempoolNextTransaction = OgmiosMempoolNextTransaction
data OgmiosMempoolRelease = OgmiosMempoolRelease
instance ToJSONRPC OgmiosMempoolAcquire where
  toMethod = const "acquireMempool"
  toParams = const Nothing

instance ToJSONRPC OgmiosMempoolNextTransaction where
  toMethod = const "nextTransaction"
  toParams = const (Just $ object ["fields" .= ("all" :: Text)])

instance ToJSONRPC OgmiosMempoolRelease where
  toMethod = const "releaseMempool"
  toParams = const Nothing

data OgmiosMempoolAcquireResponse = OgmiosMempoolAcquireResponse
  { omarAcquired :: !Text
  , omarSlot :: !Natural
  }
  deriving stock (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "omar", LowerFirst]] OgmiosMempoolAcquireResponse

newtype OgmiosMempoolNextTransactionResponse = OgmiosMempoolNextTransactionResponse
  { omntrTransaction :: Maybe OgmiosTransactionDetails
  }
  deriving stock (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "omntr", LowerFirst]] OgmiosMempoolNextTransactionResponse

newtype OgmiosTransactionDetails = OgmiosTransactionDetails
  { otdCbor :: String
  }
  deriving stock (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "otd", LowerFirst]] OgmiosTransactionDetails

newtype OgmiosMempoolReleaseResponse = OgmiosMempoolReleaseResponse
  { omrrReleased :: Text
  }
  deriving stock (Show, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[StripPrefix "omrr", LowerFirst]] OgmiosMempoolReleaseResponse

submitTx :: OgmiosRequest GYTx -> ClientM (OgmiosResponse TxSubmissionResponse)
protocolParams :: OgmiosRequest OgmiosPP -> ClientM (OgmiosResponse ProtocolParameters)
tip :: OgmiosRequest OgmiosTip -> ClientM (OgmiosResponse OgmiosTipResponse)
stakePools :: OgmiosRequest OgmiosStakePools -> ClientM (OgmiosResponse OgmiosStakePoolsResponse)
drepState :: OgmiosRequest (Set.Set (GYCredential 'GYKeyRoleDRep)) -> ClientM (OgmiosResponse [OgmiosDRepStateResponse])
stakeAddressInfo :: OgmiosRequest GYStakeAddress -> ClientM (OgmiosResponse (Map Text OgmiosStakeAddressInfo))
startTime :: OgmiosRequest OgmiosStartTime -> ClientM (OgmiosResponse GYTime)
eraSummaries :: OgmiosRequest OgmiosEraSummaries -> ClientM (OgmiosResponse [EraSummary])
constitution :: OgmiosRequest OgmiosConstitution -> ClientM (OgmiosResponse OgmiosConstitutionResponse)
proposals :: OgmiosRequest OgmiosProposals -> ClientM (OgmiosResponse [OgmiosProposalResponse])

-- mempoolAcquire :: OgmiosRequest OgmiosMempoolAcquire -> ClientM (OgmiosResponse OgmiosMempoolAcquireResponse)
-- mempoolNextTransaction :: OgmiosRequest OgmiosMempoolNextTransaction -> ClientM (OgmiosResponse OgmiosMempoolNextTransactionResponse)
-- mempoolRelease :: OgmiosRequest OgmiosMempoolRelease -> ClientM (OgmiosResponse OgmiosMempoolReleaseResponse)

type OgmiosApi =
  ReqBody '[JSON] (OgmiosRequest GYTx) :> Post '[JSON] (OgmiosResponse TxSubmissionResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosPP) :> Post '[JSON] (OgmiosResponse ProtocolParameters)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosTip) :> Post '[JSON] (OgmiosResponse OgmiosTipResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosStakePools) :> Post '[JSON] (OgmiosResponse OgmiosStakePoolsResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest (Set.Set (GYCredential 'GYKeyRoleDRep))) :> Post '[JSON] (OgmiosResponse [OgmiosDRepStateResponse])
    :<|> ReqBody '[JSON] (OgmiosRequest GYStakeAddress) :> Post '[JSON] (OgmiosResponse (Map Text OgmiosStakeAddressInfo))
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosStartTime) :> Post '[JSON] (OgmiosResponse GYTime)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosEraSummaries) :> Post '[JSON] (OgmiosResponse [EraSummary])
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosConstitution) :> Post '[JSON] (OgmiosResponse OgmiosConstitutionResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosProposals) :> Post '[JSON] (OgmiosResponse [OgmiosProposalResponse])

-- :<|> ReqBody '[JSON] (OgmiosRequest OgmiosMempoolAcquire) :> Post '[JSON] (OgmiosResponse OgmiosMempoolAcquireResponse)
-- :<|> ReqBody '[JSON] (OgmiosRequest OgmiosMempoolNextTransaction) :> Post '[JSON] (OgmiosResponse OgmiosMempoolNextTransactionResponse)
-- :<|> ReqBody '[JSON] (OgmiosRequest OgmiosMempoolRelease) :> Post '[JSON] (OgmiosResponse OgmiosMempoolReleaseResponse)

submitTx
  :<|> protocolParams
  :<|> tip
  :<|> stakePools
  :<|> drepState
  :<|> stakeAddressInfo
  :<|> startTime
  :<|> eraSummaries
  :<|> constitution
  :<|> proposals =
    -- :<|> mempoolAcquire
    -- :<|> mempoolNextTransaction
    -- :<|> mempoolRelease =
    client @OgmiosApi Proxy

-- | Submit a transaction to the node via Ogmios.
ogmiosSubmitTx :: OgmiosApiEnv -> GYSubmitTx
ogmiosSubmitTx env tx = do
  TxSubmissionResponse (TxIdResponse txId) <-
    handleOgmiosSubmitError
      <=< runOgmiosClient env
      $ submitTx (OgmiosRequest tx)
  pure txId
 where
  handleOgmiosSubmitError = either submitE (`reduceOgmiosResponse` submitE)
  submitE :: Show a => a -> IO b
  submitE = throwIO . SubmitTxException . Text.pack . show

-- Luckily, for protocol parameters, types are similar to ones defined in Maestro's Haskell SDK. Most of the below types (related to protocol parameters) mimics the ones defined in Maestro's Haskell SDK.
data ProtocolParametersUpdateDRep = ProtocolParametersUpdateDRep
  { ppUpdateDrepEconomic :: !MaestroRational
  , ppUpdateDrepGovernance :: !MaestroRational
  , ppUpdateDrepNetwork :: !MaestroRational
  , ppUpdateDrepTechnical :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ppUpdateDrep", LowerFirst]] ProtocolParametersUpdateDRep

-- | DRep voting thresholds.
data DRepVotingThresholds = DRepVotingThresholds
  { drepVotingThresholdsConstitution :: !MaestroRational
  , drepVotingThresholdsConstitutionalCommittee :: !ConstitutionalCommittee
  , drepVotingThresholdsHardForkInitiation :: !MaestroRational
  , drepVotingThresholdsNoConfidence :: !MaestroRational
  , drepVotingThresholdsProtocolParametersUpdate :: !ProtocolParametersUpdateDRep
  , drepVotingThresholdsTreasuryWithdrawals :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "drepVotingThresholds", LowerFirst]] DRepVotingThresholds

data ConstitutionalCommittee = ConstitutionalCommittee
  { constitutionalCommitteeDefault :: !MaestroRational
  , constitutionalCommitteeStateOfNoConfidence :: !MaestroRational
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "constitutionalCommittee", LowerFirst]] ConstitutionalCommittee

-- | Stake pool voting thresholds.
data StakePoolVotingThresholds = StakePoolVotingThresholds
  { stakePoolVotingThresholdsConstitutionalCommittee :: !ConstitutionalCommittee
  , stakePoolVotingThresholdsHardForkInitiation :: !MaestroRational
  , stakePoolVotingThresholdsNoConfidence :: !MaestroRational
  , stakePoolVotingThresholdsProtocolParametersUpdate :: !ProtocolParametersUpdateStakePool
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "stakePoolVotingThresholds", LowerFirst]] StakePoolVotingThresholds

data CostModels = CostModels
  { costModelsPlutusV1 :: !CostModel
  , costModelsPlutusV2 :: !CostModel
  , costModelsPlutusV3 :: !CostModel
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "costModels", Rename "PlutusV1" "plutus:v1", Rename "PlutusV2" "plutus:v2", Rename "PlutusV3" "plutus:v3"]] CostModels

-- | Protocol parameters.
data ProtocolParametersHKD f = ProtocolParameters
  { protocolParametersCollateralPercentage :: !(HKD f Natural)
  , protocolParametersConstitutionalCommitteeMaxTermLength :: !(HKD f Natural)
  , protocolParametersConstitutionalCommitteeMinSize :: !(HKD f Natural)
  , protocolParametersDelegateRepresentativeDeposit :: !(HKD f AsAda)
  , protocolParametersDelegateRepresentativeMaxIdleTime :: !(HKD f Natural)
  , protocolParametersDelegateRepresentativeVotingThresholds :: !(HKD f DRepVotingThresholds)
  , protocolParametersDesiredNumberOfStakePools :: !(HKD f Natural)
  , protocolParametersGovernanceActionDeposit :: !(HKD f AsAda)
  , protocolParametersGovernanceActionLifetime :: !(HKD f Natural)
  , protocolParametersMaxBlockBodySize :: !(HKD f AsBytes)
  , protocolParametersMaxBlockHeaderSize :: !(HKD f AsBytes)
  , protocolParametersMaxCollateralInputs :: !(HKD f Natural)
  , protocolParametersMaxExecutionUnitsPerBlock :: !(HKD f (MemoryCpuWith Natural))
  , protocolParametersMaxExecutionUnitsPerTransaction :: !(HKD f (MemoryCpuWith Natural))
  , protocolParametersMaxReferenceScriptsSize :: !(HKD f AsBytes)
  , protocolParametersMaxTransactionSize :: !(HKD f AsBytes)
  , protocolParametersMaxValueSize :: !(HKD f AsBytes)
  , protocolParametersMinFeeCoefficient :: !(HKD f Natural)
  , protocolParametersMinFeeConstant :: !(HKD f AsAda)
  , protocolParametersMinFeeReferenceScripts :: !(HKD f MinFeeReferenceScripts)
  , protocolParametersMinStakePoolCost :: !(HKD f AsAda)
  , protocolParametersMinUtxoDepositCoefficient :: !(HKD f Natural)
  , protocolParametersMonetaryExpansion :: !(HKD f MaestroRational)
  , protocolParametersPlutusCostModels :: !(HKD f CostModels)
  , protocolParametersScriptExecutionPrices :: !(HKD f (MemoryCpuWith MaestroRational))
  , protocolParametersStakeCredentialDeposit :: !(HKD f AsAda)
  , protocolParametersStakePoolDeposit :: !(HKD f AsAda)
  , protocolParametersStakePoolPledgeInfluence :: !(HKD f MaestroRational)
  , protocolParametersStakePoolRetirementEpochBound :: !(HKD f EpochNo)
  , protocolParametersStakePoolVotingThresholds :: !(HKD f StakePoolVotingThresholds)
  , protocolParametersTreasuryExpansion :: !(HKD f MaestroRational)
  , protocolParametersVersion :: !(HKD f ProtocolVersion)
  }
  deriving stock Generic

type ProtocolParameters = ProtocolParametersHKD Identity

deriving via (CustomJSON '[FieldLabelModifier '[StripPrefix "protocolParameters", LowerFirst]] ProtocolParameters) instance FromJSON ProtocolParameters
deriving instance Eq ProtocolParameters
deriving instance Show ProtocolParameters

type ProtocolParametersM = ProtocolParametersHKD Ledger.StrictMaybe
deriving via (CustomJSON '[FieldLabelModifier '[StripPrefix "protocolParameters", LowerFirst]] ProtocolParametersM) instance FromJSON ProtocolParametersM
deriving instance Eq ProtocolParametersM
deriving instance Show ProtocolParametersM

protocolVersionFromOgmios :: String -> ProtocolVersion -> Ledger.ProtVer
protocolVersionFromOgmios errPath protocolParametersVersion =
  Ledger.ProtVer
    { Ledger.pvMajor = Ledger.mkVersion (Maestro.protocolVersionMajor protocolParametersVersion) & fromMaybe (error (errPath <> "Major version received from Maestro is out of bounds"))
    , Ledger.pvMinor = Maestro.protocolVersionMinor protocolParametersVersion
    }

pparamsFromOgmios :: forall f. HKDFunctor f => String -> ProtocolParametersHKD f -> ConwayPParams f Conway.ConwayEra
pparamsFromOgmios errPath ProtocolParameters {..} =
  ConwayPParams
    { cppMinFeeA = THKD $ hkdMap prxy (Ledger.Coin . (toInteger @Natural)) protocolParametersMinFeeCoefficient
    , cppMinFeeB = THKD $ hkdMap prxy (Ledger.Coin . toInteger . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersMinFeeConstant
    , cppMaxBBSize = THKD $ hkdMap prxy ((fromIntegral @Natural @Word32) . Maestro.asBytesBytes) protocolParametersMaxBlockBodySize
    , cppMaxTxSize = THKD $ hkdMap prxy ((fromIntegral @Natural @Word32) . Maestro.asBytesBytes) protocolParametersMaxTransactionSize
    , cppMaxBHSize = THKD $ hkdMap prxy (fromIntegral @Natural @Word16 . Maestro.asBytesBytes) protocolParametersMaxBlockHeaderSize
    , cppKeyDeposit = THKD $ hkdMap prxy (Ledger.Coin . toInteger . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersStakeCredentialDeposit
    , cppPoolDeposit = THKD $ hkdMap prxy (Ledger.Coin . toInteger . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersStakePoolDeposit
    , cppEMax =
        THKD $
          hkdMap
            prxy
            ( Ledger.EpochInterval
                . fromIntegral
                . Maestro.unEpochNo
            )
            protocolParametersStakePoolRetirementEpochBound
    , cppNOpt = THKD $ hkdMap prxy (fromIntegral @Natural @Word16) protocolParametersDesiredNumberOfStakePools
    , cppA0 = THKD $ hkdMap prxy (fromMaybe (error (errPath <> "Pool influence received from Maestro is out of bounds")) . Ledger.boundRational @Ledger.NonNegativeInterval . Maestro.unMaestroRational) protocolParametersStakePoolPledgeInfluence
    , cppRho = THKD $ hkdMap prxy (fromMaybe (error (errPath <> "Monetory expansion parameter received from Maestro is out of bounds")) . Ledger.boundRational @Ledger.UnitInterval . Maestro.unMaestroRational) protocolParametersMonetaryExpansion
    , cppTau = THKD $ hkdMap prxy (fromMaybe (error (errPath <> "Treasury expansion parameter received from Maestro is out of bounds")) . Ledger.boundRational @Ledger.UnitInterval . Maestro.unMaestroRational) protocolParametersTreasuryExpansion
    , cppProtocolVersion = toNoUpdate @f @Ledger.ProtVer $ hkdMap prxy (protocolVersionFromOgmios errPath) protocolParametersVersion
    , cppMinPoolCost = THKD $ hkdMap prxy (Ledger.Coin . toInteger . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersMinStakePoolCost
    , cppCoinsPerUTxOByte = THKD $ hkdMap prxy (Api.L.CoinPerByte . Ledger.Coin . toInteger @Natural) protocolParametersMinUtxoDepositCoefficient
    , cppCostModels =
        THKD $
          hkdMap
            prxy
            ( \ppPlutusCostModels ->
                Ledger.mkCostModels $
                  Map.fromList
                    [
                      ( Ledger.PlutusV1
                      , either (error (errPath <> "Couldn't build PlutusV1 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV1 $ coerce @_ @[Int64] (costModelsPlutusV1 ppPlutusCostModels)
                      )
                    ,
                      ( Ledger.PlutusV2
                      , either (error (errPath <> "Couldn't build PlutusV2 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV2 $ coerce @_ @[Int64] (costModelsPlutusV2 ppPlutusCostModels)
                      )
                    ,
                      ( Ledger.PlutusV3
                      , either (error (errPath <> "Couldn't build PlutusV3 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV3 $ coerce @_ @[Int64] (costModelsPlutusV3 ppPlutusCostModels)
                      )
                    ]
            )
            protocolParametersPlutusCostModels
    , cppPrices =
        THKD $
          hkdMap
            prxy
            ( \ppScriptExecutionPrices ->
                Ledger.Prices
                  { Ledger.prSteps = fromMaybe (error (errPath <> "Couldn't bound Maestro's cpu steps")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithCpu ppScriptExecutionPrices
                  , Ledger.prMem = fromMaybe (error (errPath <> "Couldn't bound Maestro's memory units")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithMemory ppScriptExecutionPrices
                  }
            )
            protocolParametersScriptExecutionPrices
    , cppMaxTxExUnits =
        THKD $
          hkdMap
            prxy
            ( \ppMaxExecutionUnitsPerTransaction ->
                Ledger.OrdExUnits $
                  Ledger.ExUnits
                    { Ledger.exUnitsSteps =
                        Maestro.memoryCpuWithCpu ppMaxExecutionUnitsPerTransaction
                    , Ledger.exUnitsMem =
                        Maestro.memoryCpuWithMemory ppMaxExecutionUnitsPerTransaction
                    }
            )
            protocolParametersMaxExecutionUnitsPerTransaction
    , cppMaxBlockExUnits =
        THKD $
          hkdMap
            prxy
            ( \ppMaxExecutionUnitsPerBlock ->
                Ledger.OrdExUnits $
                  Ledger.ExUnits
                    { Ledger.exUnitsSteps =
                        Maestro.memoryCpuWithCpu ppMaxExecutionUnitsPerBlock
                    , Ledger.exUnitsMem =
                        Maestro.memoryCpuWithMemory ppMaxExecutionUnitsPerBlock
                    }
            )
            protocolParametersMaxExecutionUnitsPerBlock
    , cppMaxValSize = THKD $ hkdMap prxy (fromIntegral @Natural @Word32 . Maestro.asBytesBytes) protocolParametersMaxValueSize
    , cppCollateralPercentage = THKD $ hkdMap prxy (fromIntegral @Natural @Word16) protocolParametersCollateralPercentage
    , cppMaxCollateralInputs = THKD $ hkdMap prxy (fromIntegral @Natural @Word16) protocolParametersMaxCollateralInputs
    , cppPoolVotingThresholds =
        THKD $
          hkdMap
            prxy
            ( \ppStakePoolVotingThresholds ->
                Ledger.PoolVotingThresholds
                  { pvtPPSecurityGroup = unsafeBoundRational $ Maestro.unMaestroRational $ Maestro.ppUpdateStakePoolSecurity $ stakePoolVotingThresholdsProtocolParametersUpdate ppStakePoolVotingThresholds
                  , pvtMotionNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ stakePoolVotingThresholdsNoConfidence ppStakePoolVotingThresholds
                  , pvtHardForkInitiation = unsafeBoundRational $ Maestro.unMaestroRational $ stakePoolVotingThresholdsHardForkInitiation ppStakePoolVotingThresholds
                  , pvtCommitteeNormal = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeDefault $ stakePoolVotingThresholdsConstitutionalCommittee ppStakePoolVotingThresholds
                  , pvtCommitteeNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeStateOfNoConfidence $ stakePoolVotingThresholdsConstitutionalCommittee ppStakePoolVotingThresholds
                  }
            )
            protocolParametersStakePoolVotingThresholds
    , cppDRepVotingThresholds =
        THKD $
          hkdMap
            prxy
            ( \ppDelegateRepresentativeVotingThresholds ->
                Ledger.DRepVotingThresholds
                  { dvtUpdateToConstitution = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsConstitution ppDelegateRepresentativeVotingThresholds
                  , dvtTreasuryWithdrawal = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsTreasuryWithdrawals ppDelegateRepresentativeVotingThresholds
                  , dvtPPTechnicalGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepTechnical $ drepVotingThresholdsProtocolParametersUpdate ppDelegateRepresentativeVotingThresholds
                  , dvtPPNetworkGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepNetwork $ drepVotingThresholdsProtocolParametersUpdate ppDelegateRepresentativeVotingThresholds
                  , dvtPPGovGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepGovernance $ drepVotingThresholdsProtocolParametersUpdate ppDelegateRepresentativeVotingThresholds
                  , dvtPPEconomicGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepEconomic $ drepVotingThresholdsProtocolParametersUpdate ppDelegateRepresentativeVotingThresholds
                  , dvtMotionNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsNoConfidence ppDelegateRepresentativeVotingThresholds
                  , dvtHardForkInitiation = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsHardForkInitiation ppDelegateRepresentativeVotingThresholds
                  , dvtCommitteeNormal = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeDefault $ drepVotingThresholdsConstitutionalCommittee ppDelegateRepresentativeVotingThresholds
                  , dvtCommitteeNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeStateOfNoConfidence $ drepVotingThresholdsConstitutionalCommittee ppDelegateRepresentativeVotingThresholds
                  }
            )
            protocolParametersDelegateRepresentativeVotingThresholds
    , cppCommitteeMinSize = THKD $ hkdMap prxy (fromIntegral @Natural @Word16) protocolParametersConstitutionalCommitteeMinSize
    , cppCommitteeMaxTermLength = THKD $ hkdMap prxy (Ledger.EpochInterval . fromIntegral @Natural) protocolParametersConstitutionalCommitteeMaxTermLength
    , cppGovActionLifetime = THKD $ hkdMap prxy (Ledger.EpochInterval . fromIntegral @Natural) protocolParametersGovernanceActionLifetime
    , cppGovActionDeposit = THKD $ hkdMap prxy (Ledger.Coin . fromIntegral . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersGovernanceActionDeposit
    , cppDRepDeposit = THKD $ hkdMap prxy (Ledger.Coin . fromIntegral . Maestro.asLovelaceLovelace . Maestro.asAdaAda) protocolParametersDelegateRepresentativeDeposit
    , cppDRepActivity = THKD $ hkdMap prxy (Ledger.EpochInterval . fromIntegral @Natural) protocolParametersDelegateRepresentativeMaxIdleTime
    , cppMinFeeRefScriptCostPerByte = THKD $ hkdMap prxy (unsafeBoundRational @Ledger.NonNegativeInterval . Maestro.minFeeReferenceScriptsBase) protocolParametersMinFeeReferenceScripts
    }
 where
  prxy = Proxy @f

-- | Fetch protocol parameters.
ogmiosProtocolParameters :: OgmiosApiEnv -> IO ApiProtocolParameters
ogmiosProtocolParameters env = do
  ogmiosPParams <-
    handleOgmiosError fn
      <=< runOgmiosClient env
      $ protocolParams (OgmiosRequest OgmiosPP)
  pure $
    Ledger.PParams $
      pparamsFromOgmios errPath ogmiosPParams
 where
  errPath = "GeniusYield.Providers.Ogmios.ogmiosProtocolParameters: "
  fn = "ogmiosProtocolParameters"

-- | Get slot of current block.
ogmiosGetSlotOfCurrentBlock :: OgmiosApiEnv -> IO GYSlot
ogmiosGetSlotOfCurrentBlock env = do
  OgmiosTipResponse s <-
    handleOgmiosError fn
      <=< runOgmiosClient env
      $ tip (OgmiosRequest OgmiosTip)
  pure s
 where
  fn = "ogmiosGetSlotOfCurrentBlock"

ogmiosStakePools :: OgmiosApiEnv -> IO (Set.Set Api.S.PoolId)
ogmiosStakePools env = do
  sps <- handleOgmiosError fn <=< runOgmiosClient env $ stakePools (OgmiosRequest OgmiosStakePools)
  pure $ Set.map (stakePoolIdToApi . stakePoolIdFromBech32) $ Map.keysSet sps
 where
  fn = "ogmiosStakePools"

ogmiosGetDRepsState :: OgmiosApiEnv -> Set.Set (GYCredential 'GYKeyRoleDRep) -> IO (Map.Map (GYCredential 'GYKeyRoleDRep) (Maybe GYDRepState))
ogmiosGetDRepsState env dreps = do
  drepStates <- handleOgmiosError fn <=< runOgmiosClient env $ drepState (OgmiosRequest dreps)
  let foundStates =
        Map.fromList $
          map
            ( \s ->
                ( ogDRepStateCred s
                , Just $
                    GYDRepState
                      { drepExpiry = ogDRepStateMandate s & epochFromOgmios
                      , drepAnchor =
                          let man = ogDRepStateAnchor s
                           in man >>= \an -> Just $ anchorFromOgmiosMetadata an
                      , drepDeposit = ogDRepStateDeposit s & asAdaAda & asLovelaceLovelace
                      , drepDelegs = ogDRepStateDelegs s
                      }
                )
            )
            drepStates
      filteredFoundStates = Map.restrictKeys foundStates dreps
  pure $ Set.foldl' (\mapAcc drep -> if Map.member drep mapAcc then mapAcc else Map.insert drep Nothing mapAcc) filteredFoundStates dreps
 where
  fn = "ogmiosGetDRepsState"

ogmiosGetDRepState :: OgmiosApiEnv -> GYCredential 'GYKeyRoleDRep -> IO (Maybe GYDRepState)
ogmiosGetDRepState env drep = do
  drepStates <- ogmiosGetDRepsState env $ Set.singleton drep
  pure $ join $ Map.lookup drep drepStates

ogmiosStakeAddressInfo :: OgmiosApiEnv -> GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
ogmiosStakeAddressInfo env addr = do
  mstakeAddressInfo <- handleOgmiosError fn <=< runOgmiosClient env $ stakeAddressInfo (OgmiosRequest addr)
  pure $ listToMaybe $ map (\OgmiosStakeAddressInfo {..} -> GYStakeAddressInfo {gyStakeAddressInfoDelegatedPool = delegate & poolId & stakePoolIdFromBech32 & Just, gyStakeAddressInfoAvailableRewards = asAdaAda rewards & asLovelaceLovelace}) $ Map.elems mstakeAddressInfo
 where
  fn = "ogmiosStakeAddressInfo"

ogmiosStartTime :: OgmiosApiEnv -> IO CTime.SystemStart
ogmiosStartTime env = do
  gytime <- handleOgmiosError fn <=< runOgmiosClient env $ startTime (OgmiosRequest OgmiosStartTime)
  pure $ CTime.SystemStart $ posixSecondsToUTCTime $ timeToPOSIX gytime
 where
  fn = "ogmiosStartTime"

data EraParameters = EraParameters
  { eraParametersEpochLength :: !EpochSize
  , eraParametersSlotLength :: !EpochSlotLength
  , eraParametersSafeZone :: !(Maybe Word64)
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraParameters", LowerFirst]] EraParameters

data EraSummary = EraSummary
  { eraSummaryStart :: !EraBound
  -- ^ Start of this era.
  , eraSummaryEnd :: !(Maybe EraBound)
  -- ^ End of this era.
  , eraSummaryParameters :: !EraParameters
  -- ^ Parameters of this era.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "eraSummary", LowerFirst]] EraSummary

-- Largely similar to how we handle for Maestro.
ogmiosEraSummaries :: OgmiosApiEnv -> IO Api.EraHistory
ogmiosEraSummaries env = do
  eraSumms <- handleOgmiosError fn <=< runOgmiosClient env $ eraSummaries (OgmiosRequest OgmiosEraSummaries)
  maybe (throwIO $ OgmiosIncorrectEraHistoryLength eraSumms) pure $ parseEraHist mkEra eraSumms
 where
  mkBound Maestro.EraBound {eraBoundEpoch, eraBoundSlot, eraBoundTime} =
    Ouroboros.Bound
      { boundTime = CTime.RelativeTime $ Maestro.eraBoundTimeSeconds eraBoundTime
      , boundSlot = CSlot.SlotNo $ fromIntegral eraBoundSlot
      , boundEpoch = CSlot.EpochNo $ fromIntegral eraBoundEpoch
      }
  mkEraParams EraParameters {eraParametersEpochLength, eraParametersSlotLength, eraParametersSafeZone} =
    Ouroboros.EraParams
      { eraEpochSize = CSlot.EpochSize $ fromIntegral eraParametersEpochLength
      , eraSlotLength = CTime.mkSlotLength $ Maestro.epochSlotLengthMilliseconds eraParametersSlotLength / 1000
      , eraSafeZone = Ouroboros.StandardSafeZone $ fromJust eraParametersSafeZone
      , eraGenesisWin = fromIntegral $ fromJust eraParametersSafeZone -- TODO: Get it from provider? It is supposed to be 3k/f where k is security parameter (at present 2160) and f is active slot coefficient. Usually ledger set the safe zone size such that it guarantees at least k blocks...
      }
  mkEra EraSummary {eraSummaryStart, eraSummaryEnd, eraSummaryParameters} =
    Ouroboros.EraSummary
      { eraStart = mkBound eraSummaryStart
      , eraEnd = maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . mkBound) eraSummaryEnd
      , eraParams = mkEraParams eraSummaryParameters
      }
  fn = "ogmiosEraSummaries"

ogmiosConstitution :: OgmiosApiEnv -> IO GYConstitution
ogmiosConstitution env = do
  ogmiosConstitutionResp <- handleOgmiosError fn <=< runOgmiosClient env $ constitution (OgmiosRequest OgmiosConstitution)
  pure $ constitutionFromOgmios ogmiosConstitutionResp
 where
  fn = "ogmiosConstitution"

ogmiosProposals :: OgmiosApiEnv -> Set GYGovActionId -> IO (Seq.Seq GYGovActionState)
ogmiosProposals env actionIds = do
  proposalsResp <- handleOgmiosError fn <=< runOgmiosClient env $ proposals (OgmiosRequest $ OgmiosProposals actionIds)
  pure $ Seq.fromList $ map govActionStateFromOgmiosProposalResponse proposalsResp
 where
  fn = "ogmiosProposals"

processWSResponse :: FromJSON a => Text -> Text -> IO a
processWSResponse fn msg = do
  case Aeson.eitherDecode (BSL.fromStrict $ TE.encodeUtf8 msg) of
    Left err -> throwIO . OgmiosWebsocketDecodeError $ err
    Right (ogresponse :: OgmiosResponse a) -> ogresponse `reduceOgmiosResponse` (throwIO . OgmiosErrorResponse fn)

ogmiosMempoolTxsWs :: OgmiosApiEnv -> IO [GYTx]
ogmiosMempoolTxsWs (baseUrl . ogmiosToClientEnv -> BaseUrl {..}) = WS.runClient baseUrlHost baseUrlPort baseUrlPath $ \conn -> do
  rpc conn $ OgmiosRequest OgmiosMempoolAcquire
  msgAcq <- WS.receiveData conn
  void $ processWSResponse @OgmiosMempoolAcquireResponse fn msgAcq
  txs <- go conn []
  rpc conn $ OgmiosRequest OgmiosMempoolRelease
  msgRel <- WS.receiveData conn
  void $ processWSResponse @OgmiosMempoolReleaseResponse fn msgRel
  pure txs
 where
  rpc :: ToJSONRPC a => WS.Connection -> OgmiosRequest a -> IO ()
  rpc conn toSend = do
    WS.sendTextData conn $ TE.decodeUtf8 $ BSL.toStrict $ Aeson.encode toSend
  go conn acc = do
    rpc conn $ OgmiosRequest OgmiosMempoolNextTransaction
    msg <- WS.receiveData conn
    OgmiosMempoolNextTransactionResponse {omntrTransaction} <- processWSResponse @OgmiosMempoolNextTransactionResponse fn msg
    case omntrTransaction of
      Nothing -> pure $ reverse acc
      Just OgmiosTransactionDetails {otdCbor} -> case txFromHex otdCbor of
        Nothing -> throwIO $ OgmiosTransactionDeserialisationError otdCbor
        Just tx -> go conn (tx : acc)
  fn = "ogmiosMempoolTxs"

-- For some reason, it doesn't work this way.
-- ogmiosMempoolTxs :: OgmiosApiEnv -> IO [GYTx]
-- ogmiosMempoolTxs env = do
--   _ <-
--     handleOgmiosError fn
--       <=< runOgmiosClient env
--       $ mempoolAcquire (OgmiosRequest OgmiosMempoolAcquire)
--   txs <- go []
--   _ <- handleOgmiosError fn <=< runOgmiosClient env $ mempoolRelease (OgmiosRequest OgmiosMempoolRelease)
--   pure txs
--  where
--   fn = "ogmiosMempoolTxs"
--   go acc = do
--     OgmiosMempoolNextTransactionResponse {omntrTransaction} <- handleOgmiosError fn <=< runOgmiosClient env $ mempoolNextTransaction (OgmiosRequest OgmiosMempoolNextTransaction)
--     case omntrTransaction of
--       Nothing -> pure $ reverse acc
--       Just OgmiosTransactionDetails {otdCbor} -> case txFromHex otdCbor of
--         Nothing -> throwIO $ OgmiosTransactionDeserialisationError otdCbor
--         Just tx -> go (tx : acc)
