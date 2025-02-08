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
) where

import Cardano.Api.Ledger qualified as Api.L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
 )
import Cardano.Ledger.Plutus qualified as Ledger
import Control.Monad ((<=<))
import Data.Aeson (Value (Null), object, withArray, withObject, (.:), (.:?), (.=))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Deriving.Aeson
import GHC.Int (Int64)
import GeniusYield.Imports
import GeniusYield.Providers.Common (
  SubmitTxException (..),
  newServantClientEnv,
 )
import GeniusYield.Types
import Maestro.Types.V1 (AsAda (..), AsBytes, AsLovelace (..), CostModel, EpochNo, LowerFirst, MaestroRational, MemoryCpuWith, MinFeeReferenceScripts, ProtocolParametersUpdateStakePool, ProtocolVersion)
import Maestro.Types.V1 qualified as Maestro
import Servant.API (
  JSON,
  Post,
  ReqBody,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  ClientEnv,
  ClientError,
  ClientM,
  client,
  runClientM,
 )
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
-}

newtype OgmiosApiEnv = OgmiosApiEnv ClientEnv

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

data OgmiosMetadata = OgmiosMetadata
  { metadataUrl :: !GYUrl
  , metadataHash :: !GYAnchorDataHash
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "metadata", LowerFirst]] OgmiosMetadata

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

submitTx :: OgmiosRequest GYTx -> ClientM (OgmiosResponse TxSubmissionResponse)
protocolParams :: OgmiosRequest OgmiosPP -> ClientM (OgmiosResponse ProtocolParameters)
tip :: OgmiosRequest OgmiosTip -> ClientM (OgmiosResponse OgmiosTipResponse)
stakePools :: OgmiosRequest OgmiosStakePools -> ClientM (OgmiosResponse OgmiosStakePoolsResponse)
drepState :: OgmiosRequest (Set.Set (GYCredential 'GYKeyRoleDRep)) -> ClientM (OgmiosResponse [OgmiosDRepStateResponse])

type OgmiosApi =
  ReqBody '[JSON] (OgmiosRequest GYTx) :> Post '[JSON] (OgmiosResponse TxSubmissionResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosPP) :> Post '[JSON] (OgmiosResponse ProtocolParameters)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosTip) :> Post '[JSON] (OgmiosResponse OgmiosTipResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest OgmiosStakePools) :> Post '[JSON] (OgmiosResponse OgmiosStakePoolsResponse)
    :<|> ReqBody '[JSON] (OgmiosRequest (Set.Set (GYCredential 'GYKeyRoleDRep))) :> Post '[JSON] (OgmiosResponse [OgmiosDRepStateResponse])

submitTx :<|> protocolParams :<|> tip :<|> stakePools :<|> drepState = client @OgmiosApi Proxy

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
data ProtocolParameters = ProtocolParameters
  { protocolParametersCollateralPercentage :: !Natural
  , protocolParametersConstitutionalCommitteeMaxTermLength :: !Natural
  , protocolParametersConstitutionalCommitteeMinSize :: !Natural
  , protocolParametersDelegateRepresentativeDeposit :: !AsAda
  , protocolParametersDelegateRepresentativeMaxIdleTime :: !Natural
  , protocolParametersDelegateRepresentativeVotingThresholds :: !DRepVotingThresholds
  , protocolParametersDesiredNumberOfStakePools :: !Natural
  , protocolParametersGovernanceActionDeposit :: !AsAda
  , protocolParametersGovernanceActionLifetime :: !Natural
  , protocolParametersMaxBlockBodySize :: !AsBytes
  , protocolParametersMaxBlockHeaderSize :: !AsBytes
  , protocolParametersMaxCollateralInputs :: !Natural
  , protocolParametersMaxExecutionUnitsPerBlock :: !(MemoryCpuWith Natural)
  , protocolParametersMaxExecutionUnitsPerTransaction :: !(MemoryCpuWith Natural)
  , protocolParametersMaxReferenceScriptsSize :: !AsBytes
  , protocolParametersMaxTransactionSize :: !AsBytes
  , protocolParametersMaxValueSize :: !AsBytes
  , protocolParametersMinFeeCoefficient :: !Natural
  , protocolParametersMinFeeConstant :: !AsAda
  , protocolParametersMinFeeReferenceScripts :: !MinFeeReferenceScripts
  , protocolParametersMinStakePoolCost :: !AsAda
  , protocolParametersMinUtxoDepositCoefficient :: !Natural
  , protocolParametersMonetaryExpansion :: !MaestroRational
  , protocolParametersPlutusCostModels :: !CostModels
  , protocolParametersScriptExecutionPrices :: !(MemoryCpuWith MaestroRational)
  , protocolParametersStakeCredentialDeposit :: !AsAda
  , protocolParametersStakePoolDeposit :: !AsAda
  , protocolParametersStakePoolPledgeInfluence :: !MaestroRational
  , protocolParametersStakePoolRetirementEpochBound :: !EpochNo
  , protocolParametersStakePoolVotingThresholds :: !StakePoolVotingThresholds
  , protocolParametersTreasuryExpansion :: !MaestroRational
  , protocolParametersVersion :: !ProtocolVersion
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "protocolParameters", LowerFirst]] ProtocolParameters

-- | Fetch protocol parameters.
ogmiosProtocolParameters :: OgmiosApiEnv -> IO ApiProtocolParameters
ogmiosProtocolParameters env = do
  ProtocolParameters {..} <-
    handleOgmiosError fn
      <=< runOgmiosClient env
      $ protocolParams (OgmiosRequest OgmiosPP)
  pure $
    Ledger.PParams $
      ConwayPParams
        { cppMinFeeA = THKD $ Ledger.Coin $ toInteger protocolParametersMinFeeCoefficient
        , cppMinFeeB = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersMinFeeConstant
        , cppMaxBBSize = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxBlockBodySize
        , cppMaxTxSize = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxTransactionSize
        , cppMaxBHSize = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxBlockHeaderSize
        , cppKeyDeposit = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersStakeCredentialDeposit
        , cppPoolDeposit = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersStakePoolDeposit
        , cppEMax =
            THKD $
              Ledger.EpochInterval . fromIntegral $
                Maestro.unEpochNo protocolParametersStakePoolRetirementEpochBound
        , cppNOpt = THKD $ fromIntegral protocolParametersDesiredNumberOfStakePools
        , cppA0 = THKD $ fromMaybe (error (errPath <> "Pool influence received from Maestro is out of bounds")) $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersStakePoolPledgeInfluence
        , cppRho = THKD $ fromMaybe (error (errPath <> "Monetory expansion parameter received from Maestro is out of bounds")) $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersMonetaryExpansion
        , cppTau = THKD $ fromMaybe (error (errPath <> "Treasury expansion parameter received from Maestro is out of bounds")) $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersTreasuryExpansion
        , cppProtocolVersion =
            Ledger.ProtVer
              { Ledger.pvMajor = Ledger.mkVersion (Maestro.protocolVersionMajor protocolParametersVersion) & fromMaybe (error (errPath <> "Major version received from Maestro is out of bounds"))
              , Ledger.pvMinor = Maestro.protocolVersionMinor protocolParametersVersion
              }
        , cppMinPoolCost = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersMinStakePoolCost
        , cppCoinsPerUTxOByte = THKD $ Api.L.CoinPerByte $ Ledger.Coin $ toInteger protocolParametersMinUtxoDepositCoefficient
        , cppCostModels =
            THKD $
              Ledger.mkCostModels $
                Map.fromList
                  [
                    ( Ledger.PlutusV1
                    , either (error (errPath <> "Couldn't build PlutusV1 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV1 $ coerce @_ @[Int64] (costModelsPlutusV1 protocolParametersPlutusCostModels)
                    )
                  ,
                    ( Ledger.PlutusV2
                    , either (error (errPath <> "Couldn't build PlutusV2 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV2 $ coerce @_ @[Int64] (costModelsPlutusV2 protocolParametersPlutusCostModels)
                    )
                  ,
                    ( Ledger.PlutusV3
                    , either (error (errPath <> "Couldn't build PlutusV3 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV3 $ coerce @_ @[Int64] (costModelsPlutusV3 protocolParametersPlutusCostModels)
                    )
                  ]
        , cppPrices = THKD $ Ledger.Prices {Ledger.prSteps = fromMaybe (error (errPath <> "Couldn't bound Maestro's cpu steps")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithCpu protocolParametersScriptExecutionPrices, Ledger.prMem = fromMaybe (error (errPath <> "Couldn't bound Maestro's memory units")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithMemory protocolParametersScriptExecutionPrices}
        , cppMaxTxExUnits =
            THKD $
              Ledger.OrdExUnits $
                Ledger.ExUnits
                  { Ledger.exUnitsSteps =
                      Maestro.memoryCpuWithCpu protocolParametersMaxExecutionUnitsPerTransaction
                  , Ledger.exUnitsMem =
                      Maestro.memoryCpuWithMemory protocolParametersMaxExecutionUnitsPerTransaction
                  }
        , cppMaxBlockExUnits =
            THKD $
              Ledger.OrdExUnits $
                Ledger.ExUnits
                  { Ledger.exUnitsSteps =
                      Maestro.memoryCpuWithCpu protocolParametersMaxExecutionUnitsPerBlock
                  , Ledger.exUnitsMem =
                      Maestro.memoryCpuWithMemory protocolParametersMaxExecutionUnitsPerBlock
                  }
        , cppMaxValSize = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxValueSize
        , cppCollateralPercentage = THKD $ fromIntegral protocolParametersCollateralPercentage
        , cppMaxCollateralInputs = THKD $ fromIntegral protocolParametersMaxCollateralInputs
        , cppPoolVotingThresholds =
            THKD $
              Ledger.PoolVotingThresholds
                { pvtPPSecurityGroup = unsafeBoundRational $ Maestro.unMaestroRational $ Maestro.ppUpdateStakePoolSecurity $ stakePoolVotingThresholdsProtocolParametersUpdate protocolParametersStakePoolVotingThresholds
                , pvtMotionNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ stakePoolVotingThresholdsNoConfidence protocolParametersStakePoolVotingThresholds
                , pvtHardForkInitiation = unsafeBoundRational $ Maestro.unMaestroRational $ stakePoolVotingThresholdsHardForkInitiation protocolParametersStakePoolVotingThresholds
                , pvtCommitteeNormal = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeDefault $ stakePoolVotingThresholdsConstitutionalCommittee protocolParametersStakePoolVotingThresholds
                , pvtCommitteeNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeStateOfNoConfidence $ stakePoolVotingThresholdsConstitutionalCommittee protocolParametersStakePoolVotingThresholds
                }
        , cppDRepVotingThresholds =
            THKD $
              Ledger.DRepVotingThresholds
                { dvtUpdateToConstitution = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsConstitution protocolParametersDelegateRepresentativeVotingThresholds
                , dvtTreasuryWithdrawal = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsTreasuryWithdrawals protocolParametersDelegateRepresentativeVotingThresholds
                , dvtPPTechnicalGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepTechnical $ drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
                , dvtPPNetworkGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepNetwork $ drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
                , dvtPPGovGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepGovernance $ drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
                , dvtPPEconomicGroup = unsafeBoundRational $ Maestro.unMaestroRational $ ppUpdateDrepEconomic $ drepVotingThresholdsProtocolParametersUpdate protocolParametersDelegateRepresentativeVotingThresholds
                , dvtMotionNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsNoConfidence protocolParametersDelegateRepresentativeVotingThresholds
                , dvtHardForkInitiation = unsafeBoundRational $ Maestro.unMaestroRational $ drepVotingThresholdsHardForkInitiation protocolParametersDelegateRepresentativeVotingThresholds
                , dvtCommitteeNormal = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeDefault $ drepVotingThresholdsConstitutionalCommittee protocolParametersDelegateRepresentativeVotingThresholds
                , dvtCommitteeNoConfidence = unsafeBoundRational $ Maestro.unMaestroRational $ constitutionalCommitteeStateOfNoConfidence $ drepVotingThresholdsConstitutionalCommittee protocolParametersDelegateRepresentativeVotingThresholds
                }
        , cppCommitteeMinSize = THKD $ fromIntegral protocolParametersConstitutionalCommitteeMinSize
        , cppCommitteeMaxTermLength = THKD (Ledger.EpochInterval $ fromIntegral protocolParametersConstitutionalCommitteeMaxTermLength)
        , cppGovActionLifetime = THKD (Ledger.EpochInterval $ fromIntegral protocolParametersGovernanceActionLifetime)
        , cppGovActionDeposit = THKD $ Ledger.Coin $ fromIntegral $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersGovernanceActionDeposit
        , cppDRepDeposit = THKD $ Ledger.Coin $ fromIntegral $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersDelegateRepresentativeDeposit
        , cppDRepActivity = THKD (Ledger.EpochInterval $ fromIntegral protocolParametersDelegateRepresentativeMaxIdleTime)
        , cppMinFeeRefScriptCostPerByte = THKD $ unsafeBoundRational $ Maestro.minFeeReferenceScriptsBase protocolParametersMinFeeReferenceScripts
        }
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
                      { drepExpiry = ogDRepStateMandate s & asEpochEpoch & (GYEpochNo . fromIntegral)
                      , drepAnchor =
                          let man = ogDRepStateAnchor s
                           in man >>= \an -> Just $ GYAnchor (metadataUrl an) (metadataHash an)
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
