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
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Api.L
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
 )
import Cardano.Ledger.Plutus qualified as Ledger
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<))
import Data.Aeson (Value (Null), encode, object, withObject, (.:), (.=))
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word16, Word32, Word64)
import Deriving.Aeson
import GHC.Int (Int64)
import GeniusYield.Imports
import GeniusYield.Providers.Common (
  SubmitTxException (..),
  datumFromCBOR,
  extractAssetClass,
  newServantClientEnv,
 )
import GeniusYield.Types
import GeniusYield.Types qualified as GYTypes (PlutusVersion (..))
import GeniusYield.Types.Script (GYAnyScript (..))
import Maestro.Types.V1 (AsAda, AsBytes, CostModel, EpochNo, LowerFirst, MaestroRational, MemoryCpuWith, MinFeeReferenceScripts, ProtocolParametersUpdateStakePool, ProtocolVersion)
import Maestro.Types.V1 qualified as Maestro
import Servant.API (
  Capture,
  Get,
  Header,
  Headers (getResponse),
  JSON,
  Post,
  QueryFlag,
  QueryParam,
  ReqBody,
  ResponseHeader (Header),
  lookupResponseHeader,
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

submitTx :: OgmiosRequest GYTx -> ClientM (OgmiosResponse TxSubmissionResponse)
protocolParams :: OgmiosRequest OgmiosPP -> ClientM (OgmiosResponse ProtocolParameters)
tip :: OgmiosRequest OgmiosTip -> ClientM (OgmiosResponse OgmiosTipResponse)
stakePools :: OgmiosRequest OgmiosStakePools -> ClientM (OgmiosResponse OgmiosStakePoolsResponse)

type OgmiosApi = ReqBody '[JSON] (OgmiosRequest GYTx) :> Post '[JSON] (OgmiosResponse TxSubmissionResponse) :<|> ReqBody '[JSON] (OgmiosRequest OgmiosPP) :> Post '[JSON] (OgmiosResponse ProtocolParameters) :<|> ReqBody '[JSON] (OgmiosRequest OgmiosTip) :> Post '[JSON] (OgmiosResponse OgmiosTipResponse) :<|> ReqBody '[JSON] (OgmiosRequest OgmiosStakePools) :> Post '[JSON] (OgmiosResponse OgmiosStakePoolsResponse)

submitTx :<|> protocolParams :<|> tip :<|> stakePools = client @OgmiosApi Proxy

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