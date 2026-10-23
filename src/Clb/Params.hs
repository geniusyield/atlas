module Clb.Params where

import Cardano.Api (AlonzoEra, BabbageEra, ConwayEra)
import Cardano.Api qualified as C (
  BabbageEra,
  ConwayEra,
  NetworkMagic (NetworkMagic),
  ShelleyGenesis (sgNetworkMagic, sgProtocolParams, sgSystemStart),
  alonzoGenesisDefaults,
  conwayGenesisDefaults,
  shelleyGenesisDefaults,
 )
import Cardano.Api.Ledger qualified as C
import Cardano.Ledger.Alonzo.Core qualified as L (CoinPerWord (CoinPerWord))
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api.Era qualified as L
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.Babbage.PParams qualified as Babbage
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Compactible qualified as Compactible
import Cardano.Ledger.Conway.PParams qualified as Conway
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Clb.Era (CardanoLedgerEra)
import Clb.TimeSlot (beginningOfTime, posixTimeToUTCTime)
import Control.Lens ((.~))
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import PlutusLedgerApi.V1 (POSIXTime (POSIXTime))
import Test.Cardano.Ledger.Plutus qualified as LT (testingCostModelV3)

type PParams era = L.PParams (CardanoLedgerEra era)

-- Shelley

emulatorProtocolMajorVersion :: L.Version
emulatorProtocolMajorVersion = L.natVersion @9

{- | Some reasonable starting defaults for constructing a 'ShelleyGenesis'.
  TODO: Override the following fields:
    * 'sgGenDelegs' to have some initial nodes
    * 'sgInitialFunds' to have any money in the system
    * 'sgMaxLovelaceSupply' must be at least the sum of the 'sgInitialFunds'
-}
emulatorShelleyGenesisDefaults :: C.ShelleyGenesis
emulatorShelleyGenesisDefaults =
  C.shelleyGenesisDefaults
    { C.sgNetworkMagic = case testNetworkMagic of C.NetworkMagic nm -> nm
    , C.sgSystemStart = posixTimeToUTCTime $ POSIXTime beginningOfTime -- usually wrong in emulator's setting
    , C.sgProtocolParams =
        C.sgProtocolParams C.shelleyGenesisDefaults
          & L.ppProtocolVersionL
            .~ L.ProtVer emulatorProtocolMajorVersion 0
          & L.ppTxFeeFixedL
            .~ L.Coin 155_381
          & L.ppTxFeePerByteL
            .~ L.CoinPerByte (Compactible.toCompactPartial $ L.Coin 44)
          & L.ppKeyDepositL
            .~ L.Coin 2_000_000
    }
 where
  testNetworkMagic :: C.NetworkMagic
  testNetworkMagic = C.NetworkMagic 1097911063

-- Alonzo

-- defaultAlonzoParams' :: L.PParams (L.AlonzoEra L.StandardCrypto)
defaultAlonzoParams' :: PParams AlonzoEra
defaultAlonzoParams' =
  let
    app :: Alonzo.AlonzoPParams Identity L.AlonzoEra =
      Alonzo.AlonzoPParams
        { Alonzo.appTxFeePerByte = L.CoinPerByte $ Compactible.toCompactPartial (L.Coin 44)
        , Alonzo.appTxFeeFixed = Compactible.toCompactPartial (L.Coin 155381)
        , Alonzo.appMaxBBSize = 90112
        , Alonzo.appMaxTxSize = 16384
        , Alonzo.appMaxBHSize = 1100
        , Alonzo.appKeyDeposit = Compactible.toCompactPartial (L.Coin 2_000_000)
        , Alonzo.appPoolDeposit = Compactible.toCompactPartial (L.Coin 500_000_000)
        , Alonzo.appEMax = L.EpochInterval 18
        , Alonzo.appNOpt = 500
        , Alonzo.appA0 = rational 0.3
        , Alonzo.appRho = rational 0.003
        , Alonzo.appTau = rational 0.2
        , Alonzo.appD = rational 0.7
        , Alonzo.appExtraEntropy = L.NeutralNonce
        , Alonzo.appProtocolVersion = L.ProtVer {L.pvMajor = L.eraProtVerHigh @L.AlonzoEra, L.pvMinor = 0}
        , Alonzo.appMinPoolCost = Compactible.toCompactPartial (L.Coin 340_000_000)
        , Alonzo.appCoinsPerUTxOWord = L.CoinPerWord (L.Coin 34482) -- When updated to babbage, it will divide it by 8 and round down, thus giving correct value.
        , Alonzo.appCostModels = defaultCostModels
        , Alonzo.appPrices =
            Alonzo.Prices
              { Alonzo.prMem = rational 0.0577
              , Alonzo.prSteps = rational 7.21e-05
              }
        , Alonzo.appMaxTxExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 14000000 10000000000
        , Alonzo.appMaxBlockExUnits = Alonzo.OrdExUnits $ Alonzo.ExUnits 62000000 20000000000
        , Alonzo.appMaxValSize = 5000
        , Alonzo.appCollateralPercentage = 150
        , Alonzo.appMaxCollateralInputs = 3
        }
   in
    coerce app

{- | Some reasonable starting defaults for constructing a 'AlonzoGenesis'.
Based on https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Defaults.hs
The era determines Plutus V2 cost model parameters:
* Conway: 185
* <= Babbage: 175
-}
emulatorAlonzoGenesisDefaults :: C.AlonzoGenesis
emulatorAlonzoGenesisDefaults = C.alonzoGenesisDefaults

rational :: L.BoundedRational r => Rational -> r
rational = fromJust . L.boundRational

-- defaultBabbageParams' :: L.PParams (L.BabbageEra L.StandardCrypto)
defaultBabbageParams' :: PParams BabbageEra
defaultBabbageParams' = L.upgradePParams () defaultAlonzoParams'

defaultCostModels :: Alonzo.CostModels
defaultCostModels =
  Alonzo.mkCostModels $
    Map.fromList $
      fmap
        toCostModel
        [ Plutus.PlutusV1
        , Plutus.PlutusV2
        ]
 where
  toCostModel lang =
    ( lang
    , fromRight
        (error "Cost model apply fail")
        $ Alonzo.mkCostModel lang
        $ case lang of
          Plutus.PlutusV1 -> [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 57996947, 18975, 10]
          Plutus.PlutusV2 -> [205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475, 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100, 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32, 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000, 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436, 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145, 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558, 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091, 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670, 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812, 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32, 35892428, 10, 57996947, 18975, 10, 38887044, 32947, 10]
          Plutus.PlutusV3 -> error "PlutusV3 is not yet supported."
          Plutus.PlutusV4 -> error "PlutusV4 is not yet supported."
    )

-- Babbage

-- | Default Babbage V2 era parameters
defaultBabbageParams :: PParams BabbageEra
defaultBabbageParams =
  let old = coerce defaultBabbageParams'
   in coerce $
        old
          { Babbage.bppProtocolVersion =
              L.ProtVer {pvMajor = L.eraProtVerHigh @(CardanoLedgerEra C.BabbageEra), pvMinor = 0}
          }

-- Conway

-- | Default Babbage V2 era parameters
defaultConwayParams :: PParams ConwayEra
defaultConwayParams =
  let old =
        coerce $
          L.upgradePParams
            Conway.UpgradeConwayPParams
              { -- Based on: https://book.world.dev.cardano.org/environments/private/conway-genesis.json
                -- Alternative option: https://book.world.dev.cardano.org/environments/sanchonet/conway-genesis.json
                -- Maybe update to whatever preprod/preview is using when it's available?
                ucppPoolVotingThresholds =
                  Conway.PoolVotingThresholds
                    { pvtPPSecurityGroup = fromJust $ L.boundRational 0.6
                    , pvtMotionNoConfidence = fromJust $ L.boundRational 0.6
                    , pvtHardForkInitiation = fromJust $ L.boundRational 0.51
                    , pvtCommitteeNormal = fromJust $ L.boundRational 0.6
                    , pvtCommitteeNoConfidence = fromJust $ L.boundRational 0.51
                    }
              , ucppMinFeeRefScriptCostPerByte = fromJust $ L.boundRational 44
              , ucppGovActionLifetime = L.EpochInterval 8
              , ucppGovActionDeposit = 50000000000
              , ucppDRepVotingThresholds =
                  Conway.DRepVotingThresholds
                    { dvtUpdateToConstitution = fromJust $ L.boundRational 0.75
                    , dvtTreasuryWithdrawal = fromJust $ L.boundRational 0.67
                    , dvtPPTechnicalGroup = fromJust $ L.boundRational 0.67
                    , dvtPPNetworkGroup = fromJust $ L.boundRational 0.67
                    , dvtPPGovGroup = fromJust $ L.boundRational 0.75
                    , dvtPPEconomicGroup = fromJust $ L.boundRational 0.67
                    , dvtMotionNoConfidence = fromJust $ L.boundRational 0.67
                    , dvtHardForkInitiation = fromJust $ L.boundRational 0.6
                    , dvtCommitteeNormal = fromJust $ L.boundRational 0.67
                    , dvtCommitteeNoConfidence = fromJust $ L.boundRational 0.6
                    }
              , ucppDRepDeposit = 500000000
              , ucppDRepActivity = L.EpochInterval 20
              , ucppCommitteeMinSize = 0
              , ucppCommitteeMaxTermLength = L.EpochInterval 73
              , ucppPlutusV3CostModel = LT.testingCostModelV3 -- TODO: This is temporary, update.
              }
            defaultBabbageParams
   in coerce $
        old
          { Conway.cppProtocolVersion =
              L.ProtVer {pvMajor = L.eraProtVerHigh @(CardanoLedgerEra C.ConwayEra), pvMinor = 0}
          }

emulatorConwayGenesisDefaults :: C.ConwayGenesis
emulatorConwayGenesisDefaults = C.conwayGenesisDefaults

-- Transition Config
type TransitionConfig era = L.TransitionConfig (CardanoLedgerEra era)
