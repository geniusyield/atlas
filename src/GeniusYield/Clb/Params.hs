module GeniusYield.Clb.Params where

import Cardano.Api.Shelley qualified as C (shelleyGenesisDefaults)
import Cardano.Ledger.Alonzo qualified as L (AlonzoEra)
import Cardano.Ledger.Alonzo.Core qualified as L (CoinPerWord(CoinPerWord))
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.PParams qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage qualified as L (BabbageEra)
import Cardano.Ledger.Babbage.PParams qualified as Babbage
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Crypto qualified as L (StandardCrypto)
import Cardano.Ledger.Shelley.Genesis qualified as L (ShelleyGenesis, mkShelleyGlobals)
import Cardano.Slotting.EpochInfo qualified as S (fixedEpochInfo)
import Cardano.Slotting.Time qualified as S (SlotLength)
import Data.Coerce (coerce)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)


-- | Type that unifies protocol parameters across eras.
data PParams
  = -- | alonzo era protocol parameters
    AlonzoParams (L.PParams (L.AlonzoEra L.StandardCrypto))
  | -- | babbage era protocol parameters
    BabbageParams (L.PParams (L.BabbageEra L.StandardCrypto))

babbageOnly :: PParams -> L.PParams (L.BabbageEra L.StandardCrypto)
babbageOnly (BabbageParams v) = v
babbageOnly _ = error "Sorry, Babbage only"

-- Alonzo

-- | Default Alonzo era parameters
defaultAlonzoParams :: PParams
defaultAlonzoParams = AlonzoParams defaultAlonzoParams'

defaultAlonzoParams' :: L.PParams (L.AlonzoEra L.StandardCrypto)
defaultAlonzoParams' =
  let
    app :: Alonzo.AlonzoPParams Identity (L.AlonzoEra L.StandardCrypto) =
      Alonzo.AlonzoPParams
        { Alonzo.appMinFeeA = L.Coin 44
        , Alonzo.appMinFeeB = L.Coin 155381
        , Alonzo.appMaxBBSize = 90112
        , Alonzo.appMaxTxSize = 16384
        , Alonzo.appMaxBHSize = 1100
        , Alonzo.appKeyDeposit = L.Coin 2_000_000
        , Alonzo.appPoolDeposit = L.Coin 500_000_000
        , Alonzo.appEMax = 18
        , Alonzo.appNOpt = 500
        , Alonzo.appA0 = rational 0.3
        , Alonzo.appRho = rational 0.003
        , Alonzo.appTau = rational 0.2
        , Alonzo.appD = rational 0.7
        , Alonzo.appExtraEntropy = L.NeutralNonce
        , Alonzo.appProtocolVersion = L.ProtVer {L.pvMajor = L.eraProtVerHigh @(L.AlonzoEra L.StandardCrypto), L.pvMinor = 0}
        , Alonzo.appMinPoolCost = L.Coin 340_000_000
        , Alonzo.appCoinsPerUTxOWord = L.CoinPerWord (L.Coin 34482)  -- When updated to babbage, it will divide it by 8 and round down, thus giving correct value.
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
  in coerce app

rational :: L.BoundedRational r => Rational -> r
rational = fromJust . L.boundRational

defaultBabbageParams' :: L.PParams (L.BabbageEra L.StandardCrypto)
defaultBabbageParams' = L.upgradePParams () defaultAlonzoParams'

defaultCostModels :: Alonzo.CostModels
defaultCostModels =
  (\validCM -> Alonzo.CostModels validCM mempty mempty) $
    Map.fromList $
      fmap toCostModel [Alonzo.PlutusV1, Alonzo.PlutusV2]
  where
    toCostModel lang =
      ( lang
      , fromRight
          (error "Cost model apply fail") $
          Alonzo.mkCostModel lang $
            case lang of
              Alonzo.PlutusV1 -> [205665,812,1,1,1000,571,0,1,1000,24177,4,1,1000,32,117366,10475,4,23000,100,23000,100,23000,100,23000,100,23000,100,23000,100,100,100,23000,100,19537,32,175354,32,46417,4,221973,511,0,1,89141,32,497525,14068,4,2,196500,453240,220,0,1,1,1000,28662,4,2,245000,216773,62,1,1060367,12586,1,208512,421,1,187000,1000,52998,1,80436,32,43249,32,1000,32,80556,1,57667,4,1000,10,197145,156,1,197145,156,1,204924,473,1,208896,511,1,52467,32,64832,32,65493,32,22558,32,16563,32,76511,32,196500,453240,220,0,1,1,69522,11687,0,1,60091,32,196500,453240,220,0,1,1,196500,453240,220,0,1,1,806990,30482,4,1927926,82523,4,265318,0,4,0,85931,32,205665,812,1,1,41182,32,212342,32,31220,32,32696,32,43357,32,32247,32,38314,32,57996947,18975,10]
              Alonzo.PlutusV2 -> [205665,812,1,1,1000,571,0,1,1000,24177,4,1,1000,32,117366,10475,4,23000,100,23000,100,23000,100,23000,100,23000,100,23000,100,100,100,23000,100,19537,32,175354,32,46417,4,221973,511,0,1,89141,32,497525,14068,4,2,196500,453240,220,0,1,1,1000,28662,4,2,245000,216773,62,1,1060367,12586,1,208512,421,1,187000,1000,52998,1,80436,32,43249,32,1000,32,80556,1,57667,4,1000,10,197145,156,1,197145,156,1,204924,473,1,208896,511,1,52467,32,64832,32,65493,32,22558,32,16563,32,76511,32,196500,453240,220,0,1,1,69522,11687,0,1,60091,32,196500,453240,220,0,1,1,196500,453240,220,0,1,1,1159724,392670,0,2,806990,30482,4,1927926,82523,4,265318,0,4,0,85931,32,205665,812,1,1,41182,32,212342,32,31220,32,32696,32,43357,32,32247,32,38314,32,35892428,10,57996947,18975,10,38887044,32947,10]
              Alonzo.PlutusV3 -> error "PlutusV3 is not yet supported."
      )

-- Babbage

-- | Default Babbage V2 era parameters
defaultBabbageParams :: PParams
defaultBabbageParams =
  let old = coerce defaultBabbageParams'
  in
    BabbageParams
      $ coerce $ old
        { Babbage.bppProtocolVersion =
            L.ProtVer {pvMajor = L.eraProtVerHigh @(L.BabbageEra L.StandardCrypto), pvMinor = 0}
        }

-- | A sensible default 'Globals' value for the emulator
mkGlobals :: S.SlotLength -> L.Version -> L.Globals
mkGlobals sLength = L.mkShelleyGlobals
  genesisDefaultsFromParams
  (S.fixedEpochInfo emulatorEpochSize sLength)

genesisDefaultsFromParams :: L.ShelleyGenesis L.StandardCrypto
genesisDefaultsFromParams = C.shelleyGenesisDefaults -- FIXME:

-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: L.EpochSize
emulatorEpochSize = L.EpochSize 432000


