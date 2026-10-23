module Clb.Config (
  ClbConfig (..),
  CheckLimits (..),
  defaultSlotConfig,
  mkDefaultClbConfig,
  defaultBabbageClbConfig,
  defaultBabbageParams,
  defaultConwayClbConfig,
  defaultConwayParams,
  skipLimits,
  warnLimits,
  forceLimits,
  keptBlocks,
  paramsFromConfig,
  defaultConwayTransitionConfig,
  defaultBabbageTransitionConfig,
  defaultClbConfig,
) where

import Cardano.Api qualified as C
import Cardano.Chain.Genesis qualified as Byron (mainnetProtocolMagicId)
import Cardano.Crypto.ProtocolMagic qualified as Byron (ProtocolMagicId (..))
import Cardano.Ledger.Alonzo.Transition qualified as T
import Cardano.Ledger.Babbage.Transition qualified as T
import Cardano.Ledger.BaseTypes qualified as Shelley (Network (..))
import Cardano.Ledger.BaseTypes.NonZero (NonZero (unNonZero))
import Cardano.Ledger.Conway.Transition qualified as T
import Cardano.Ledger.Mary.Transition qualified as T
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.Transition qualified as T
import Clb.Era (CardanoLedgerEra, DefaultEmulatorEra)
import Clb.Params (
  PParams,
  TransitionConfig,
  defaultBabbageParams,
  defaultConwayParams,
 )
import Clb.TimeSlot (
  SlotConfig (SlotConfig, scSlotLength, scSlotZeroTime),
  nominalDiffTimeToPOSIXTime,
  utcTimeToPOSIXTime,
 )
import Control.Lens.Getter ((^.))
import PlutusLedgerApi.V3 (POSIXTime (getPOSIXTime))

-- | Config for the blockchain.
data ClbConfig era = ClbConfig
  { clbConfigCheckLimits :: !CheckLimits
  -- ^ limits check mode
  , clbConfigProtocol :: !(PParams era)
  -- ^ Protocol parameters
  , clbConfigNetworkId :: !C.NetworkId
  -- ^ Network id (mainnet / testnet)
  , clbConfigSlotConfig :: !SlotConfig
  -- ^ Slot config
  , clbConfigConfig :: !(TransitionConfig era)
  -- ^ Transition configuration : needed to be able to start in a current era
  }

data CheckLimits
  = -- | ignore TX-limits
    IgnoreLimits
  | -- | log TX to error log if it exceeds limits but accept TX
    WarnLimits
  | -- | reject TX if it exceeds the limits
    ErrorLimits
  deriving Show

-- | Default slot config
defaultSlotConfig :: SlotConfig
defaultSlotConfig =
  SlotConfig
    { scSlotLength = 1_000 -- each slot lasts for 1 second
    , scSlotZeroTime = 0 -- starts at unix epoch start
    }

{- | Default Babbage era config. If we use this parameter
 then Babbage era TXs will be used for testing.
-}
defaultBabbageClbConfig :: ClbConfig C.BabbageEra
defaultBabbageClbConfig = mkDefaultClbConfig defaultBabbageParams defaultBabbageTransitionConfig

defaultBabbageTransitionConfig :: TransitionConfig C.BabbageEra
defaultBabbageTransitionConfig =
  T.BabbageTransitionConfig $
    T.AlonzoTransitionConfig C.alonzoGenesisDefaults $
      T.MaryTransitionConfig $
        T.AllegraTransitionConfig $
          T.mkShelleyTransitionConfig C.shelleyGenesisDefaults

-- | Default Conwayconfig.
defaultConwayClbConfig :: ClbConfig C.ConwayEra
defaultConwayClbConfig = mkDefaultClbConfig defaultConwayParams defaultConwayTransitionConfig

defaultConwayTransitionConfig :: TransitionConfig C.ConwayEra
defaultConwayTransitionConfig = T.ConwayTransitionConfig C.conwayGenesisDefaults defaultBabbageTransitionConfig

defaultClbConfig :: ClbConfig DefaultEmulatorEra
defaultClbConfig = defaultConwayClbConfig

-- | Default blockchain config.
mkDefaultClbConfig :: PParams era -> TransitionConfig era -> ClbConfig era
mkDefaultClbConfig params config =
  ClbConfig
    { clbConfigCheckLimits = ErrorLimits
    , clbConfigProtocol = params
    , clbConfigNetworkId = C.Testnet $ C.NetworkMagic 42
    , clbConfigSlotConfig = defaultSlotConfig
    , clbConfigConfig = config
    }

paramsFromConfig ::
  T.EraTransition (CardanoLedgerEra era) =>
  TransitionConfig era ->
  ClbConfig era
paramsFromConfig tc =
  ClbConfig
    { clbConfigSlotConfig =
        SlotConfig
          { scSlotZeroTime = utcTimeToPOSIXTime $ L.sgSystemStart sg
          , scSlotLength =
              getPOSIXTime $ nominalDiffTimeToPOSIXTime $ L.fromNominalDiffTimeMicro $ L.sgSlotLength sg
          }
    , clbConfigProtocol = tc ^. T.tcInitialPParamsG
    , clbConfigNetworkId = fromShelleyNetwork (L.sgNetworkId sg) (C.NetworkMagic $ L.sgNetworkMagic sg)
    , clbConfigCheckLimits = ErrorLimits -- TODO: https://github.com/mlabs-haskell/clb/issues/50
    , clbConfigConfig = tc
    }
 where
  sg = tc ^. T.tcShelleyGenesisL

fromShelleyNetwork :: Shelley.Network -> C.NetworkMagic -> C.NetworkId
fromShelleyNetwork Shelley.Testnet nm = C.Testnet nm
fromShelleyNetwork Shelley.Mainnet nm
  | nm == mainnetNetworkMagic = C.Mainnet
  | otherwise = error "fromShelleyNetwork Mainnet: wrong mainnet network magic"

mainnetNetworkMagic :: C.NetworkMagic
mainnetNetworkMagic =
  C.NetworkMagic
    . Byron.unProtocolMagicId
    $ Byron.mainnetProtocolMagicId

-- | Do not check for limits
skipLimits :: ClbConfig era -> ClbConfig era
skipLimits cfg = cfg {clbConfigCheckLimits = IgnoreLimits}

-- | Warn on limits
warnLimits :: ClbConfig era -> ClbConfig era
warnLimits cfg = cfg {clbConfigCheckLimits = WarnLimits}

-- | Error on limits
forceLimits :: ClbConfig era -> ClbConfig era
forceLimits cfg = cfg {clbConfigCheckLimits = ErrorLimits}

keptBlocks :: T.EraTransition (CardanoLedgerEra era) => ClbConfig era -> Integer
keptBlocks ClbConfig {clbConfigConfig} =
  fromIntegral $ unNonZero $ L.sgSecurityParam (clbConfigConfig ^. T.tcShelleyGenesisL)
