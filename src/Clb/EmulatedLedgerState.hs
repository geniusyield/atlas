{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Clb.EmulatedLedgerState (
  EmulatedLedgerState (..),
  ledgerEnv,
  ledgerState,
  initialState,
  setUtxo,
  updateSlot,
  setSlot,
  nextSlot,
  getSlot,
) where

import Cardano.Api (ShelleyLedgerEra)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Transition (tcInitialStakingL)
import Cardano.Ledger.Api.Transition qualified as L
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.State (ChainAccountState (..), EraStake (InstantStake), mkStakePoolState, psStakePoolsL)
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra)
import Clb.Params (PParams, TransitionConfig)
import Control.Lens (makeLenses, over, (^.))
import Control.Lens.Setter ((.~))
import Data.Default (Default, def)
import Data.Function ((&))
import Data.ListMap qualified as ListMap

{- | State of the ledger - might be used to store ledger state of the whole
blockchain or to maintain a so-called "cached state" that reflect the state
of things for the mempool.
-}
data EmulatedLedgerState era = EmulatedLedgerState
  { _ledgerEnv :: !(L.MempoolEnv (CardanoLedgerEra era))
  , _ledgerState :: !(L.MempoolState (CardanoLedgerEra era))
  }

deriving instance (IsCardanoLedgerEra era, Show (L.CertState (ShelleyLedgerEra era)), Show (InstantStake (ShelleyLedgerEra era))) => Show (EmulatedLedgerState era)

makeLenses ''EmulatedLedgerState

-- | Initial ledger state for a distribution
initialState ::
  forall era.
  ( L.EraTxOut (CardanoLedgerEra era)
  , L.EraTransition (CardanoLedgerEra era)
  , Default (L.GovState (CardanoLedgerEra era))
  ) =>
  PParams era ->
  TransitionConfig era ->
  EmulatedLedgerState era
initialState params tc =
  let L.ShelleyGenesisStaking {sgsPools} = tc ^. tcInitialStakingL
      state =
        EmulatedLedgerState
          { _ledgerEnv =
              L.LedgerEnv
                { L.ledgerSlotNo = 0
                , L.ledgerEpochNo = Nothing
                , L.ledgerIx = minBound
                , L.ledgerPp = params
                , L.ledgerAccount = ChainAccountState (L.Coin 0) (L.Coin 0)
                }
          , _ledgerState =
              L.LedgerState
                { lsUTxOState = L.smartUTxOState params mempty (L.Coin 0) (L.Coin 0) def (L.Coin 0)
                , lsCertState = def & L.certPStateL .~ pState'
                }
          }
      pState' :: L.PState (CardanoLedgerEra era) =
        def
          & psStakePoolsL
            .~ fmap (mkStakePoolState mempty mempty) (ListMap.toMap sgsPools)
   in state

-- | Set the slot number
setSlot :: SlotNo -> EmulatedLedgerState era -> EmulatedLedgerState era
setSlot sl = over ledgerEnv (\l -> l {L.ledgerSlotNo = sl})

-- | Update the slot number
updateSlot :: (SlotNo -> SlotNo) -> EmulatedLedgerState era -> EmulatedLedgerState era
updateSlot f s =
  let slot = f $ getSlotNo s
   in setSlot slot s

-- | Increase the slot number by one
nextSlot :: EmulatedLedgerState era -> EmulatedLedgerState era
nextSlot = updateSlot succ

-- | Get the slot number
getSlotNo :: EmulatedLedgerState era -> SlotNo
getSlotNo (EmulatedLedgerState L.LedgerEnv {ledgerSlotNo} _) = ledgerSlotNo

-- | Get the slot number
getSlot :: Num a => EmulatedLedgerState era -> a
getSlot (EmulatedLedgerState L.LedgerEnv {ledgerSlotNo = SlotNo s} _) = fromIntegral s

-- | Set the utxo
setUtxo ::
  ( L.EraTxOut (CardanoLedgerEra era)
  , Default (L.GovState (CardanoLedgerEra era))
  , EraStake (CardanoLedgerEra era)
  ) =>
  L.PParams (CardanoLedgerEra era) ->
  L.UTxO (CardanoLedgerEra era) ->
  EmulatedLedgerState era ->
  EmulatedLedgerState era
setUtxo params utxo els@EmulatedLedgerState {_ledgerState} = els {_ledgerState = newPoolState}
 where
  newPoolState = _ledgerState {L.lsUTxOState = L.smartUTxOState params utxo (L.Coin 0) (L.Coin 0) def (L.Coin 0)}
