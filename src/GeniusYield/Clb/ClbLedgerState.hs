{-# LANGUAGE TemplateHaskell #-}
module GeniusYield.Clb.ClbLedgerState where

import Cardano.Ledger.Api qualified as C
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Shelley.API qualified as L
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.Slot (SlotNo)
import Control.Lens (makeLenses, (^.), over, (.~), (&))
import Data.Default (def)
import GeniusYield.Clb.Era (EmulatorEra)
import GeniusYield.Clb.Params (PParams (..))


type EmulatorBlock = [L.Validated (Core.Tx EmulatorEra)]


{- | State of the ledger with configuration, mempool, and the blockchain.
-}
data EmulatedLedgerState =
  EmulatedLedgerState
    { _ledgerEnv      :: !(L.MempoolEnv EmulatorEra)
    , _memPoolState   :: !(L.MempoolState EmulatorEra)
    , _currentBlock   :: !EmulatorBlock
    , _previousBlocks :: ![EmulatorBlock]
    }
    deriving Show

makeLenses ''EmulatedLedgerState

{- | Increase the slot number by one
-}
nextSlot :: EmulatedLedgerState -> EmulatedLedgerState
nextSlot = over ledgerEnv f where
  f l@L.LedgerEnv{ledgerSlotNo=oldSlot} = l{L.ledgerSlotNo = succ oldSlot}

{- | Set the slot number
-}
setSlot :: SlotNo -> EmulatedLedgerState -> EmulatedLedgerState
setSlot sl = over ledgerEnv (\l -> l{L.ledgerSlotNo=sl})

{- | Set the utxo
-}
setUtxo :: C.PParams EmulatorEra -> L.UTxO EmulatorEra -> EmulatedLedgerState -> EmulatedLedgerState
setUtxo params utxo els@EmulatedLedgerState{_memPoolState} = els { _memPoolState = newPoolState }
  where
    newPoolState = _memPoolState { L.lsUTxOState = L.smartUTxOState params utxo (L.Coin 0) (L.Coin 0) def }

{- | Make a block with all transactions that have been validated in the
current block, add the block to the blockchain, and empty the current block.
-}
makeBlock :: EmulatedLedgerState -> EmulatedLedgerState
makeBlock state =
  state
    & currentBlock .~ []
    & over previousBlocks ((reverse $ state ^. currentBlock) :)

{- | Initial ledger state for a distribution
-}
initialState :: PParams -> EmulatedLedgerState
initialState (BabbageParams params) = EmulatedLedgerState
  { _ledgerEnv = L.LedgerEnv
      { L.ledgerSlotNo = 0
      , L.ledgerIx = minBound
      , L.ledgerPp = params
      , L.ledgerAccount = L.AccountState (L.Coin 0) (L.Coin 0)
      }
  , _memPoolState = L.LedgerState
    { lsUTxOState = L.smartUTxOState params mempty (L.Coin 0) (L.Coin 0) def
    , lsCertState = def
    }
  , _currentBlock = []
  , _previousBlocks = []
  }
initialState _ = error "Unsupported PParams"