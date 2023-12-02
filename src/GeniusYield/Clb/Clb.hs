module GeniusYield.Clb.Clb where

import Control.Monad.State (State, MonadState, gets)
import Cardano.Api qualified as C
import GeniusYield.Clb.MockConfig (MockConfig)
import PlutusLedgerApi.V1 (DatumHash, Datum, Address (addressCredential), TxOutRef, Credential)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

-- | State monad wrapper to run blockchain.
newtype Clb a = Clb (State ClbState a)
  deriving newtype (Functor, Applicative, Monad, MonadState ClbState)

data ClbState = ClbState
  { mockConfig :: !MockConfig
  , mockDatums :: !(Map DatumHash Datum)
  , mockAddresses :: !(Map Address (Set TxOutRef))
  -- , mockLedgerState :: !(C.UTxO C.BabbageEra)

  }

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> CardanoTx

-- | Log generic error.
logError :: String -> Clb ()
logError =
  -- FIXME:
  undefined -- logFail . GenericFail

logInfo :: String -> Clb ()
logInfo msg = do
  -- slot <- gets mockCurrentSlot
  -- modify' $ \s -> s {mockInfo = appendLog slot msg (mockInfo s)}
  -- FIXME:
  undefined

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: Address -> Clb [TxOutRef]
txOutRefAt addr = gets (txOutRefAtState addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: Address -> ClbState -> [TxOutRef]
txOutRefAtState addr st = foldMap S.toList . M.lookup addr $ mockAddresses st

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: Credential -> Clb [TxOutRef]
txOutRefAtPaymentCred cred = gets (txOutRefAtPaymentCredState cred)

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: Credential -> ClbState -> [TxOutRef]
txOutRefAtPaymentCredState cred st =
  S.toList $ M.foldrWithKey
    (\addr s acc -> if addressCredential addr == cred then s <> acc else acc)
    S.empty
    $ mockAddresses st


