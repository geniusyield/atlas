module GeniusYield.Clb.Clb where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Address qualified as L (compactAddr)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L (BabbageTxOut(TxOutCompact), getEitherAddrBabbageTxOut)
import Cardano.Ledger.BaseTypes qualified as L (Network (Testnet), Globals, mkVersion)
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.SafeHash qualified as L
import Cardano.Ledger.Shelley.API qualified as L (LedgerState(..), UTxOState (utxosUtxo), StakeReference (..), Validated, applyTx)
import Cardano.Ledger.TxIn qualified as L (TxId (..), TxIn (..), mkTxInPartial)
import Cardano.Ledger.UTxO qualified as L (UTxO (..))
import Control.Lens (over, (.~))
import Control.Monad.State (State, MonadState (get), gets, runState, modify', put)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GeniusYield.Clb.ClbLedgerState (EmulatedLedgerState (..), initialState, setUtxo, memPoolState, currentBlock)
import GeniusYield.Clb.Era (EmulatorEra)
import GeniusYield.Clb.MockConfig (MockConfig (..))
import GeniusYield.Clb.Params (PParams(BabbageParams, AlonzoParams), mkGlobals, babbageOnly)
import GeniusYield.Clb.TimeSlot (slotLength)
import GeniusYield.Imports
import GeniusYield.Types.TxOutRef (txOutRefToPlutus, txOutRefFromApi)
import GeniusYield.Types.Value (GYValue, valueToApi)
import PlutusLedgerApi.V1 qualified as P (Datum, DatumHash, TxOutRef, Credential)
import PlutusLedgerApi.V1.Scripts qualified as P (ScriptError)
import Prettyprinter ( Pretty(pretty), colon, (<+>), indent, vcat )
import Test.Cardano.Ledger.Core.KeyPair qualified as TL
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseInfo, assertFailure)

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> CardanoTx

-- A validated Tx, but: might has IsValid = False
newtype OnChainTx = OnChainTx { getOnChainTx :: L.Validated (Core.Tx EmulatorEra) }

-- | A reason why a transaction is invalid.
data ValidationError =
    TxOutRefNotFound -- TxIn
    -- ^ The transaction output consumed by a transaction input could not be found
    -- (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | ScriptFailure P.ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CardanoLedgerValidationError Text
    -- ^ An error from Cardano.Ledger validation
    | MaxCollateralInputsExceeded
    -- ^ Balancing failed, it needed more than the maximum number of collateral inputs
    deriving (Eq, Show)

data ValidationResult
  = FailPhase1 !CardanoTx !ValidationError
  -- ^ A transaction failed to validate in phase 1.
  | FailPhase2 !OnChainTx !ValidationError !(MaryValue L.StandardCrypto)
  -- ^ A transaction failed to validate in phase 2. The @Value@ indicates the amount of collateral stored in the transaction.
  | Success !EmulatedLedgerState !OnChainTx -- !RedeemerReport
  -- deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

-- | State monad wrapper to run emulator.
newtype Clb a = Clb (State ClbState a)
  deriving newtype (Functor, Applicative, Monad, MonadState ClbState)

-- | Emulator state: ledger state + some additional things
-- FIXME: remove non-state parts like MockConfig and Log (?)
data ClbState = ClbState
  { emulatedLedgerState :: !EmulatedLedgerState
  , mockConfig :: !MockConfig
  , mockDatums :: !(Map P.DatumHash P.Datum)
  , mockInfo :: !(Log String)
  , mockCurrentSlot :: !Slot -- FIXME: sync up with emulatedLedgerState
  }

-- | Run emulator.
runMock :: Clb a -> ClbState -> (a, ClbState)
runMock (Clb act) = runState act

-- | Init emulator state.
initMock :: MockConfig -> GYValue -> ClbState
initMock MockConfig{mockConfigProtocol = (AlonzoParams _)} _ = error "Unsupported params"
initMock cfg@MockConfig{mockConfigProtocol = params@(BabbageParams pparams)} initVal =
  ClbState
    { emulatedLedgerState = setUtxo pparams utxos (initialState params)
    , mockDatums = M.empty
    , mockConfig = cfg
    , mockCurrentSlot = Slot 1
    , mockInfo = mempty
    }
  where

    utxos = L.UTxO $ M.fromList [genesis]

    genesis :: (L.TxIn (Core.EraCrypto EmulatorEra), Core.TxOut EmulatorEra)
    genesis =
      ( L.mkTxInPartial genesisTxId 0
      , L.TxOutCompact
          (L.compactAddr $ mkAddr' $ intToKeyPair 0)
          (fromJust $ L.toCompact $ C.toMaryValue $ valueToApi initVal)
      )

    mkAddr' payKey = L.Addr L.Testnet (TL.mkCred payKey) L.StakeRefNull

    -- | genesis transaction ID
    genesisTxId :: L.TxId L.StandardCrypto
    genesisTxId = L.TxId $ L.unsafeMakeSafeHash dummyHash

    -- Hash for genesis transaction
    dummyHash :: Crypto.Hash Crypto.Blake2b_256 Core.EraIndependentTxBody
    dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

--------------------------------------------------------------------------------
-- Trace log (from PSM)
--------------------------------------------------------------------------------

-- | Log of Slot-timestamped events
newtype Log a = Log {unLog :: Seq (Slot, a)}
  deriving (Functor)

instance Semigroup (Log a) where
  (<>) (Log sa) (Log sb) = Log (merge sa sb)
    where
      merge Empty b = b
      merge a Empty = a
      merge (a :<| as) (b :<| bs) =
        if fst a <= fst b
          then a Seq.<| merge as (b Seq.<| bs)
          else b Seq.<| merge (a Seq.<| as) bs

instance Monoid (Log a) where
  mempty = Log Seq.empty

ppMockEvent :: Log String -> String
ppMockEvent = show . vcat . fmap ppSlot . fromGroupLog
  where
    ppSlot (slot, events) = vcat [pretty slot <> colon, indent 2 (vcat $ pretty <$> events)]

fromGroupLog :: Log a -> [(Slot, [a])]
fromGroupLog = fmap toGroup . groupBy ((==) `on` fst) . fromLog
  where
    toGroup ((a, b) : rest) = (a, b : fmap snd rest)
    toGroup [] = error "toGroup: Empty list"

-- | Convert log to plain list
fromLog :: Log a -> [(Slot, a)]
fromLog (Log s) = toList s

newtype Slot = Slot {getSlot :: Integer}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Enum, Real, Integral)

instance Pretty Slot where
  pretty (Slot i) = "Slot" <+> pretty i

--------------------------------------------------------------------------------
-- Actions in Clb monad
--------------------------------------------------------------------------------

-- | Log generic error.
logError :: String -> Clb ()
logError = logInfo
  -- FIXME:
  -- logFail . GenericFail

logInfo :: String -> Clb ()
logInfo msg = do
  slot <- gets mockCurrentSlot
  modify' $ \s -> s {mockInfo = appendLog slot msg (mockInfo s)}

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

-- | Read all TxOutRefs that belong to given address.
txOutRefAt :: C.AddressInEra C.BabbageEra -> Clb [P.TxOutRef]
txOutRefAt addr = gets (txOutRefAtState $ C.toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: L.Addr L.StandardCrypto -> ClbState -> [P.TxOutRef]
txOutRefAtState addr st =
  txOutRefToPlutus . txOutRefFromApi . C.fromShelleyTxIn
    <$> M.keys (M.filter atAddr utxos)
  where
    utxos = L.unUTxO $ L.utxosUtxo $ L.lsUTxOState $ _memPoolState $ emulatedLedgerState st

    atAddr :: L.BabbageTxOut EmulatorEra -> Bool
    atAddr out = case L.getEitherAddrBabbageTxOut out of
      Right cAddr -> L.compactAddr addr == cAddr
      -- Left should never happen (abuse of Either in fact)
      Left addr' -> addr == addr'

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: P.Credential -> Clb [P.TxOutRef]
txOutRefAtPaymentCred cred = gets (txOutRefAtPaymentCredState cred)

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: P.Credential -> ClbState -> [P.TxOutRef]
txOutRefAtPaymentCredState _cred _st = TODO

sendTx :: C.Tx C.BabbageEra -> Clb ValidationResult
sendTx (C.ShelleyTx _ tx) = do -- FIXME: use patterns, but not cardano-api:internal!
  state@ClbState{mockConfig=MockConfig{mockConfigProtocol, mockConfigSlotConfig}} <- get
  -- FIXME: fromJust
  let majorVer = fromJust $ runIdentity
        $ runMaybeT @Identity $ L.mkVersion $ fst $ C.protocolParamProtocolVersion
        $ C.fromLedgerPParams C.ShelleyBasedEraBabbage $ babbageOnly mockConfigProtocol
  let globals = mkGlobals (slotLength mockConfigSlotConfig) majorVer
  let ret = validateTx
        globals
        (emulatedLedgerState state)
        tx
  case ret of
    Success newState _ -> put $ state { emulatedLedgerState = newState }
    FailPhase1 _ err -> error $ "Ahhh" <> show err
    FailPhase2 _ err _ -> error $ "Ahhh" <> show err
  pure ret

--------------------------------------------------------------------------------
-- Helpers for working with tests
--------------------------------------------------------------------------------

-- | Helper for building tests
testNoErrorsTraceClb :: GYValue -> MockConfig -> String -> Clb a -> TestTree
testNoErrorsTraceClb funds cfg msg act =
  testCaseInfo msg $ do
    logg <- maybe (pure mockLog) assertFailure errors
    pure $ logg <> "\nState: " <> show (emulatedLedgerState mock)
  where
    (errors, mock) = runMock (act >> checkErrors) $ initMock cfg funds
    mockLog = "\nBlockchain log :\n----------------\n" <> ppMockEvent (mockInfo mock)

checkErrors :: Clb (Maybe String)
checkErrors = do
  -- FIXME: implement
  _ <- get
  pure Nothing

--------------------------------------------------------------------------------
-- Transactions validation TODO: factor out
--------------------------------------------------------------------------------

validateTx :: L.Globals -> EmulatedLedgerState -> Core.Tx EmulatorEra -> ValidationResult
validateTx globals state  tx =
  case res of
    -- FIXME: why Phase1, not sure here?
    Left err ->
      FailPhase1
        (CardanoTx (C.ShelleyTx C.ShelleyBasedEraBabbage tx) C.BabbageEraInCardanoMode)
        err
    Right (newState, vtx) ->
      Success newState vtx
  where
    res = fmap OnChainTx <$> applyTx globals state tx

-- | A wrapper around the ledger's applyTx
-- TODO: step slot somewhere, since this is not ledger's responsibility!
applyTx ::
  L.Globals ->
  EmulatedLedgerState ->
  Core.Tx EmulatorEra ->
  Either ValidationError (EmulatedLedgerState, L.Validated (Core.Tx EmulatorEra))
applyTx globals oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <-
    first (CardanoLedgerValidationError . Text.pack . show)
    (L.applyTx globals _ledgerEnv _memPoolState tx)
  pure (oldState & memPoolState .~ newMempool & over currentBlock (vtx :), vtx)

--------------------------------------------------------------------------------
-- Key utils
--------------------------------------------------------------------------------

-- | Create key pair from an integer (deterministic)
intToKeyPair :: L.Crypto c => Integer -> TL.KeyPair r c
intToKeyPair n = TL.KeyPair vk sk
  where
    sk = Crypto.genKeyDSIGN $ mkSeedFromInteger n
    vk = L.VKey $ Crypto.deriveVerKeyDSIGN sk

    {- | Construct a seed from a bunch of Word64s

      We multiply these words by some extra stuff to make sure they contain
      enough bits for our seed.
    -}
    mkSeedFromInteger :: Integer -> Crypto.Seed
    mkSeedFromInteger stuff =
      Crypto.mkSeedFromBytes . Crypto.hashToBytes $
        Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff
