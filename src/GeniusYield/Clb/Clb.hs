module GeniusYield.Clb.Clb where

import Control.Monad.State (State, MonadState (get), gets, runState, modify', modify, put)
import Cardano.Api qualified as C
import GeniusYield.Clb.MockConfig (MockConfig (..))
import PlutusLedgerApi.V1 (DatumHash, TxOutRef, Credential)
import PlutusLedgerApi.V1 qualified as P (Datum)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseInfo, assertFailure)
import Data.Function (on)
import Data.Foldable (toList)
import Prettyprinter
import GeniusYield.Clb.ClbLedgerState (EmulatedLedgerState (..), initialState, setUtxo, memPoolState, currentBlock)
import GeniusYield.Clb.Params (PParams(BabbageParams, AlonzoParams), mkGlobals, babbageOnly)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.TxIn (TxId (..), TxIn (..), mkTxInPartial)
import GeniusYield.Clb.Era (EmulatorEra)
import qualified Cardano.Ledger.SafeHash as L
import Cardano.Crypto.Hash qualified as Crypto
import qualified Cardano.Ledger.Core as L
import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut(TxOutCompact), getEitherAddrBabbageTxOut)
import Cardano.Ledger.Api (Crypto, Addr (..), RdmrPtr)
import Cardano.Ledger.Compactible (toCompact)
import Data.Maybe (fromJust)
import Test.Cardano.Ledger.Core.KeyPair qualified as TC
import qualified Cardano.Crypto.DSIGN as C
import qualified Cardano.Ledger.Keys as C
import qualified Cardano.Crypto.Seed as C
import Cardano.Ledger.Address (compactAddr)
import Test.Cardano.Ledger.Core.KeyPair (mkCred)
import GeniusYield.Types.Value (GYValue, valueToApi)
import Cardano.Api.Shelley (toMaryValue, toShelleyAddr, fromShelleyTxIn)
import qualified Cardano.Api as Api
import Cardano.Ledger.Shelley.API (LedgerState(..), UTxOState (utxosUtxo), StakeReference (..))
import GeniusYield.Types.TxOutRef (txOutRefToPlutus, txOutRefFromApi)
import Cardano.Ledger.BaseTypes (Network (Testnet), Globals, mkVersion)
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusLedgerApi.V1.Scripts (ScriptError)
import qualified Cardano.Ledger.Shelley.API as L
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Api.Tx qualified as Api (Tx(ShelleyTx))
import Cardano.Slotting.Slot (SlotNo)
import Data.Bifunctor (first)
import Control.Lens (over, (&), (.~))
import GeniusYield.Clb.TimeSlot (slotLength)
import qualified Cardano.Api.Shelley as C
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Identity (Identity, runIdentity)



-- | State monad wrapper to run blockchain.
newtype Clb a = Clb (State ClbState a)
  deriving newtype (Functor, Applicative, Monad, MonadState ClbState)

data ClbState = ClbState
  { emulatedLedgerState :: EmulatedLedgerState
  , mockConfig :: !MockConfig
  , mockDatums :: !(Map DatumHash P.Datum)
  -- , mockAddresses :: !(Map Address (Set TxOutRef))
  , mockInfo :: !(Log String)
  , mockCurrentSlot :: !Slot
  }

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

newtype Slot = Slot {getSlot :: Integer}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Enum, Real, Integral)

instance Pretty Slot where
  pretty (Slot i) = "Slot" <+> pretty i

-- | Run blockchain.
runMock :: Clb a -> ClbState -> (a, ClbState)
runMock (Clb act) = runState act

-- | Init blockchain state.
initMock :: MockConfig -> GYValue -> ClbState
initMock MockConfig{mockConfigProtocol = (AlonzoParams _)} _ = error "Unsupported params"
initMock cfg@MockConfig{mockConfigProtocol = params@(BabbageParams pparams)} initVal =
  ClbState
    {
      emulatedLedgerState = setUtxo pparams utxos (initialState params)
    , mockDatums = M.empty
    -- , mockAddresses = M.empty -- M.singleton genesisAddress (S.singleton genesisTxOutRef)
    , mockConfig = cfg
    , mockCurrentSlot = Slot 1
    , mockInfo = mempty
    }
  where

    utxos = UTxO $ M.fromList [genesis]

    genesis :: (TxIn (Core.EraCrypto EmulatorEra), Core.TxOut EmulatorEra)
    genesis =
      ( mkTxInPartial genesisTxId 0
      , TxOutCompact
        (compactAddr $ mkAddr' $ intToKeyPair 0)
        (fromJust $ toCompact $ toMaryValue $ valueToApi initVal)
      )

    mkAddr' payKey = Addr Testnet (mkCred payKey) StakeRefNull

    -- | genesis transaction ID
    genesisTxId :: TxId StandardCrypto
    genesisTxId = TxId $ L.unsafeMakeSafeHash dummyHash

    -- Hash for genesis transaction
    dummyHash :: Crypto.Hash Crypto.Blake2b_256 L.EraIndependentTxBody
    dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

-- | Create User out of integer
intToKeyPair :: Crypto c => Integer -> TC.KeyPair r c
intToKeyPair n = TC.KeyPair vk sk
  where
    sk = C.genKeyDSIGN $ mkSeedFromInteger n
    vk = C.VKey $ C.deriveVerKeyDSIGN sk

{- | Construct a seed from a bunch of Word64s

  We multiply these words by some extra stuff to make sure they contain
  enough bits for our seed.
-}
mkSeedFromInteger :: Integer -> C.Seed
mkSeedFromInteger stuff =
  C.mkSeedFromBytes . Crypto.hashToBytes $
    Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff

-- | Cardano tx from any era.
data CardanoTx where
  CardanoTx :: C.IsCardanoEra era => C.Tx era -> C.EraInMode era C.CardanoMode -> CardanoTx

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
txOutRefAt :: Api.AddressInEra Api.BabbageEra -> Clb [TxOutRef]
txOutRefAt addr = gets (txOutRefAtState $ toShelleyAddr addr)

-- | Read all TxOutRefs that belong to given address.
txOutRefAtState :: Addr StandardCrypto -> ClbState -> [TxOutRef]
txOutRefAtState addr st =
  txOutRefToPlutus . txOutRefFromApi . fromShelleyTxIn
    <$> M.keys (M.filter atAddr utxos)
  where
    utxos = unUTxO $ utxosUtxo $ lsUTxOState $ _memPoolState $ emulatedLedgerState st

    atAddr :: BabbageTxOut EmulatorEra -> Bool
    atAddr out = case getEitherAddrBabbageTxOut out of
      Left addr' -> addr == addr'
      Right cAddr -> compactAddr addr == cAddr

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: Credential -> Clb [TxOutRef]
txOutRefAtPaymentCred cred = gets (txOutRefAtPaymentCredState cred)

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCredState :: Credential -> ClbState -> [TxOutRef]
txOutRefAtPaymentCredState cred st =
  undefined
  -- S.toList $ M.foldrWithKey
  --   (\addr s acc -> if addressCredential addr == cred then s <> acc else acc)
  --   S.empty
  --   $ mockAddresses st

testNoErrorsTraceClb :: GYValue -> MockConfig -> String -> Clb a -> TestTree
testNoErrorsTraceClb funds cfg msg act =
  testCaseInfo msg $ do
    logg <- maybe (pure mockLog) assertFailure errors
    pure $ logg <> "\nState: " <> show (emulatedLedgerState mock)
  where
    (errors, mock) = runMock (act >> checkErrors) $ initMock cfg funds
    mockLog = "\nBlockchain log :\n----------------\n" <> ppMockEvent (mockInfo mock)

ppMockEvent :: Log String -> String
ppMockEvent = show . vcat . fmap ppSlot . fromGroupLog
  where
    ppSlot (slot, events) = vcat [pretty slot <> colon, indent 2 (vcat $ pretty <$> events)]


fromGroupLog :: Log a -> [(Slot, [a])]
fromGroupLog = fmap toGroup . L.groupBy ((==) `on` fst) . fromLog
  where
    toGroup ((a, b) : rest) = (a, b : fmap snd rest)
    toGroup [] = error "toGroup: Empty list"

-- | Convert log to plain list
fromLog :: Log a -> [(Slot, a)]
fromLog (Log s) = toList s

checkErrors :: Clb (Maybe String)
checkErrors = do
  _ <- get
  pure Nothing


--------------------------------------------------------------------------------
-- Sendind and validating transactions

sendTx :: Api.Tx Api.BabbageEra -> Clb ValidationResult
sendTx (Api.ShelleyTx _ tx) = do -- FIXME: use patterns, but not cardano-api:internal!
  state@ClbState{mockConfig=MockConfig{mockConfigProtocol, mockConfigSlotConfig}} <- get
  -- FIXME: fromJust
  let majorVer = fromJust $ runIdentity $ runMaybeT @Identity $ mkVersion $ fst $ C.protocolParamProtocolVersion
                  $ C.fromLedgerPParams C.ShelleyBasedEraBabbage $ babbageOnly mockConfigProtocol
  let globals = mkGlobals (slotLength mockConfigSlotConfig) majorVer
  let ret = validateCardanoTx
        globals
        (emulatedLedgerState state)
        tx
  case ret of
    Success newState _ -> put $ state { emulatedLedgerState = newState }
    FailPhase1 _ err -> error $ "Ahhh" <> show err
    FailPhase2 _ err _ -> error $ "Ahhh" <> show err
  pure ret
newtype OnChainTx = OnChainTx { getOnChainTx :: L.Validated (L.Tx EmulatorEra) }

data ValidationResult
  = FailPhase1 !CardanoTx !ValidationError
  -- ^ A transaction failed to validate in phase 1.
  | FailPhase2 !OnChainTx !ValidationError !(MaryValue StandardCrypto)
  -- ^ A transaction failed to validate in phase 2. The @Value@ indicates the amount of collateral stored in the transaction.
  | Success !EmulatedLedgerState !OnChainTx -- !RedeemerReport
  -- deriving stock (Eq, Show, Generic)
  | Dummy

type RedeemerReport = M.Map RdmrPtr ([Text], ExUnits)

-- | A reason why a transaction is invalid.
data ValidationError =
    TxOutRefNotFound -- TxIn
    -- ^ The transaction output consumed by a transaction input could not be found (either because it was already spent, or because
    -- there was no transaction with the given hash on the blockchain).
    | ScriptFailure ScriptError
    -- ^ For pay-to-script outputs: evaluation of the validator script failed.
    | CardanoLedgerValidationError Text
    -- ^ An error from Cardano.Ledger validation
    | MaxCollateralInputsExceeded
    -- ^ Balancing failed, it needed more than the maximum number of collateral inputs
    deriving (Eq, Show)

data ValidationPhase = Phase1 | Phase2 deriving (Eq, Show) -- , Generic) -- , FromJSON, ToJSON)

type ValidationErrorInPhase = (ValidationPhase, ValidationError)

validateCardanoTx :: Globals -> EmulatedLedgerState -> L.Tx EmulatorEra -> ValidationResult
validateCardanoTx =
  -- TODO: genesis transactions
  -- if txIns tx == [genesisTxIn]
  --   then Success (unsafeMakeValid tx) M.empty
  --   else hasValidationErrors params (fromIntegral slot) index (getEmulatorEraTx tx)
  hasValidationErrors --(fromIntegral slot)

hasValidationErrors :: Globals -> EmulatedLedgerState -> L.Tx EmulatorEra -> ValidationResult
hasValidationErrors globals state  tx =
  case res of
    -- FIXME: why Phase1?
    Left err -> FailPhase1 (CardanoTx (Api.ShelleyTx Api.ShelleyBasedEraBabbage tx) C.BabbageEraInCardanoMode) err
    Right (newState, vtx) -> Success newState vtx

    -- case getTxExUnitsWithLogs emulatorPParams utxo tx of
    --   Left (Phase1, err) -> FailPhase1 (CardanoTx tx ShelleyBasedEraBabbage) err
    --   Left (Phase2, err) -> FailPhase2 vtx err $ getCollateral index tx
    --   Right report         -> Success vtx report
  where
    res = fmap OnChainTx <$> applyTx globals state tx

-- | A wrapper around the ledger's applyTx
-- TODO: step slot, since this is not ledger's responsibility!
applyTx ::
  Globals ->
  EmulatedLedgerState ->
  L.Tx EmulatorEra ->
  Either ValidationError (EmulatedLedgerState, L.Validated (Core.Tx EmulatorEra))
applyTx globals oldState@EmulatedLedgerState{_ledgerEnv, _memPoolState} tx = do
  (newMempool, vtx) <-
    first (CardanoLedgerValidationError . Text.pack . show)
    (L.applyTx globals _ledgerEnv _memPoolState tx)
  pure (oldState & memPoolState .~ newMempool & over currentBlock (vtx :), vtx)
