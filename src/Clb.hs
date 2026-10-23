{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clb (
  -- * Parameters
  X.defaultBabbageClbConfig,
  X.defaultConwayClbConfig,

  -- * CLB Monad
  Clb,
  ClbT (..),

  -- * CLB State
  ClbState (..),
  EmulatedLedgerState (..),
  chainState,
  clbConfig,
  knownDatums,
  clbLog,
  clbFailures,

  -- * CLB Runner
  runClb,
  runClbT,
  initClb,

  -- * Actions

  -- ** UTxO set
  currentUtxoState,
  getUtxosAt,
  txOutRefAt,
  txOutRefAtPaymentCred,

  -- * Tx submission (Haskell API)
  ValidationResult (..),
  CardanoTx,
  OnChainTx (..),
  submitTx,
  validateTx,

  -- ** Working with slots
  getCurrentSlot,
  waitSlot,
  modifySlot,

  -- ** Parameters and configuration
  getEpochInfo,
  getGlobals,
  getStakePools,
  getClbConfig,

  -- * Working with logs
  LogEntry (..),
  mkDebug,
  mkInfo,
  mkWarn,
  mkError,
  LogLevel (..),
  Log (Log),
  unLog,
  fromLog,
  logInfo,
  logFail,
  logError,

  -- * Log utils
  checkErrors,
  getFails,
  ppLog,
  dumpUtxoState,

  -- * Emulator configuration
  ClbConfig (..),
  SlotConfig (..),

  -- * Protocol parameters
  PParams,

  -- * key utils
  intToKeyPair,
  intToCardanoSk,

  -- * tx utility functions
  txWitnessDatums,
  txInlineDatums,
)
where

import Cardano.Api (mkAdaValue, shelleyBasedEra)
import Cardano.Api qualified as Api
import Cardano.Api qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.DSIGN qualified as Crypto
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Address qualified as L (compactAddr, decompactAddr)
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Api.Transition (EraTransition)
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as Core
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.Keys qualified as L
import Cardano.Ledger.Plutus.TxInfo (transCred, transDataHash, transTxIn)
import Cardano.Ledger.Shelley.API qualified as L hiding (TxOutCompact)
import Cardano.Ledger.Shelley.Core (EraRule)
import Cardano.Ledger.Shelley.LedgerState qualified as L
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo)
import Clb.Config (ClbConfig (..))
import Clb.Config qualified as X (defaultBabbageClbConfig, defaultConwayClbConfig)
import Clb.EmulatedLedgerState (
  EmulatedLedgerState (..),
  initialState,
  ledgerEnv,
  ledgerState,
  nextSlot,
  setSlot,
  setUtxo,
 )
import Clb.Era (CardanoLedgerEra, IsCardanoLedgerEra, maryBasedEra)
import Clb.Params (PParams, emulatorShelleyGenesisDefaults)
import Clb.TimeSlot (
  Slot (..),
  SlotConfig (..),
  fromSlot,
  posixTimeToUTCTime,
  slotConfigToEpochInfo,
  toSlot,
 )
import Clb.Tx (CardanoTx, OnChainTx (..))
import Control.Lens (makeLenses, over, (&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Identity (Identity)
import Control.Monad.State (MonadState, StateT, gets, modify, modify', runState, runStateT)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.State.Transition (STS (BaseM))
import Control.State.Transition.Extended (
  STS (Environment, Signal, State),
  ValidationPolicy (..),
 )
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List
import Data.Map qualified as M

import Cardano.Ledger.Binary (Interns, Share)
import Cardano.Ledger.State (InstantStake)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Text (Text)
import Data.Type.Ord (type (<=))
import GHC.TypeNats (CmpNat)
import PlutusLedgerApi.V1 qualified as PV1 (
  BuiltinData,
  Credential,
  Datum (..),
  DatumHash,
  TxOutRef,
  dataToBuiltinData,
 )
import PlutusLedgerApi.V2 qualified as PV2
import Prettyprinter (
  Doc,
  Pretty,
  colon,
  fillSep,
  hang,
  indent,
  pretty,
  vcat,
  vsep,
  (<+>),
 )
import Test.Cardano.Ledger.Core.KeyPair qualified as TL
import Text.Show.Pretty (ppShow)

--------------------------------------------------------------------------------
-- Base emulator types
--------------------------------------------------------------------------------

type ValidationError era = L.ApplyTxError (CardanoLedgerEra era)

data ValidationResult era
  = -- | A transaction failed to be validated by Ledger
    Fail !(Core.Tx Core.TopTx (CardanoLedgerEra era)) !(ValidationError era)
  | -- | New state and a validated transaction
    Success !(EmulatedLedgerState era) !(OnChainTx era)

deriving stock instance
  ( IsCardanoLedgerEra era
  , Show (Core.Tx Core.TopTx (CardanoLedgerEra era))
  , Show (L.CertState (C.ShelleyLedgerEra era))
  , Show (InstantStake (C.ShelleyLedgerEra era))
  ) =>
  Show (ValidationResult era)

--------------------------------------------------------------------------------
-- CLB base monad (instead of PSM's Run)
--------------------------------------------------------------------------------

newtype ClbT era m a = ClbT {unwrapClbT :: StateT (ClbState era) m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadState (ClbState era)
    , MonadTrans
    )

-- | State monad wrapper to run emulator.
type Clb era = ClbT era Identity

-- | CLB Emulator state: ledger state + some additional things
data ClbState era = ClbState
  { _chainState :: !(EmulatedLedgerState era)
  -- ^ the state of the ledger
  , _knownDatums :: !(M.Map PV1.DatumHash PV1.Datum)
  {- ^ datums: inline datums + presented as witnesses
  See also https://github.com/mlabs-haskell/clb/issues/45
  -}
  , _clbLog :: !(Log LogEntry)
  -- ^ The log of CLB, contains all log items, including failures.
  , _clbFailures :: !(Log FailReason)
  -- ^ Failures that occured.
  , _clbConfig :: !(ClbConfig era)
  {- ^ The configuration (supposed to be read-only)
  TODO: remove non-state parts like ClbConfig from the state?
  https://github.com/mlabs-haskell/clb/issues/63
  -}
  }

-- deriving stock (Show)

data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leMsg :: !String
  }
  deriving stock Show

mkDebug :: String -> LogEntry
mkDebug = LogEntry Debug

mkInfo :: String -> LogEntry
mkInfo = LogEntry Info

mkWarn :: String -> LogEntry
mkWarn = LogEntry Warning

mkError :: String -> LogEntry
mkError = LogEntry Error

data LogLevel = Debug | Info | Warning | Error
  deriving stock Show

instance Pretty LogLevel where
  pretty Debug = "[DEBUG]"
  pretty Info = "[ INFO]"
  pretty Warning = "[ WARN]"
  pretty Error = "[ERROR]"

instance Pretty LogEntry where
  pretty (LogEntry l msg) = pretty l <+> hang 1 msg'
   where
    ws = wordsIdent <$> lines msg
    wsD = fmap pretty <$> ws
    ls = fillSep <$> wsD
    msg' = vsep ls

-- | Like 'words' but keeps leading identation.
wordsIdent :: String -> [String]
wordsIdent s = takeWhile isSpace s : words s

-- | Fail reasons.
newtype FailReason
  = GenericFail String
  deriving Show

--------------------------------------------------------------------------------
-- Trace log (from PSM)
--------------------------------------------------------------------------------

-- | Log of Slot-timestamped events
newtype Log a = Log {unLog :: Seq (Slot, a)}
  deriving stock (Show, Functor)

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

ppLog :: Log LogEntry -> Doc ann
ppLog = vcat . fmap ppSlot . fromGroupLog
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

-- Lenses for ClbState
makeLenses ''ClbState

-- | Run CLB emulator.
runClb :: Clb era a -> ClbState era -> (a, ClbState era)
runClb (ClbT act) = runState act

runClbT :: ClbT era m a -> ClbState era -> m (a, ClbState era)
runClbT (ClbT act) = runStateT act

-- | Init emulator state.
initClb ::
  forall era.
  ( IsCardanoLedgerEra era
  , EraTransition (CardanoLedgerEra era)
  ) =>
  ClbConfig era ->
  -- TODO: Either (Api.Value, Api.Value) [(L.Addr L.StandardCrypto, L.Coin)] would be more accurate
  Api.Value ->
  Api.Value ->
  Maybe [(L.Addr, L.Coin)] ->
  ClbState era
initClb
  cfg@ClbConfig {clbConfigProtocol = pparams, clbConfigConfig = tc}
  _initVal
  walletFunds
  mInitFunds =
    ClbState
      { _chainState = setUtxo pparams utxos (initialState pparams tc)
      , _knownDatums = M.empty
      , _clbConfig = cfg
      , _clbLog = mempty
      , _clbFailures = mempty
      -- , _txPool = mempty
      -- , _chainNewestFirst = mempty
      }
   where
    utxos :: L.UTxO (CardanoLedgerEra era)
    utxos = case mInitFunds of
      Just shelleyInitFunds -> L.UTxO $ M.fromList $ mkGenesis $ zip shelleyInitFunds [1 ..]
      Nothing -> L.UTxO $ M.fromList $ mkWalletGenesis walletFunds <$> [1 .. 10]

    mkGenesis ::
      [((L.Addr, L.Coin), Integer)] ->
      [(L.TxIn, Core.TxOut (C.ShelleyLedgerEra era))]
    mkGenesis = fmap $
      \((a, c), i) ->
        ( L.mkTxInPartial genesisTxId i
        , L.mkBasicTxOut a $ mkAdaValue (shelleyBasedEra @era) c
        )

    mkWalletGenesis :: Api.Value -> Integer -> (L.TxIn, Core.TxOut (C.ShelleyLedgerEra era))
    mkWalletGenesis walletFund wallet =
      ( L.mkTxInPartial genesisTxId wallet
      , L.mkBasicTxOut
          (mkAddr' $ intToKeyPair wallet)
          (C.toLedgerValue (maryBasedEra @era) walletFund)
      )

    mkAddr' :: TL.KeyPair L.Payment -> L.Addr
    mkAddr' payKey = L.Addr L.Testnet (TL.mkCredential payKey) L.StakeRefNull

    -- \| genesis transaction ID
    genesisTxId :: L.TxId
    genesisTxId = L.TxId $ L.unsafeMakeSafeHash dummyHash

    -- Hash for genesis transaction
    dummyHash :: Crypto.Hash Crypto.Blake2b_256 Core.EraIndependentTxBody
    dummyHash = Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

--------------------------------------------------------------------------------
-- Actions in Clb monad
--------------------------------------------------------------------------------

getClbConfig :: Monad m => ClbT era m (ClbConfig era)
getClbConfig = gets (^. clbConfig)

getCurrentSlot :: Monad m => ClbT era m C.SlotNo
getCurrentSlot = gets $ L.ledgerSlotNo . (^. chainState . ledgerEnv)

getStakePools ::
  ( Monad m
  , L.MinVersion <= Core.ProtVerHigh (CardanoLedgerEra era)
  , L.MinVersion <= Core.ProtVerLow (CardanoLedgerEra era)
  , Core.ProtVerLow (CardanoLedgerEra era) <= Core.ProtVerHigh (CardanoLedgerEra era)
  , CmpNat (Core.ProtVerHigh (CardanoLedgerEra era)) L.MaxVersion ~ LT
  , CmpNat (Core.ProtVerLow (CardanoLedgerEra era)) L.MaxVersion ~ LT
  , Share (L.CertState (CardanoLedgerEra era))
      ~ ( Interns (L.Credential L.Staking)
        , Interns (L.KeyHash L.StakePool)
        , Interns (L.Credential L.DRepRole)
        , Interns (L.Credential L.HotCommitteeRole)
        )
  , L.EraCertState (CardanoLedgerEra era)
  ) =>
  ClbT era m (Set (L.KeyHash L.StakePool))
getStakePools =
  gets $
    M.keysSet
      . L.psStakePools
      . (^. L.certPStateL)
      . L.lsCertState
      . (^. chainState . ledgerState)

-- | Log a generic (non-typed) error.
logError :: Monad m => String -> ClbT era m ()
logError msg = do
  logInfo $ LogEntry Error msg
  logFail $ GenericFail msg

-- | Add a non-error log enty.
logInfo :: Monad m => LogEntry -> ClbT era m ()
logInfo le = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ over clbLog $ appendLog slot le

-- | Log failure.
logFail :: Monad m => FailReason -> ClbT era m ()
logFail res = do
  C.SlotNo slotNo <- getCurrentSlot
  let slot = Slot $ toInteger slotNo
  modify' $ over clbFailures $ appendLog slot res

-- | Insert event to log
appendLog :: Slot -> a -> Log a -> Log a
appendLog slot val (Log xs) = Log (xs Seq.|> (slot, val))

-- Read
getUtxosAt ::
  (Monad m, Core.EraTxOut (CardanoLedgerEra era)) =>
  L.Addr ->
  ClbT era m (L.UTxO (CardanoLedgerEra era))
getUtxosAt addr = do
  allUTxOs <- gets currentUtxoState
  let hasAddr txOut = txOut ^. L.addrTxOutL == addr
  pure . L.UTxO $ M.filter hasAddr $ L.unUTxO allUTxOs

-- Returns the complete utx state in the emulated ledger state
currentUtxoState :: ClbState era -> L.UTxO (CardanoLedgerEra era)
currentUtxoState =
  (^. chainState . ledgerState . L.lsUTxOStateL . L.utxoL)

-- | Read all TxOutRefs that belong to given address.
txOutRefAt ::
  forall era m.
  (Monad m, IsCardanoLedgerEra era) =>
  C.AddressInEra era ->
  ClbT era m [PV1.TxOutRef]
txOutRefAt (C.toShelleyAddr -> addr) = gets txOutRefAt'
 where
  txOutRefAt' st = transTxIn <$> M.keys (M.filter atAddr utxos)
   where
    utxos = L.unUTxO $ currentUtxoState st
    atAddr out = case out ^. Core.addrEitherTxOutL of
      Right cAddr -> L.compactAddr addr == cAddr
      -- Left should never happen (abuse of Either in fact)
      Left addr' -> addr == addr'

-- | Read all TxOutRefs that belong to given payment credential.
txOutRefAtPaymentCred :: forall era m. Monad m => IsCardanoLedgerEra era => PV1.Credential -> ClbT era m [PV1.TxOutRef]
txOutRefAtPaymentCred cred = gets txOutRefAtPaymentCredState
 where
  txOutRefAtPaymentCredState :: IsCardanoLedgerEra era => ClbState era -> [PV1.TxOutRef]
  txOutRefAtPaymentCredState st = fmap transTxIn . M.keys $ M.filter withCred utxos
   where
    withCred out = case out ^. Core.addrEitherTxOutL of
      Right addr -> addrHasCred $ L.decompactAddr addr
      Left addr -> addrHasCred addr
    addrHasCred (L.Addr _ paymentCred _) = transCred paymentCred == cred
    -- Not considering byron addresses.
    addrHasCred _ = False
    utxos = L.unUTxO $ currentUtxoState st

getEpochInfo :: Monad m => ClbT era m (EpochInfo (Either Text))
getEpochInfo =
  gets (slotConfigToEpochInfo . clbConfigSlotConfig . _clbConfig)

getGlobals :: (Monad m, IsCardanoLedgerEra era) => ClbT era m Globals
getGlobals = do
  startTime <- gets (scSlotZeroTime . clbConfigSlotConfig . _clbConfig)
  L.mkShelleyGlobals
    ( emulatorShelleyGenesisDefaults
        { L.sgSystemStart = posixTimeToUTCTime startTime
        }
    )
    <$> getEpochInfo

{- | The main tx submission mechanism (for "as a library mode").
Takes a transaction, validates it against the latest blockchain state.
Applies it if it is valid, indicating an error otherwise.
Also:
  * Updates datums cache with datums used as witnesses
  * Moves to the next slot
-}
submitTx ::
  forall era m.
  ( Monad m
  , IsCardanoLedgerEra era
  , STS (EraRule "LEDGER" (C.ShelleyLedgerEra era))
  , BaseM (EraRule "LEDGER" (CardanoLedgerEra era)) ~ L.ShelleyBase
  , Environment (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolEnv (C.ShelleyLedgerEra era)
  , Core.Tx Core.TopTx (C.ShelleyLedgerEra era) ~ Signal (EraRule "LEDGER" (C.ShelleyLedgerEra era))
  , State (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolState (C.ShelleyLedgerEra era)
  ) =>
  C.Tx era ->
  ClbT era m (ValidationResult era)
submitTx apiTx@(C.ShelleyTx _ tx) = do
  state <- gets (^. chainState)
  globals <- getGlobals
  case applyTx ValidateAll globals state tx of
    Right (newState, vtx) -> do
      let txBody = C.getTxBody apiTx
      let txDatums = txWitnessDatums txBody <> txInlineDatums txBody
      modify $
        over chainState (const $ nextSlot newState)
          . over knownDatums (`M.union` txDatums)
      return $ Success newState vtx
    Left err -> return $ Fail tx err

-- | Validate a tx against 'EmulatedLedgerState' given some 'Globals'.
validateTx ::
  ( IsCardanoLedgerEra era
  , STS (EraRule "LEDGER" (C.ShelleyLedgerEra era))
  , BaseM (EraRule "LEDGER" (CardanoLedgerEra era)) ~ L.ShelleyBase
  , Environment (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolEnv (C.ShelleyLedgerEra era)
  , Core.Tx Core.TopTx (C.ShelleyLedgerEra era) ~ Signal (EraRule "LEDGER" (C.ShelleyLedgerEra era))
  , State (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolState (C.ShelleyLedgerEra era)
  ) =>
  Globals ->
  EmulatedLedgerState era ->
  C.Tx era ->
  ValidationResult era
validateTx globals els (C.ShelleyTx _ tx) =
  case applyTx ValidateAll globals els tx of
    Right (newState, vtx) -> Success newState vtx
    Left err -> Fail tx err

-- | Returns all inline datums from transaction outputs (along with their hashes).
txInlineDatums :: forall era. C.TxBody era -> M.Map PV1.DatumHash PV1.Datum
txInlineDatums txb =
  let C.TxBodyContent {txOuts} = C.getTxBodyContent txb
      inlineDatums = flip mapMaybe txOuts $
        \(C.TxOut _ _ d _) -> case d of
          C.TxOutDatumNone -> Nothing
          C.TxOutDatumHash _ _ -> Nothing
          C.TxOutDatumInline _ sd -> Just sd
          C.TxOutSupplementalDatum _ _ -> Nothing
      datums =
        M.fromList
          ( (\d -> (datumHash d, d))
              . PV1.Datum
              . fromCardanoScriptData
              . C.getScriptData
              <$> inlineDatums
          )
   in datums

-- | Returns wintess datums along with their hashes.
txWitnessDatums :: C.TxBody era -> M.Map PV1.DatumHash PV1.Datum
txWitnessDatums (C.ShelleyTxBody _ _ _ C.TxBodyNoScriptData _ _) = mempty
txWitnessDatums (C.ShelleyTxBody _ _ _ (C.TxBodyScriptData _ (L.TxDats dats) _) _ _) =
  let datums =
        M.fromList
          ( (\d -> (datumHash d, d))
              . PV1.Datum
              . fromCardanoScriptData
              . C.getScriptData
              . C.fromAlonzoData
              <$> M.elems dats
          )
   in datums

fromCardanoScriptData :: C.ScriptData -> PV1.BuiltinData
fromCardanoScriptData = PV1.dataToBuiltinData . C.toPlutusData

datumHash :: PV2.Datum -> PV2.DatumHash
datumHash (PV2.Datum (PV2.BuiltinData dat)) =
  transDataHash $ L.hashData $ L.Data @L.AlonzoEra dat

dumpUtxoState ::
  forall m era.
  ( IsCardanoLedgerEra era
  , Monad m
  , Show (L.CertState (C.ShelleyLedgerEra era))
  , Show (InstantStake (C.ShelleyLedgerEra era))
  ) =>
  ClbT era m ()
dumpUtxoState = do
  s <- gets (^. chainState . ledgerState)
  logInfo $ LogEntry Info $ ppShow s

--------------------------------------------------------------------------------
-- Helpers for working with emulator traces
--------------------------------------------------------------------------------

{- | Checks that script runs without errors and returns pretty printed failure
 if something bad happens.
-}
checkErrors :: Monad m => ClbT era m (Maybe String)
checkErrors = do
  failures <- fromLog <$> getFails
  pure $
    if null failures
      then Nothing
      else Just (init . unlines $ fmap show failures)

-- | Return list of failures
getFails :: Monad m => ClbT era m (Log FailReason)
getFails = gets _clbFailures

--------------------------------------------------------------------------------
-- Transactions validation TODO: factor out
--------------------------------------------------------------------------------

{- | Code copy-pasted from ledger's `applyTx` to use custom `ApplySTSOpts`.
Validates a transaction against a 'EmulatedLedgerState' and returns the
updated state and the transaction that "made it to the ledger/state".
-}
applyTx ::
  forall era.
  ( STS (EraRule "LEDGER" (CardanoLedgerEra era))
  , IsCardanoLedgerEra era
  , BaseM (EraRule "LEDGER" (CardanoLedgerEra era)) ~ L.ShelleyBase
  , Environment (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolEnv (C.ShelleyLedgerEra era)
  , Core.Tx Core.TopTx (C.ShelleyLedgerEra era) ~ Signal (EraRule "LEDGER" (C.ShelleyLedgerEra era))
  , State (EraRule "LEDGER" (C.ShelleyLedgerEra era)) ~ L.MempoolState (C.ShelleyLedgerEra era)
  ) =>
  ValidationPolicy ->
  L.Globals ->
  EmulatedLedgerState era ->
  Core.Tx Core.TopTx (CardanoLedgerEra era) ->
  Either (L.ApplyTxError (CardanoLedgerEra era)) (EmulatedLedgerState era, OnChainTx era)
applyTx asoValidation globals currState tx = do
  (newState, vtx) <-
    L.applyTxValidation
      asoValidation
      globals
      (currState ^. ledgerEnv)
      (currState ^. ledgerState)
      tx
  pure (currState & ledgerState .~ newState, OnChainTx vtx)

--------------------------------------------------------------------------------
-- Key utils
--------------------------------------------------------------------------------

-- | Create key pair from an integer (deterministic)
intToKeyPair :: Integer -> TL.KeyPair r
intToKeyPair n = TL.KeyPair vk sk
 where
  sk = Crypto.genKeyDSIGN $ mkSeedFromInteger n
  vk = L.VKey $ Crypto.deriveVerKeyDSIGN sk

  -- \| Construct a seed from a bunch of Word64s
  --
  --      We multiply these words by some extra stuff to make sure they contain
  --      enough bits for our seed.
  --
  mkSeedFromInteger :: Integer -> Crypto.Seed
  mkSeedFromInteger stuff =
    Crypto.mkSeedFromBytes . Crypto.hashToBytes $
      Crypto.hashWithSerialiser @Crypto.Blake2b_256 CBOR.toCBOR stuff

intToCardanoSk :: Integer -> C.SigningKey C.PaymentKey
intToCardanoSk n = case intToKeyPair n of
  TL.KeyPair _ sk -> C.PaymentSigningKey sk

waitSlot :: forall era m. Monad m => SlotNo -> ClbT era m ()
waitSlot slot = do
  currSlot <- getCurrentSlot
  -- TODO: shall we throw?
  when (currSlot < slot) $
    modify $
      over chainState $
        setSlot slot

-- Additional actions

modifySlot :: Monad m => (Slot -> Slot) -> ClbT era m Slot
modifySlot f = do
  logInfo $ LogEntry Info "modify slot..."
  newSlot <- f . toSlot <$> getCurrentSlot
  modify $ over chainState $ setSlot $ fromSlot newSlot
  return newSlot
