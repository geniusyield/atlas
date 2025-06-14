{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Test.Clb
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Clb (
  GYTxMonadClbT,
  GYTxMonadClb,
  mkTestForT,
  mkTestFor,
  asClb,
  asRandClb,
  liftClb,
  dumpUtxoState,
  mustFail,
  mustFailWith,
  sendSkeleton,
  sendSkeleton',
  logInfoS,
) where

import Control.Monad.Except (ExceptT, runExceptT, tryError)
import Control.Monad.Random (StdGen, RandT, mkStdGen, evalRandT)
import Control.Monad.Reader (MonadReader, ReaderT, local, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, get, put, runStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict qualified as Map
import Data.SOP.NonEmpty (NonEmpty (NonEmptyCons, NonEmptyOne))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock (
  NominalDiffTime,
  UTCTime,
 )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Conway.Core qualified as ConwayCore
import Cardano.Ledger.Shelley.API qualified as L.S
import Cardano.Ledger.State qualified as L
import Cardano.Slotting.Slot (
  EpochNo (..),
  EpochSize (..),
 )
import Cardano.Slotting.Time (
  RelativeTime (RelativeTime),
  mkSlotLength,
 )
import Clb (
  ClbConfig (..),
  ClbState (..),
  ClbT,
  EmulatedLedgerState (..),
  Log (Log),
  LogEntry (LogEntry),
  LogLevel (..),
  SlotConfig (..),
  ValidationResult (..),
  getCurrentSlot,
  getFails,
  logError,
  logInfo,
  txOutRefAt,
  txOutRefAtPaymentCred,
  unLog,
  waitSlot,
 )
import Clb qualified
import Control.Monad.Trans.Maybe (runMaybeT)
import Ouroboros.Consensus.Cardano.Block qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (eraGenesisWin))
import PlutusLedgerApi.V2 qualified as Plutus
import Prettyprinter (
  PageWidth (AvailablePerLine),
  defaultLayoutOptions,
  layoutPageWidth,
  layoutPretty,
 )
import Prettyprinter.Render.String (renderString)
import Test.Cardano.Ledger.Core.KeyPair qualified as TL
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (
  assertFailure,
  testCaseInfo,
 )

import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder.Class
import GeniusYield.TxBuilder.Common
import GeniusYield.TxBuilder.Errors
import GeniusYield.TxBuilder.User
import GeniusYield.Types

deriving newtype instance Num EpochSize
deriving newtype instance Num EpochNo

newtype GYTxClbEnv = GYTxClbEnv
  { clbEnvWallet :: User
  -- ^ The actor for a GYTxMonadClb action.
  }

newtype GYTxClbState = GYTxClbState
  { clbNextWalletInt :: Integer
  -- ^ Next integer to use with 'Clb.intToKeyPair' call in order to generate a new user.
  }

type GYTxMonadClb = GYTxMonadClbT Identity

newtype GYTxMonadClbT m a = GYTxMonadClbT
  { unGYTxMonadClbT :: ReaderT GYTxClbEnv (StateT GYTxClbState (ExceptT GYTxMonadException (RandT StdGen (ClbT ApiEra m)))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader GYTxClbEnv, MonadState GYTxClbState)
  deriving anyclass GYTxBuilderMonad

instance MonadTrans GYTxMonadClbT where
  lift = GYTxMonadClbT . lift . lift . lift . lift . lift

instance Monad m => MonadRandom (GYTxMonadClbT m) where
  getRandomR = GYTxMonadClbT . getRandomR
  getRandom = GYTxMonadClbT getRandom
  getRandomRs = GYTxMonadClbT . getRandomRs
  getRandoms = GYTxMonadClbT getRandoms

asRandClb ::
  forall a m.
  Monad m =>
  User ->
  Integer ->
  GYTxMonadClbT m a ->
  RandT StdGen (ClbT ApiEra m) (Maybe a)
asRandClb w i m = do
  e <- runExceptT $ (unGYTxMonadClbT m `runReaderT` GYTxClbEnv w) `runStateT` GYTxClbState {clbNextWalletInt = i}
  case e of
    Left (GYApplicationException (toApiError -> GYApiError {gaeMsg})) -> lift (logError $ T.unpack gaeMsg) >> return Nothing
    Left err -> lift (logError $ show err) >> return Nothing
    Right (a, _) -> return $ Just a

asClb ::
  forall a m.
  Monad m =>
  StdGen ->
  User ->
  Integer ->
  GYTxMonadClbT m a ->
  ClbT ApiEra m (Maybe a)
asClb g w i m = evalRandT (asRandClb w i m) g

liftClb :: forall a m. Monad m => ClbT ApiEra m a -> GYTxMonadClbT m a
liftClb = GYTxMonadClbT . lift . lift . lift . lift

{- | Given a test name, runs the trace for every wallet, checking there weren't
     errors.
-}
mkTestFor :: forall a. String -> (TestInfo -> GYTxMonadClb a) -> Tasty.TestTree
mkTestFor name = runIdentity . mkTestForT name

mkTestForT :: forall a m. Monad m => String -> (TestInfo -> GYTxMonadClbT m a) -> m Tasty.TestTree
mkTestForT name action =
  testNoErrorsTraceClb v w Clb.defaultConwayClbConfig name $ do
    asClb pureGen (w1 testWallets) nextWalletInt $
      action TestInfo {testGoldAsset = fakeCoin fakeGold, testIronAsset = fakeCoin fakeIron, testWallets}
 where
  -- TODO (simplify-genesis): Remove generation of non ada funds.
  v =
    valueFromLovelace 1_000_000_000_000_000
      <> fakeValue fakeGold 1_000_000_000
      <> fakeValue fakeIron 1_000_000_000

  w =
    valueFromLovelace 1_000_000_000_000
      <> fakeValue fakeGold 1_000_000
      <> fakeValue fakeIron 1_000_000

  -- TODO (simplify-genesis):: Remove creation of wallets. Only create one (or more) genesis/funder wallet and pass it on.
  testWallets :: Wallets
  testWallets =
    Wallets
      (mkSimpleWallet (Clb.intToKeyPair 1))
      (mkSimpleWallet (Clb.intToKeyPair 2))
      (mkSimpleWallet (Clb.intToKeyPair 3))
      (mkSimpleWallet (Clb.intToKeyPair 4))
      (mkSimpleWallet (Clb.intToKeyPair 5))
      (mkSimpleWallet (Clb.intToKeyPair 6))
      (mkSimpleWallet (Clb.intToKeyPair 7))
      (mkSimpleWallet (Clb.intToKeyPair 8))
      (mkSimpleWallet (Clb.intToKeyPair 9))

  -- This is the next consecutive number after the highest one used above for 'Clb.intToKeyPair' calls.
  nextWalletInt :: Integer
  nextWalletInt = 10

  -- \| Helper for building tests
  testNoErrorsTraceClb :: forall b. GYValue -> GYValue -> Clb.ClbConfig ApiEra -> String -> ClbT ApiEra m b -> m Tasty.TestTree
  testNoErrorsTraceClb funds walletFunds cfg msg act = do
    (mbErrors, mock) <- Clb.runClbT (act >> Clb.checkErrors) $ Clb.initClb cfg (valueToApi funds) (valueToApi walletFunds) Nothing
    let
      mockLog = "\nEmulator log :\n--------------\n" <> logString
      options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
      logDoc = Clb.ppLog $ Clb._clbLog mock
      logString = renderString $ layoutPretty options logDoc
    pure $ testCaseInfo msg $
      maybe (pure mockLog) assertFailure $
        mbErrors >>= \errors -> pure (mockLog <> "\n\nError :\n-------\n" <> errors)

mkSimpleWallet :: TL.KeyPair L.S.Payment -> User
mkSimpleWallet kp =
  let key = paymentSigningKeyFromLedgerKeyPair kp
   in User'
        { userPaymentSKey' = key
        , userStakeSKey' = Nothing
        , userAddr =
            addressFromPaymentKeyHash GYTestnetPreprod . paymentKeyHash $
              paymentVerificationKey key
        }

{- | Try to execute an action, and if it fails, restore to the current state
 while preserving logs. If the action succeeds, logs an error as we expect
 it to fail. Use 'mustFailWith' to provide custom
 error message or/and failure action name.
 FIXME: should we move it to CLB?
-}
mustFail :: forall a m. Monad m => GYTxMonadClbT m a -> GYTxMonadClbT m ()
mustFail = mustFailWith (const True)

mustFailWith :: forall a m. Monad m => (GYTxMonadException -> Bool) -> GYTxMonadClbT m a -> GYTxMonadClbT m ()
mustFailWith isExpectedError act = do
  (st, preFails) <- liftClb $ do
    st <- get
    preFails <- getFails
    pure (st, preFails)
  tryError (void act) >>= \case
    Left e@(isExpectedError -> True) -> do
      gyLogInfo' "" . printf "Successfully caught expected exception %s" $ show e
      infoLog <- liftClb $ gets _clbLog
      postFails <- liftClb getFails
      liftClb $
        put
          st
            { _clbLog = infoLog <> mkMustFailLog preFails postFails
            -- , mustFailLog = mkMustFailLog preFails postFails
            }
    Left err -> liftClb $ logError $ "Action failed with unexpected exception: " ++ show err
    Right _ -> liftClb $ logError "Expected action to fail but it succeeds"
 where
  mkMustFailLog (unLog -> pre) (unLog -> post) =
    Log $ second (LogEntry Error . ((msg <> ":") <>) . show) <$> Seq.drop (Seq.length pre) post
  msg = "Unnamed failure action"

instance Monad m => MonadError GYTxMonadException (GYTxMonadClbT m) where
  throwError = GYTxMonadClbT . throwError

  catchError m handler = GYTxMonadClbT . catchError (unGYTxMonadClbT m) $ unGYTxMonadClbT . handler

allUTxOs :: Monad m => GYTxMonadClbT m GYUTxOs
allUTxOs = do
  utxos <- liftClb $ gets (L.unUTxO . L.S.utxosUtxo . L.S.lsUTxOState . _ledgerState . _chainState)
  pure $ utxosFromList $ map (\(ref, o) -> utxoFromApi' (Api.S.fromShelleyTxIn ref) (Api.S.fromShelleyTxOut Api.ShelleyBasedEraConway o)) $ Map.toList utxos

instance Monad m => GYTxQueryMonad (GYTxMonadClbT m) where
  networkId = do
    magic <- liftClb $ gets (clbConfigNetworkId . _clbConfig)
    -- TODO: Add epoch slots and network era to clb and retrieve from there.
    pure . GYPrivnet $
      GYNetworkInfo
        { gyNetworkMagic = Api.S.unNetworkMagic $ Api.S.toNetworkMagic magic
        , gyNetworkEpochSlots = 500
        }

  lookupDatum :: GYDatumHash -> GYTxMonadClbT m (Maybe GYDatum)
  lookupDatum h = liftClb $ do
    mdh <- gets _knownDatums
    return $ do
      d <- Map.lookup (datumHashToPlutus h) mdh
      return $ datumFromPlutus d

  utxosAtAddress addr mAssetClass = do
    gyLogDebug' "" $ "utxosAtAddress, addr: " <> show addr
    refs <- liftClb $ txOutRefAt $ addressToApi' addr
    gyLogDebug' "" $ "utxosAtAddress, refs: " <> show refs
    utxos <- wither f refs
    let utxos' =
          case mAssetClass of
            Nothing -> utxos
            Just ac -> filter (\GYUTxO {..} -> valueAssetClass utxoValue ac > 0) utxos
    return $ utxosFromList utxos'
   where
    f :: Plutus.TxOutRef -> GYTxMonadClbT m (Maybe GYUTxO)
    f ref = do
      case txOutRefFromPlutus ref of
        Left _ -> return Nothing -- TODO: should it error?
        Right ref' -> utxoAtTxOutRef ref'
  utxosWithAsset ac = do
    filterUTxOs (\GYUTxO {..} -> valueAssetClass utxoValue (nonAdaTokenToAssetClass ac) > 0) <$> allUTxOs

  utxosAtPaymentCredential :: GYPaymentCredential -> Maybe GYAssetClass -> GYTxMonadClbT m GYUTxOs
  utxosAtPaymentCredential cred mAssetClass = do
    refs <- liftClb $ txOutRefAtPaymentCred $ paymentCredentialToPlutus cred
    utxos <- wither f refs
    pure
      . utxosFromList
      $ filter
        (\GYUTxO {utxoValue} -> maybe True ((> 0) . valueAssetClass utxoValue) mAssetClass)
        utxos
   where
    f :: Plutus.TxOutRef -> GYTxMonadClbT m (Maybe GYUTxO)
    f ref = case txOutRefFromPlutus ref of
      Left _ -> return Nothing
      Right ref' -> utxoAtTxOutRef ref'

  utxoAtTxOutRef ref = do
    utxosLookup ref <$> allUTxOs

  stakeAddressInfo = const $ pure Nothing

  drepState = const $ pure Nothing
  constitution = error "CLB does not support fetching of constitution"

  mempoolTxs = error "CLB does not support fetching of mempool transactions"

  proposals _actionIds = error "CLB does not support fetching of proposals"

  -- Note, we need to define only one of drepState or drepsState unless required for performace reasons as they have default definition in terms of each other.

  slotConfig = do
    (zero, len) <- slotConfig'
    return $ simpleSlotConfig zero len

  slotOfCurrentBlock = liftClb $ slotFromApi <$> Clb.getCurrentSlot

  logMsg _ns s msg = do
    -- let doc = lines msg
    let doc = msg
    liftClb $ logInfo $ case s of
      GYDebug -> LogEntry Debug doc
      GYInfo -> LogEntry Info doc
      GYWarning -> LogEntry Warning doc
      GYError -> LogEntry Error doc

  waitUntilSlot slot = do
    -- Silently returns if the given slot is greater than the current slot.
    liftClb . Clb.waitSlot $ slotToApi slot
    pure slot
  waitForNextBlock = slotOfCurrentBlock

instance Monad m => GYTxUserQueryMonad (GYTxMonadClbT m) where
  ownAddresses = asks $ userAddresses' . clbEnvWallet

  ownChangeAddress = asks $ userChangeAddress . clbEnvWallet

  ownCollateral = runMaybeT $ do
    UserCollateral {userCollateralRef, userCollateralCheck} <- asks (userCollateral . clbEnvWallet) >>= hoistMaybe
    collateralUtxo <-
      lift $
        utxoAtTxOutRef userCollateralRef
          >>= maybe (throwError . GYQueryUTxOException $ GYNoUtxoAtRef userCollateralRef) pure
    if not userCollateralCheck || (utxoValue collateralUtxo == collateralValue)
      then pure collateralUtxo
      else hoistMaybe Nothing

  availableUTxOs = do
    addrs <- ownAddresses
    utxosAtAddresses addrs

  someUTxO lang = do
    addrs <- ownAddresses
    utxos <- availableUTxOs
    case lang of
      PlutusV3 -> ifNotV1 utxos addrs
      PlutusV2 -> ifNotV1 utxos addrs
      PlutusV1 ->
        case find utxoTranslatableToV1 $ utxosToList utxos of
          Just u -> return $ utxoRef u
          Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs -- TODO: Better error message here?
   where
    ifNotV1 utxos addrs =
      case someTxOutRef utxos of
        Nothing -> throwError $ GYQueryUTxOException $ GYNoUtxosAtAddress addrs
        Just (ref, _) -> return ref

instance Monad m => GYTxMonad (GYTxMonadClbT m) where
  signTxBody = signTxBodyImpl . asks $ AGYPaymentSigningKey . userPaymentSKey . clbEnvWallet
  signTxBodyWithStake = signTxBodyWithStakeImpl $ asks ((,) . AGYPaymentSigningKey . userPaymentSKey . clbEnvWallet) <*> asks (fmap AGYStakeSigningKey . userStakeSKey . clbEnvWallet)
  submitTx tx = do
    let txBody = getTxBody tx
    dumpBody txBody
    gyLogDebug' "" $ "encoded tx: " <> txToHex tx
    vRes <- liftClb . Clb.submitTx $ txToApi tx
    case vRes of
      Success _state _onChainTx -> pure $ txBodyTxId txBody
      Fail _ err -> throwAppError . someBackendError . T.pack $ show err
   where
    -- TODO: use Prettyprinter
    dumpBody :: GYTxBody -> GYTxMonadClbT m ()
    dumpBody body = do
      ins <- mapM utxoAtTxOutRef' $ txBodyTxIns body
      refIns <- mapM utxoAtTxOutRef' $ txBodyTxInsReference body
      gyLogDebug' "" $
        printf
          "fee: %d lovelace\nmint value: %s\nvalidity range: %s\ncollateral: %s\ntotal collateral: %d\ninputs:\n\n%sreference inputs:\n\n%soutputs:\n\n%s"
          (txBodyFee body)
          (txBodyMintValue body)
          (show $ txBodyValidityRange body)
          (show $ txBodyCollateral body)
          (txBodyTotalCollateralLovelace body)
          (concatMap dumpInUTxO ins)
          (concatMap dumpInUTxO refIns)
          (concatMap dumpOutUTxO $ utxosToList $ txBodyUTxOs body)

    dumpInUTxO :: GYUTxO -> String
    dumpInUTxO GYUTxO {..} =
      printf " - ref:        %s\n" utxoRef
        <> printf "   addr:       %s\n" utxoAddress
        <> printf "   value:      %s\n" utxoValue
        <> printf "   datum:      %s\n" (show utxoOutDatum)
        <> printf "   ref script: %s\n\n" (show utxoRefScript)

    dumpOutUTxO :: GYUTxO -> String
    dumpOutUTxO GYUTxO {..} =
      printf " - addr:       %s\n" utxoAddress
        <> printf "   value:      %s\n" utxoValue
        <> printf "   datum:      %s\n" (show utxoOutDatum)
        <> printf "   ref script: %s\n\n" (show utxoRefScript)

  -- Transaction submission and confirmation is immediate in CLB.
  awaitTxConfirmed' _ _ = pure ()

instance Monad m => GYTxGameMonad (GYTxMonadClbT m) where
  type TxMonadOf (GYTxMonadClbT m) = GYTxMonadClbT m
  createUser = do
    st <- get
    let i = clbNextWalletInt st
        user = mkSimpleWallet $ Clb.intToKeyPair i
    gyLogDebug' "createUser" . T.unpack $ "Created simple user with address: " <> addressToText (userAddr user)
    put st {clbNextWalletInt = i + 1}
    pure user
  asUser u act = do
    -- Overwrite the own user and perform the action.
    local
      (\x -> x {clbEnvWallet = u})
      act

slotConfig' :: Monad m => GYTxMonadClbT m (UTCTime, NominalDiffTime)
slotConfig' = liftClb $ do
  sc <- gets $ clbConfigSlotConfig . _clbConfig
  let len = fromInteger (scSlotLength sc) / 1000
      zero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $ scSlotZeroTime sc
  return (zero, len)

protocolParameters :: Monad m => GYTxMonadClbT m (ConwayCore.PParams (Api.S.ShelleyLedgerEra ApiEra))
protocolParameters = do
  pparams <- liftClb $ gets $ clbConfigProtocol . _clbConfig
  pure $ coerce pparams

instance Monad m => GYTxSpecialQueryMonad (GYTxMonadClbT m) where
  systemStart = gyscSystemStart <$> slotConfig

  protocolParams = protocolParameters

  stakePools = pure Set.empty

  -- stakePools = do
  --     pids <- liftClb $ gets $ Map.keys . stake'pools . mockStake
  --     foldM f Set.empty pids
  --   where
  --     f :: Set Api.S.PoolId -> Api.S.PoolId -> GYTxMonadClbT m (Set Api.S.PoolId)
  --     f s pid = either
  --         (\e -> throwError $ GYConversionException $ GYLedgerToCardanoError $ DeserialiseRawBytesError ("stakePools, error: " <> fromString (show e)))
  --         (\pid' -> return $ Set.insert pid' s)
  --         $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsStakePoolKey) bs
  --       where
  --         Plutus.BuiltinByteString bs = Plutus.getPubKeyHash $ unPoolId pid

  eraHistory = do
    (_, len) <- slotConfig'
    return $ Api.EraHistory $ eh len
   where
    eh :: NominalDiffTime -> Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
    eh =
      Ouroboros.mkInterpreter
        . Ouroboros.Summary
        . NonEmptyCons byronEra
        . NonEmptyCons shelleyEra
        . NonEmptyCons allegraEra
        . NonEmptyCons maryEra
        . NonEmptyCons alonzoEra
        . NonEmptyCons babbageEra
        . NonEmptyOne
        . conwayEra

    byronEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864, eraGenesisWin = 0}
        }
    shelleyEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }
    allegraEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }
    maryEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }
    alonzoEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }
    babbageEra =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }
    conwayEra len =
      Ouroboros.EraSummary
        { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
        , eraEnd = Ouroboros.EraUnbounded
        , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength len, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
        }

dumpUtxoState :: Monad m => GYTxMonadClbT m ()
dumpUtxoState = liftClb Clb.dumpUtxoState

-------------------------------------------------------------------------------
-- Preset StdGen
-------------------------------------------------------------------------------

pureGen :: StdGen
pureGen = mkStdGen 42

-- | This is simply defined as @buildTxBody skeleton >>= signAndSubmitConfirmed@.
sendSkeleton :: GYTxMonad m => GYTxSkeleton v -> m GYTxId
sendSkeleton skeleton = snd <$> sendSkeleton' skeleton

sendSkeleton' :: GYTxMonad m => GYTxSkeleton v -> m (GYTxBody, GYTxId)
sendSkeleton' skeleton = buildTxBody skeleton >>= \tx -> signAndSubmitConfirmed tx >>= \txId -> pure (tx, txId)

-- | Variant of `logInfo` from @Clb@ that logs a string with @Info@ severity.
logInfoS :: Monad m => String -> ClbT ApiEra m ()
logInfoS s = Clb.logInfo $ Clb.LogEntry Clb.Info s
