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
  GYTxMonadClb,
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

import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
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
import Cardano.Api.Script qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Address qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Conway.Core qualified as ConwayCore
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.TxInfo qualified as L
import Cardano.Ledger.Shelley.API qualified as L.S
import Cardano.Ledger.UTxO qualified as L
import Cardano.Slotting.Slot (
  EpochNo (..),
  EpochSize (..),
 )
import Cardano.Slotting.Time (
  RelativeTime (RelativeTime),
  mkSlotLength,
 )
import Clb (
  Clb,
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

type AtlasClb = Clb ApiEra

newtype GYTxClbEnv = GYTxClbEnv
  { clbEnvWallet :: User
  -- ^ The actor for a GYTxMonadClb action.
  }

newtype GYTxClbState = GYTxClbState
  { clbNextWalletInt :: Integer
  -- ^ Next integer to use with 'Clb.intToKeyPair' call in order to generate a new user.
  }

newtype GYTxMonadClb a = GYTxMonadClb
  { unGYTxMonadClb :: ReaderT GYTxClbEnv (StateT GYTxClbState (ExceptT GYTxMonadException (RandT StdGen AtlasClb))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader GYTxClbEnv, MonadState GYTxClbState)
  deriving anyclass GYTxBuilderMonad

instance MonadRandom GYTxMonadClb where
  getRandomR = GYTxMonadClb . getRandomR
  getRandom = GYTxMonadClb getRandom
  getRandomRs = GYTxMonadClb . getRandomRs
  getRandoms = GYTxMonadClb getRandoms

asRandClb ::
  User ->
  Integer ->
  GYTxMonadClb a ->
  RandT StdGen AtlasClb (Maybe a)
asRandClb w i m = do
  e <- runExceptT $ (unGYTxMonadClb m `runReaderT` GYTxClbEnv w) `runStateT` GYTxClbState {clbNextWalletInt = i}
  case e of
    Left (GYApplicationException (toApiError -> GYApiError {gaeMsg})) -> lift (logError $ T.unpack gaeMsg) >> return Nothing
    Left err -> lift (logError $ show err) >> return Nothing
    Right (a, _) -> return $ Just a

asClb ::
  StdGen ->
  User ->
  Integer ->
  GYTxMonadClb a ->
  AtlasClb (Maybe a)
asClb g w i m = evalRandT (asRandClb w i m) g

liftClb :: AtlasClb a -> GYTxMonadClb a
liftClb = GYTxMonadClb . lift . lift . lift . lift

{- | Given a test name, runs the trace for every wallet, checking there weren't
     errors.
-}
mkTestFor :: String -> (TestInfo -> GYTxMonadClb a) -> Tasty.TestTree
mkTestFor name action =
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
  testNoErrorsTraceClb :: GYValue -> GYValue -> Clb.ClbConfig ApiEra -> String -> AtlasClb a -> Tasty.TestTree
  testNoErrorsTraceClb funds walletFunds cfg msg act =
    testCaseInfo msg $
      maybe (pure mockLog) assertFailure $
        mbErrors >>= \errors -> pure (mockLog <> "\n\nError :\n-------\n" <> errors)
   where
    -- _errors since we decided to store errors in the log as well.
    (mbErrors, mock) = Clb.runClb (act >> Clb.checkErrors) $ Clb.initClb cfg (valueToApi funds) (valueToApi walletFunds) Nothing
    mockLog = "\nEmulator log :\n--------------\n" <> logString
    options = defaultLayoutOptions {layoutPageWidth = AvailablePerLine 150 1.0}
    logDoc = Clb.ppLog $ Clb._clbLog mock
    logString = renderString $ layoutPretty options logDoc

mkSimpleWallet :: TL.KeyPair L.S.Payment L.StandardCrypto -> User
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
mustFail :: GYTxMonadClb a -> GYTxMonadClb ()
mustFail = mustFailWith (const True)

mustFailWith :: (GYTxMonadException -> Bool) -> GYTxMonadClb a -> GYTxMonadClb ()
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

instance MonadError GYTxMonadException GYTxMonadClb where
  throwError = GYTxMonadClb . throwError

  catchError m handler = GYTxMonadClb . catchError (unGYTxMonadClb m) $ unGYTxMonadClb . handler

instance GYTxQueryMonad GYTxMonadClb where
  networkId = do
    magic <- liftClb $ gets (clbConfigNetworkId . _clbConfig)
    -- TODO: Add epoch slots and network era to clb and retrieve from there.
    pure . GYPrivnet $
      GYNetworkInfo
        { gyNetworkMagic = Api.S.unNetworkMagic $ Api.S.toNetworkMagic magic
        , gyNetworkEpochSlots = 500
        }

  lookupDatum :: GYDatumHash -> GYTxMonadClb (Maybe GYDatum)
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
    f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
    f ref = do
      case txOutRefFromPlutus ref of
        Left _ -> return Nothing -- TODO: should it error?
        Right ref' -> utxoAtTxOutRef ref'

  utxosAtPaymentCredential :: GYPaymentCredential -> Maybe GYAssetClass -> GYTxMonadClb GYUTxOs
  utxosAtPaymentCredential cred mAssetClass = do
    refs <- liftClb $ txOutRefAtPaymentCred $ paymentCredentialToPlutus cred
    utxos <- wither f refs
    pure
      . utxosFromList
      $ filter
        (\GYUTxO {utxoValue} -> maybe True ((> 0) . valueAssetClass utxoValue) mAssetClass)
        utxos
   where
    f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
    f ref = case txOutRefFromPlutus ref of
      Left _ -> return Nothing
      Right ref' -> utxoAtTxOutRef ref'

  utxoAtTxOutRef ref = do
    -- All UTxOs map
    utxos <- liftClb $ gets (L.unUTxO . L.S.utxosUtxo . L.S.lsUTxOState . _ledgerState . _chainState)
    -- Maps keys to Plutus TxOutRef
    let m = Map.mapKeys (txOutRefToPlutus . txOutRefFromApi . Api.S.fromShelleyTxIn) utxos

    return $ do
      o <- Map.lookup (txOutRefToPlutus ref) m

      let a = addressFromApi . Api.S.fromShelleyAddrToAny . either id L.decompactAddr $ o ^. L.addrEitherTxOutL
          v = valueFromApi . Api.S.fromMaryValue . either id L.fromCompact $ o ^. L.valueEitherTxOutL

      d <- case o ^. L.datumTxOutL of
        L.NoDatum -> pure GYOutDatumNone
        L.DatumHash dh -> GYOutDatumHash <$> rightToMaybe (datumHashFromPlutus $ L.transDataHash dh)
        L.Datum binaryData ->
          pure
            $ GYOutDatumInline
              . datumFromPlutus
              . Plutus.Datum
              . Plutus.dataToBuiltinData
              . L.getPlutusData
              . L.binaryDataToData
            $ binaryData

      let s = case o ^. L.referenceScriptTxOutL of
            L.S.SJust x ->
              someScriptFromReferenceApi $
                Api.fromShelleyScriptToReferenceScript Api.ShelleyBasedEraConway x
            L.S.SNothing -> Nothing

      return
        GYUTxO
          { utxoRef = ref
          , utxoAddress = a
          , utxoValue = v
          , utxoOutDatum = d
          , utxoRefScript = s
          }

  stakeAddressInfo = const $ pure Nothing

  drepState = const $ pure Nothing

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

instance GYTxUserQueryMonad GYTxMonadClb where
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

instance GYTxMonad GYTxMonadClb where
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
    dumpBody :: GYTxBody -> GYTxMonadClb ()
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

instance GYTxGameMonad GYTxMonadClb where
  type TxMonadOf GYTxMonadClb = GYTxMonadClb
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

slotConfig' :: GYTxMonadClb (UTCTime, NominalDiffTime)
slotConfig' = liftClb $ do
  sc <- gets $ clbConfigSlotConfig . _clbConfig
  let len = fromInteger (scSlotLength sc) / 1000
      zero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $ scSlotZeroTime sc
  return (zero, len)

protocolParameters :: GYTxMonadClb (ConwayCore.PParams (Api.S.ShelleyLedgerEra ApiEra))
protocolParameters = do
  pparams <- liftClb $ gets $ clbConfigProtocol . _clbConfig
  pure $ coerce pparams

instance GYTxSpecialQueryMonad GYTxMonadClb where
  systemStart = gyscSystemStart <$> slotConfig

  protocolParams = protocolParameters

  stakePools = pure Set.empty

  -- stakePools = do
  --     pids <- liftClb $ gets $ Map.keys . stake'pools . mockStake
  --     foldM f Set.empty pids
  --   where
  --     f :: Set Api.S.PoolId -> Api.S.PoolId -> GYTxMonadClb (Set Api.S.PoolId)
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

dumpUtxoState :: GYTxMonadClb ()
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
