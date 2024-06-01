{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : GeniusYield.TxBuilder.Run
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Clb
    ( Wallet (..)
    , WalletName
    , walletAddress
    , GYTxRunState (..)
    , GYTxMonadClb
    , asClb
    , asRandClb
    , liftClb
    , ownAddress
    , sendSkeleton
    , sendSkeleton'
    , dumpUtxoState
    , mustFail
    ) where

import qualified Cardano.Api                               as Api
import qualified Cardano.Api.Shelley                       as Api.S
import qualified Cardano.Ledger.Alonzo.Language            as Ledger
import qualified Cardano.Ledger.BaseTypes                  as Ledger
import           Cardano.Slotting.Time                     (RelativeTime (RelativeTime),
                                                            mkSlotLength)
import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable                             (foldMap')
import           Data.List                                 (singleton, (\\))
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.Map.Strict                           as Map
import           Data.Semigroup                            (Sum (..))
import qualified Data.Set                                  as Set
import           Data.SOP.NonEmpty                         (NonEmpty (NonEmptyCons, NonEmptyOne))
import           Data.Time.Clock                           (NominalDiffTime,
                                                            UTCTime)
import           Data.Time.Clock.POSIX                     (posixSecondsToUTCTime)
import qualified Ouroboros.Consensus.Cardano.Block         as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History      as Ouroboros
import qualified PlutusLedgerApi.V1.Interval               as Plutus
import qualified PlutusLedgerApi.V2                        as Plutus
import qualified PlutusTx.Builtins.Internal                as Plutus
import           Data.Sequence                             (ViewR (..), viewr)
import           GeniusYield.Imports
import           GeniusYield.Transaction                   (
    GYCoinSelectionStrategy (GYRandomImproveMultiAsset),
    BuildTxException (BuildTxBalancingError),
    BalancingError(BalancingErrorInsufficientFunds)
    )
import           GeniusYield.Transaction.Common            (adjustTxOut,
                                                            minimumUTxO)
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

import Cardano.Api (ShelleyBasedEra(ShelleyBasedEraBabbage))
import Cardano.Api.Script (fromShelleyBasedScript, fromShelleyScriptToReferenceScript)
import Cardano.Api.Shelley qualified as ApiS
import Cardano.Ledger.Address (unCompactAddr, decompactAddr)
import Cardano.Ledger.Api (binaryDataToData, getPlutusData)
import Cardano.Ledger.Plutus.TxInfo qualified as L
import Cardano.Ledger.Babbage.TxOut qualified as L
import Cardano.Ledger.Plutus.Data qualified as L
import qualified Cardano.Ledger.Alonzo.Core           as AlonzoCore
import Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Shelley.API (LedgerState(..), UTxOState (utxosUtxo), StakeReference (..))
import Cardano.Ledger.UTxO (UTxO(..))
import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Clb (
  PParams(..),
  SlotConfig(..),
  MockConfig(..),
  EmulatedLedgerState (..),

  ClbT,
  ClbState (..),

  getCurrentSlot,
  txOutRefAt,
  txOutRefAtPaymentCred,

  sendTx,
  ValidationResult (..),
  OnChainTx,

  LogEntry (LogEntry),
  LogLevel (..),
  Log (Log),
  fromLog,
  unLog,
  getFails,
  logInfo,
  logError,
 )
import Clb qualified (dumpUtxoState)
import PlutusLedgerApi.V2 qualified as PV2
import GeniusYield.Test.Address

type Clb = ClbT Identity

type WalletName = String

-- | Testing Wallet representation.
data Wallet = Wallet
    { walletPaymentSigningKey :: !GYPaymentSigningKey
    , walletNetworkId         :: !GYNetworkId
    , walletName              :: !WalletName
    }
    deriving (Show, Eq, Ord)

instance HasAddress Wallet where
    toAddress = addressToPlutus . walletAddress

-- | Gets a GYAddress of a testing wallet.
walletAddress :: Wallet -> GYAddress
walletAddress Wallet{..} = addressFromPubKeyHash walletNetworkId $ pubKeyHash $
                           paymentVerificationKey walletPaymentSigningKey

-- instance HasAddress Wallet where
--     toAddress = addressToPlutus . walletAddress

newtype GYTxRunEnv = GYTxRunEnv { runEnvWallet :: Wallet }

type FeesLovelace = Sum Integer
type MinAdaLovelace = Sum Integer

-- Used by 'withWalletBalancesCheckSimple' (not yet)
newtype GYTxRunState = GYTxRunState { walletExtraLovelace :: Map WalletName (FeesLovelace, MinAdaLovelace) }

newtype GYTxMonadClb a = GYTxMonadClb
    { unGYTxMonadClb :: ExceptT (Either String GYTxMonadException) (StateT GYTxRunState (ReaderT GYTxRunEnv (RandT StdGen Clb))) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader GYTxRunEnv, MonadState GYTxRunState)

instance MonadRandom GYTxMonadClb where
    getRandomR  = GYTxMonadClb . getRandomR
    getRandom   = GYTxMonadClb getRandom
    getRandomRs = GYTxMonadClb . getRandomRs
    getRandoms  = GYTxMonadClb getRandoms

asRandClb :: Wallet
          -> GYTxMonadClb a
          -> RandT StdGen Clb (Maybe a)
asRandClb w m = do
    e <- runReaderT (evalStateT (runExceptT $ unGYTxMonadClb m) $ GYTxRunState Map.empty) $ GYTxRunEnv w
    case e of
        Left (Left err)  -> lift (logError err) >> return Nothing
        Left (Right err) -> lift (logError (show err)) >> return Nothing
        Right a          -> return $ Just a

asClb :: StdGen
      -> Wallet
      -> GYTxMonadClb a
      -> Clb (Maybe a)
asClb g w m = evalRandT (asRandClb w m) g

ownAddress :: GYTxMonadClb GYAddress
ownAddress = do
    nid <- networkId
    asks $ addressFromPubKeyHash nid . pubKeyHash . paymentVerificationKey . walletPaymentSigningKey . runEnvWallet

liftClb :: Clb a -> GYTxMonadClb a
liftClb = GYTxMonadClb . lift . lift . lift . lift

{- | Try to execute an action, and if it fails, restore to the current state
 while preserving logs. If the action succeeds, logs an error as we expect
 it to fail. Use 'mustFailWith' and 'mustFailWithBlock' to provide custom
 error message or/and failure action name.
 FIXME: should we move it to CLB?
-}
mustFail :: GYTxMonadClb a -> GYTxMonadClb ()
mustFail act = do
    (st, preFails) <- liftClb $ do
        st <- get
        preFails <- getFails
        pure (st, preFails)
    void $ act
    postFails <- liftClb $ getFails
    if noNewErrors preFails postFails
        then liftClb $ logError "Expected action to fail but it succeeds"
    else do
        infoLog <- liftClb $ gets mockInfo
        liftClb $ put
            st
                { mockInfo = infoLog <> mkMustFailLog preFails postFails
                -- , mustFailLog = mkMustFailLog preFails postFails
                }
    where
      noNewErrors (fromLog -> a) (fromLog -> b) = length a == length b
      mkMustFailLog (unLog -> pre) (unLog -> post) =
        Log $ second (LogEntry Error . ((msg  <> ":") <> ). show) <$> Seq.drop (Seq.length pre) post
      msg = "Unnamed failure action"

getNetworkId :: GYTxMonadClb GYNetworkId
getNetworkId = do
    res <- liftClb $ gets (networkIdFromApi . mockConfigNetworkId . mockConfig)
    maybe (fail "Unsupported network") pure res

instance MonadFail GYTxMonadClb where
    fail = GYTxMonadClb . throwError . Left

instance MonadError GYTxMonadException GYTxMonadClb where

    throwError = GYTxMonadClb . throwError . Right

    catchError m handler = GYTxMonadClb $ catchError (unGYTxMonadClb m) $ \case
        Left  err -> throwError $ Left err
        Right err -> unGYTxMonadClb $ handler err

instance GYTxQueryMonad GYTxMonadClb where

    networkId = getNetworkId

    lookupDatum :: GYDatumHash -> GYTxMonadClb (Maybe GYDatum)
    lookupDatum h = liftClb $ do
        mdh <- gets mockDatums
        return $ do
            d <- Map.lookup (datumHashToPlutus h) mdh
            return $ datumFromPlutus d

    utxosAtAddress addr mAssetClass = do
        gyLogDebug' "" $ "utxosAtAddress, addr: " <> show addr
        refs  <- liftClb $ txOutRefAt $ addressToApi' addr
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
                Left _     -> return Nothing -- TODO: should it error?
                Right ref' -> utxoAtTxOutRef ref'

    utxosAtPaymentCredential :: GYPaymentCredential -> Maybe GYAssetClass -> GYTxMonadClb GYUTxOs
    utxosAtPaymentCredential cred mAssetClass = do
        refs  <- liftClb $ txOutRefAtPaymentCred $ paymentCredentialToPlutus cred
        utxos <- wither f refs
        pure
            . utxosFromList
            $ filter (\GYUTxO{utxoValue} -> maybe True (>0) $ valueAssetClass utxoValue <$> mAssetClass)
            utxos
      where
        f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
        f ref = case txOutRefFromPlutus ref of
            Left _     -> return Nothing
            Right ref' -> utxoAtTxOutRef ref'

    utxoAtTxOutRef ref = do
        -- All UTxOs map
        utxos <- liftClb $ gets (unUTxO . utxosUtxo . lsUTxOState . _memPoolState . emulatedLedgerState)
        -- Maps keys to Plutus TxOutRef
        let m = Map.mapKeys (txOutRefToPlutus . txOutRefFromApi . Api.S.fromShelleyTxIn) utxos

        return $ do
            o <- Map.lookup (txOutRefToPlutus ref) m

            let a = addressFromApi . Api.S.fromShelleyAddrToAny . either id decompactAddr $ o ^. L.addrEitherBabbageTxOutL
                v = valueFromApi . ApiS.fromMaryValue . either id L.fromCompact $ o ^. L.valueEitherBabbageTxOutL

            d <- case o ^. L.datumBabbageTxOutL of
                L.NoDatum -> pure GYOutDatumNone
                L.DatumHash dh -> GYOutDatumHash <$> rightToMaybe (datumHashFromPlutus $ L.transDataHash dh)
                L.Datum binaryData -> pure $
                    GYOutDatumInline
                    . datumFromPlutus
                    . PV2.Datum
                    . PV2.dataToBuiltinData
                    . getPlutusData
                    . binaryDataToData
                    $ binaryData

            let s = case o ^. L.referenceScriptBabbageTxOutL of
                        SJust x  -> someScriptFromReferenceApi $ fromShelleyScriptToReferenceScript ShelleyBasedEraBabbage x
                        SNothing -> Nothing

            return GYUTxO
                { utxoRef       = ref
                , utxoAddress   = a
                , utxoValue     = v
                , utxoOutDatum  = d
                , utxoRefScript = s
                }

    slotConfig = do
        (zero, len) <- slotConfig'
        return $ simpleSlotConfig zero len

    slotOfCurrentBlock = liftClb $ slotFromApi <$> Clb.getCurrentSlot

    logMsg _ns s msg = do
        -- let doc = lines msg
        let doc = msg
        liftClb $ logInfo $ case s of
            GYDebug   -> LogEntry Debug doc
            GYInfo    -> LogEntry Info doc
            GYWarning -> LogEntry Warning doc
            GYError   -> LogEntry Error doc

instance GYTxMonad GYTxMonadClb where

    ownAddresses = singleton <$> do
        nid <- networkId
        asks $ addressFromPaymentKeyHash nid . paymentKeyHash . paymentVerificationKey . walletPaymentSigningKey . runEnvWallet

    availableUTxOs = do
        addrs <- ownAddresses
        utxosAtAddresses addrs

    someUTxO lang = do
        addrs <- ownAddresses
        utxos <- availableUTxOs
        case lang of
          PlutusV2 ->
            case someTxOutRef utxos of
                Nothing       -> throwError $ GYQueryUTxOException $ GYNoUtxosAtAddress addrs
                Just (ref, _) -> return ref
          PlutusV1 ->
            case find utxoTranslatableToV1 $ utxosToList utxos of
              Just u  -> return $ utxoRef u
              Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Better error message here?

    randSeed = return 42

sendSkeleton :: GYTxSkeleton v -> GYTxMonadClb GYTxId
sendSkeleton skeleton = snd <$> sendSkeleton' skeleton []

sendSkeleton' :: GYTxSkeleton v -> [Wallet] -> GYTxMonadClb (OnChainTx, GYTxId)
sendSkeleton' skeleton ws = do
    w <- asks runEnvWallet
    let sigs = map walletPaymentSigningKey $ w : ws
    body <- skeletonToTxBody skeleton
    pp <- protocolParameters
    modify (updateWalletState w pp body)
    dumpBody body

    let tx = signGYTxBody body sigs
    gyLogDebug' "" $ "encoded tx: " <> txToHex tx

    -- Submit
    vRes <- liftClb $ sendTx $ txToApi tx
    case vRes of
        Success _state onChainTx -> pure (onChainTx, txBodyTxId body)
        Fail _ err -> fail $ show err

  where
    -- Updates the wallet state.
    -- Updates extra lovelace required for fees & minimum ada requirements against the wallet sending this transaction.
    updateWalletState :: Wallet -> AlonzoCore.PParams (Api.S.ShelleyLedgerEra Api.S.BabbageEra) -> GYTxBody -> GYTxRunState -> GYTxRunState
    updateWalletState w@Wallet {..} pp body GYTxRunState {..} = GYTxRunState $ Map.insertWith mappend walletName v walletExtraLovelace
      where
        v = ( coerce $ txBodyFee body
            , coerce $ flip valueAssetClass GYLovelace $
                foldMap'
                  (\o ->
                    -- If this additional ada is coming back to one's own self, we need not account for it.
                    if gyTxOutAddress o == walletAddress w then
                      mempty
                    else gyTxOutValue (adjustTxOut (minimumUTxO pp) o) `valueMinus` gyTxOutValue o
                  ) $ gytxOuts skeleton
            )

    -- TODO: use Prettyprinter
    dumpBody :: GYTxBody -> GYTxMonadClb ()
    dumpBody body = do
        ins <- mapM utxoAtTxOutRef' $ txBodyTxIns body
        refIns <- mapM utxoAtTxOutRef' $ txBodyTxInsReference body
        gyLogDebug' "" $
            printf "fee: %d lovelace\nmint value: %s\nvalidity range: %s\ncollateral: %s\ntotal collateral: %d\ninputs:\n\n%sreference inputs:\n\n%soutputs:\n\n%s"
                (txBodyFee body)
                (txBodyMintValue body)
                (show $ txBodyValidityRange body)
                (show $ txBodyCollateral body)
                (txBodyTotalCollateralLovelace body)
                (concatMap dumpInUTxO ins)
                (concatMap dumpInUTxO refIns)
                (concatMap dumpOutUTxO $ utxosToList $ txBodyUTxOs body)

    dumpInUTxO :: GYUTxO -> String
    dumpInUTxO GYUTxO{..} = printf " - ref:        %s\n"   utxoRef             <>
                            printf "   addr:       %s\n"   utxoAddress         <>
                            printf "   value:      %s\n"   utxoValue           <>
                            printf "   datum:      %s\n"   (show utxoOutDatum) <>
                            printf "   ref script: %s\n\n" (show utxoRefScript)

    dumpOutUTxO :: GYUTxO -> String
    dumpOutUTxO GYUTxO{..} = printf " - addr:       %s\n"   utxoAddress         <>
                             printf "   value:      %s\n"   utxoValue           <>
                             printf "   datum:      %s\n"   (show utxoOutDatum) <>
                             printf "   ref script: %s\n\n" (show utxoRefScript)

skeletonToTxBody :: GYTxSkeleton v -> GYTxMonadClb GYTxBody
skeletonToTxBody skeleton = do
    ss <- systemStart
    eh <- eraHistory
    pp <- protocolParameters
    ps <- stakePools

    addr        <- ownAddress
    e <- buildTxCore ss eh pp ps GYRandomImproveMultiAsset (const id) [addr] addr Nothing (return [Identity skeleton])
    case e of
        Left err  -> throwAppError err
        Right res -> case res of
            GYTxBuildSuccess (Identity body :| _) -> return body
            GYTxBuildFailure (BalancingErrorInsufficientFunds v) -> throwAppError . BuildTxBalancingError $ BalancingErrorInsufficientFunds v
            GYTxBuildFailure _                    -> error "impossible case"
            GYTxBuildPartialSuccess _ _           -> error "impossible case"
            GYTxBuildNoInputs                     -> error "impossible case"

slotConfig' :: GYTxMonadClb (UTCTime, NominalDiffTime)
slotConfig' = liftClb $ do
    sc <- gets $ mockConfigSlotConfig . mockConfig
    let len  = fromInteger (scSlotLength sc) / 1000
        zero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $ scSlotZeroTime sc
    return (zero, len)

systemStart :: GYTxMonadClb Api.SystemStart
systemStart = gyscSystemStart <$> slotConfig

protocolParameters :: GYTxMonadClb (AlonzoCore.PParams (Api.S.ShelleyLedgerEra Api.S.BabbageEra))
protocolParameters = do
    pparams <- liftClb $ gets $ mockConfigProtocol . mockConfig
    pure $ coerce pparams


stakePools :: GYTxMonadClb (Set Api.S.PoolId)
stakePools = pure Set.empty
-- stakePools = do
--     pids <- liftClb $ gets $ Map.keys . stake'pools . mockStake
--     foldM f Set.empty pids
--   where
--     f :: Set Api.S.PoolId -> PoolId -> GYTxMonadClb (Set Api.S.PoolId)
--     f s pid = either
--         (\e -> throwError $ GYConversionException $ GYLedgerToCardanoError $ DeserialiseRawBytesError ("stakePools, error: " <> fromString (show e)))
--         (\pid' -> return $ Set.insert pid' s)
--         $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsStakePoolKey) bs
--       where
--         Plutus.BuiltinByteString bs = Plutus.getPubKeyHash $ unPoolId pid

eraHistory :: GYTxMonadClb Api.EraHistory
eraHistory = do
    (_, len) <- slotConfig'
    return $ Api.EraHistory $ eh len
  where
    eh :: NominalDiffTime -> Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
    eh = Ouroboros.mkInterpreter . Ouroboros.Summary
                . NonEmptyCons byronEra
                . NonEmptyCons shelleyEra
                . NonEmptyCons allegraEra
                . NonEmptyCons maryEra
                . NonEmptyCons alonzoEra
                . NonEmptyOne . babbageEra

    byronEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    babbageEra len =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength len, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }

dumpUtxoState :: GYTxMonadClb ()
dumpUtxoState = liftClb Clb.dumpUtxoState

