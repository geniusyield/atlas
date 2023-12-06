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
    , walletAddress
    , GYTxRunState (..)
    , GYTxMonadClb
    , asClb
    , asRandClb
    , liftClb
    , ownAddress
    , sendSkeleton
    , sendSkeleton'
    , sendSkeletonWithWallets
    , networkIdRun
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
import           Data.SOP.Counting                         (NonEmpty (NonEmptyCons, NonEmptyOne))
import           Data.Time.Clock                           (NominalDiffTime,
                                                            UTCTime)
import           Data.Time.Clock.POSIX                     (posixSecondsToUTCTime)
import qualified Ouroboros.Consensus.Cardano.Block         as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History      as Ouroboros
import qualified PlutusLedgerApi.V1.Interval               as Plutus
import qualified PlutusLedgerApi.V2                        as Plutus
import qualified PlutusTx.Builtins.Internal                as Plutus

import qualified Cardano.Simple.PlutusLedgerApi.V1.Scripts as Fork
import           Data.Sequence                             (ViewR (..), viewr)
import           GeniusYield.Imports
import           GeniusYield.Transaction                   (GYCoinSelectionStrategy (GYRandomImproveMultiAsset))
import           GeniusYield.Transaction.Common            (adjustTxOut,
                                                            minimumUTxO)
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import           GeniusYield.Clb.Clb (Clb, CardanoTx, logError, ClbState (..), txOutRefAt, txOutRefAtPaymentCred, logInfo, sendTx)
import GeniusYield.Clb.MockConfig (MockConfig(..))
import GeniusYield.TxBuilder.Run (Wallet (..), WalletName)
import GeniusYield.Clb.TimeSlot (SlotConfig(..))
import GeniusYield.Clb.Params (PParams(..))
import Cardano.Ledger.Shelley.API (LedgerState(..), UTxOState (utxosUtxo), StakeReference (..))
import GeniusYield.Clb.ClbLedgerState (EmulatedLedgerState (..), initialState, setUtxo)
import Cardano.Ledger.UTxO (UTxO(..))
import Control.Lens ((^.))
import Cardano.Ledger.Babbage.TxBody (addrEitherBabbageTxOutL, valueEitherBabbageTxOutL)
import Cardano.Ledger.Address (unCompactAddr, decompactAddr)
import qualified Cardano.Ledger.Compactible as L
import qualified Cardano.Api.Shelley as ApiS


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

networkIdRun :: Clb GYNetworkId
networkIdRun = do
    n <- gets $ mockConfigNetworkId . mockConfig
    return $ case n of
        Ledger.Mainnet -> GYMainnet
        Ledger.Testnet -> GYTestnetPreprod

instance MonadFail GYTxMonadClb where
    fail = GYTxMonadClb . throwError . Left

instance MonadError GYTxMonadException GYTxMonadClb where

    throwError = GYTxMonadClb . throwError . Right

    catchError m handler = GYTxMonadClb $ catchError (unGYTxMonadClb m) $ \case
        Left  err -> throwError $ Left err
        Right err -> unGYTxMonadClb $ handler err

instance GYTxQueryMonad GYTxMonadClb where

    networkId = liftClb networkIdRun

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

    utxosAtPaymentCredential :: GYPaymentCredential -> GYTxMonadClb GYUTxOs
    utxosAtPaymentCredential cred = do
        refs  <- liftClb $ txOutRefAtPaymentCred $ paymentCredentialToPlutus cred
        utxos <- wither f refs
        return $ utxosFromList utxos
      where
        f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
        f ref = do
            case txOutRefFromPlutus ref of
                Left _     -> return Nothing
                Right ref' -> utxoAtTxOutRef ref'


    utxoAtTxOutRef ref = do
        utxos <- liftClb $ gets (unUTxO . utxosUtxo . lsUTxOState . _memPoolState . emulatedLedgerState)
        -- mUtxosWithoutRefScripts   <- liftClb $ gets mockUtxos
        -- let m = mUtxosWithoutRefScripts
        let m = Map.mapKeys (txOutRefToPlutus . txOutRefFromApi . Api.S.fromShelleyTxIn) utxos
        -- mScripts <- liftClb $ gets mockScripts
        -- nid <- networkId
        return $ do
            o <- Map.lookup (txOutRefToPlutus ref) m
            -- a <- rightToMaybe $ addressFromPlutus nid $ Plutus.txOutAddress o
            a <- addressFromApi . Api.S.fromShelleyAddrToAny . decompactAddr <$> rightToMaybe (o ^. addrEitherBabbageTxOutL)
            -- v <- rightToMaybe $ valueFromPlutus       $ Plutus.txOutValue   o
            v <- valueFromApi . ApiS.fromMaryValue . L.fromCompact <$> rightToMaybe (o ^. valueEitherBabbageTxOutL)
            -- d <- case Plutus.txOutDatum o of
            --         Plutus.NoOutputDatum      -> return GYOutDatumNone
            --         Plutus.OutputDatumHash h' -> GYOutDatumHash <$> rightToMaybe (datumHashFromPlutus h')
            --         Plutus.OutputDatum d      -> return $ GYOutDatumInline $ datumFromPlutus d
            -- let s = do
            --       sh <- Plutus.txOutReferenceScript o
            --       vs <- Map.lookup sh mScripts
            --       if | isV1 vs   -> Just (Some $ scriptFromSerialisedScript @'PlutusV1 (coerce $ versioned'content vs))
            --          | isV2 vs   -> Just (Some $ scriptFromSerialisedScript @'PlutusV2 (coerce $ versioned'content vs))
            --          | otherwise -> Nothing

            return GYUTxO
                { utxoRef       = ref
                , utxoAddress   = a
                , utxoValue     = v
                , utxoOutDatum  = GYOutDatumNone -- FIXME:
                , utxoRefScript = Nothing
                }

    slotConfig = do
        (zero, len) <- slotConfig'
        return $ simpleSlotConfig zero len

    -- TODO: Make it actually the last seen block's slot.
    slotOfCurrentBlock = do
        s <- undefined -- FIXME: liftClb $ Fork.getSlot <$> Plutus.Model.currentSlot
        case slotFromInteger s of
            Nothing -> throwError $ GYConversionException $ GYInvalidSlot s
            Just s' -> return s'

    logMsg ns s msg = liftClb $ case s of
        GYDebug   -> logInfo  $ printf "%s [DEBUG]: %s" ns msg
        GYInfo    -> logInfo  $ printf "%s [INFO]: %s"  ns msg
        GYWarning -> logInfo  $ printf "%s [WARN]: %s"  ns msg
        -- GYError   -> logError $ printf "%s [ERROR]: %s" ns msg
        GYError   -> logInfo $ printf "%s [ERROR]: %s" ns msg

instance GYTxMonad GYTxMonadClb where

    ownAddresses = singleton <$> ownAddress

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

-- Send skeletons with multiple signatures from wallet
sendSkeletonWithWallets :: GYTxSkeleton v -> [Wallet] -> GYTxMonadClb GYTxId
sendSkeletonWithWallets skeleton ws = do
    --  snd <$> sendSkeleton' skeleton ws
    sendSkeleton' skeleton  []
    pure "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"

sendSkeleton :: GYTxSkeleton v -> GYTxMonadClb GYTxId
sendSkeleton skeleton = do
    -- snd <$> sendSkeleton' skeleton  []
    sendSkeleton' skeleton  []
    pure "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"


sendSkeleton' :: GYTxSkeleton v -> [Wallet]  -> GYTxMonadClb () -- (CardanoTx, GYTxId)
sendSkeleton' skeleton ws = do
    -- body
    body <- skeletonToTxBody skeleton
    dumpBody body
    -- unused now
    -- pp <- protocolParameters
    -- modify (updateWalletState w pp body)
    -- signature
    -- let sigs = walletSignatures (w:ws)
    Wallet{walletPaymentSigningKey} <- asks runEnvWallet
    -- The whole tx
    let tx = signGYTxBody body [walletPaymentSigningKey]
    gyLogDebug' "" $ "Tx encoded: " <> txToHex tx

    e <- liftClb $ do
        sendTx $ txToApi tx

    pure ()
    -- case e of
    --     Left fr -> fail $ show fr
    --     Right _ -> do
    --       mtxs <- liftClb $ gets mockTxs
    --       let tid = case viewr (unLog mtxs) of EmptyR -> error "Absurd (sendSkeleton'): Sequence can't be empty"; (_ :> a) -> txStatId $ snd a in
    --         case txIdFromPlutus tid of
    --           Left e'   -> fail $ printf "invalid tid %s, error: %s" (show tid) (show e')
    --           Right tid' -> return (tx5, tid')

  where


    dumpBody :: GYTxBody -> GYTxMonadClb ()
    dumpBody body = do
        ins <- mapM utxoAtTxOutRef' $ txBodyTxIns body
        refIns <- mapM utxoAtTxOutRef' $ txBodyTxInsReference body
        liftClb $ logInfo $ printf "fee: %d lovelace\nmint value: %s\nvalidity range: %s\ncollateral: %s\ntotal collateral: %d\ninputs:\n\n%sreference inputs:\n\n%soutputs:\n\n%s"
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
            GYTxBuildFailure v                    -> throwAppError $ InsufficientFundsErr v
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

protocolParameters :: GYTxMonadClb (Api.S.BundledProtocolParameters Api.S.BabbageEra)
protocolParameters = do
    pparams <- liftClb $ gets $ mockConfigProtocol . mockConfig
    return $ case pparams of
        AlonzoParams  _ -> error "Run.hs/protocolParameters: Only support babbage era parameters"
        BabbageParams p -> Api.BundleAsShelleyBasedProtocolParameters Api.ShelleyBasedEraBabbage (Api.S.fromLedgerPParams Api.ShelleyBasedEraBabbage p) p


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

eraHistory :: GYTxMonadClb (Api.EraHistory Api.CardanoMode)
eraHistory = do
    (_, len) <- slotConfig'
    return $ Api.EraHistory Api.CardanoMode $ eh len
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
