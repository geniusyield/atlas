{-# LANGUAGE LambdaCase #-}
{-|
Module      : GeniusYield.TxBuilder.Run
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Run
    ( Wallet (..)
    , walletAddress
    , GYTxRunState (..)
    , GYTxMonadRun
    , asRun
    , asRandRun
    , liftRun
    , ownAddress
    , sendSkeleton
    , sendSkeleton'
    , sendSkeletonWithWallets
    , networkIdRun
    ) where

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Ledger.Alonzo.Language       as Ledger
import qualified Cardano.Ledger.BaseTypes             as Ledger
import           Cardano.Slotting.Time                (RelativeTime (RelativeTime),
                                                       mkSlotLength)
import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable                        (foldMap')
import           Data.List                            ((\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map.Strict                      as Map
import           Data.Semigroup                       (Sum (..))
import qualified Data.Set                             as Set
import           Data.Time.Clock                      (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import qualified Ouroboros.Consensus.Cardano.Block    as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import           Data.SOP.Counting    (NonEmpty (NonEmptyCons, NonEmptyOne))
import           Plutus.Model
import qualified Cardano.Simple.Ledger.Slot           as Fork
import qualified Cardano.Simple.Ledger.TimeSlot       as Fork
import qualified Cardano.Simple.Ledger.Tx             as Fork
import           Plutus.Model.Mock.ProtocolParameters
import           Plutus.Model.Stake
import qualified PlutusLedgerApi.V1.Interval          as Plutus
import qualified PlutusLedgerApi.V2                   as Plutus
import qualified PlutusTx.Builtins.Internal           as Plutus

import           GeniusYield.Imports
import           GeniusYield.Transaction              (GYCoinSelectionStrategy (GYRandomImproveMultiAsset))
import           GeniusYield.Transaction.Common       (adjustTxOut, minimumUTxO)
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import qualified Cardano.Simple.PlutusLedgerApi.V1.Scripts as Fork
import           Data.Sequence                        (viewr, ViewR (..))

type WalletName = String

-- | Testing Wallet representation.
data Wallet = Wallet
    { walletPaymentSigningKey :: !GYPaymentSigningKey
    , walletNetworkId         :: !GYNetworkId
    , walletName              :: !WalletName
    }
    deriving (Show, Eq, Ord)

-- | Gets a GYAddress of a testing wallet.
walletAddress :: Wallet -> GYAddress
walletAddress Wallet{..} = addressFromPubKeyHash walletNetworkId $ pubKeyHash $
                           paymentVerificationKey walletPaymentSigningKey

instance HasAddress Wallet where
    toAddress = addressToPlutus . walletAddress

newtype GYTxRunEnv = GYTxRunEnv { runEnvWallet :: Wallet }

type FeesLovelace = Sum Integer
type MinAdaLovelace = Sum Integer

newtype GYTxRunState = GYTxRunState { walletExtraLovelace :: Map WalletName (FeesLovelace, MinAdaLovelace) }

newtype GYTxMonadRun a = GYTxMonadRun
    { unGYTxMonadRun :: ExceptT (Either String GYTxMonadException) (StateT GYTxRunState (ReaderT GYTxRunEnv (RandT StdGen Run))) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader GYTxRunEnv, MonadState GYTxRunState)

instance MonadRandom GYTxMonadRun where
    getRandomR  = GYTxMonadRun . getRandomR
    getRandom   = GYTxMonadRun getRandom
    getRandomRs = GYTxMonadRun . getRandomRs
    getRandoms  = GYTxMonadRun getRandoms

asRandRun :: Wallet
          -> GYTxMonadRun a
          -> RandT StdGen Run (Maybe a)
asRandRun w m = do
    e <- runReaderT (evalStateT (runExceptT $ unGYTxMonadRun m) $ GYTxRunState Map.empty) $ GYTxRunEnv w
    case e of
        Left (Left err)  -> lift (logError err) >> return Nothing
        Left (Right err) -> lift (logError (show err)) >> return Nothing
        Right a          -> return $ Just a

asRun :: StdGen
      -> Wallet
      -> GYTxMonadRun a
      -> Run (Maybe a)
asRun g w m = evalRandT (asRandRun w m) g

ownAddress :: GYTxMonadRun GYAddress
ownAddress = do
    nid <- networkId
    asks $ addressFromPubKeyHash nid . pubKeyHash . paymentVerificationKey . walletPaymentSigningKey . runEnvWallet

liftRun :: Run a -> GYTxMonadRun a
liftRun = GYTxMonadRun . lift . lift . lift . lift

networkIdRun :: Run GYNetworkId
networkIdRun = do
    n <- gets $ mockConfigNetworkId . mockConfig
    return $ case n of
        Ledger.Mainnet -> GYMainnet
        Ledger.Testnet -> GYTestnetPreprod

instance MonadFail GYTxMonadRun where
    fail = GYTxMonadRun . throwError . Left

instance MonadError GYTxMonadException GYTxMonadRun where

    throwError = GYTxMonadRun . throwError . Right

    catchError m handler = GYTxMonadRun $ catchError (unGYTxMonadRun m) $ \case
        Left  err -> throwError $ Left err
        Right err -> unGYTxMonadRun $ handler err

instance GYTxQueryMonad GYTxMonadRun where

    networkId = liftRun networkIdRun

    lookupDatum h = liftRun $ do
        mdh <- gets mockDatums
        return $ do
            d <- Map.lookup (datumHashToPlutus h) mdh
            return $ datumFromPlutus d

    utxosAtAddress addr = do
        refs  <- liftRun $ txOutRefAt $ addressToPlutus addr
        utxos <- wither f refs
        return $ utxosFromList utxos
      where
        f :: Plutus.TxOutRef -> GYTxMonadRun (Maybe GYUTxO)
        f ref = do
            case txOutRefFromPlutus ref of
                Left _     -> return Nothing
                Right ref' -> utxoAtTxOutRef ref'

    utxosAtPaymentCredential = const $ pure Nothing

    utxoAtTxOutRef ref = do
        mUtxosWithoutRefScripts   <- liftRun $ gets mockUtxos
        let m = mUtxosWithoutRefScripts
        mScripts <- liftRun $ gets mockScripts
        nid <- networkId
        return $ do
            o <- Map.lookup (txOutRefToPlutus ref) m
            a <- rightToMaybe $ addressFromPlutus nid $ Plutus.txOutAddress o
            v <- rightToMaybe $ valueFromPlutus       $ Plutus.txOutValue   o
            d <- case Plutus.txOutDatum o of
                    Plutus.NoOutputDatum      -> return GYOutDatumNone
                    Plutus.OutputDatumHash h' -> GYOutDatumHash <$> rightToMaybe (datumHashFromPlutus h')
                    Plutus.OutputDatum d      -> return $ GYOutDatumInline $ datumFromPlutus d
            let s = do
                  sh <- Plutus.txOutReferenceScript o
                  vs <- Map.lookup sh mScripts
                  if | isV1 vs   -> Just (Some $ scriptFromSerialisedScript @'PlutusV1 (coerce $ versioned'content vs))
                     | isV2 vs   -> Just (Some $ scriptFromSerialisedScript @'PlutusV2 (coerce $ versioned'content vs))
                     | otherwise -> Nothing

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

    currentSlot = do
        s <- liftRun $ Fork.getSlot <$> Plutus.Model.currentSlot
        case slotFromInteger s of
            Nothing -> throwError $ GYConversionException $ GYInvalidSlot s
            Just s' -> return s'

    logMsg ns s msg = liftRun $ case s of
        GYDebug   -> logInfo  $ printf "%s [DEBUG]: %s" ns msg
        GYInfo    -> logInfo  $ printf "%s [INFO]: %s"  ns msg
        GYWarning -> logInfo  $ printf "%s [WARN]: %s"  ns msg
        GYError   -> logError $ printf "%s [ERROR]: %s" ns msg

instance GYTxMonad GYTxMonadRun where

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
sendSkeletonWithWallets :: GYTxSkeleton v -> [Wallet] -> GYTxMonadRun GYTxId
sendSkeletonWithWallets skeleton ws = snd <$> sendSkeleton' skeleton ws

sendSkeleton :: GYTxSkeleton v -> GYTxMonadRun GYTxId
sendSkeleton skeleton = snd <$> sendSkeleton' skeleton  []


sendSkeleton' :: GYTxSkeleton v -> [Wallet]  -> GYTxMonadRun (Tx, GYTxId)
sendSkeleton' skeleton ws = do
    w <- asks runEnvWallet
    let sigs = walletSignatures (w:ws)
    body <- skeletonToTxBody skeleton
    pp <- protocolParameters
    modify (updateWalletState w pp body)
    dumpBody body

    let tx1     =
            toExtra (mempty
                { Fork.txSignatures = sigs
                , Fork.txValidRange       = case txBodyValidityRange body of
                    (Nothing, Nothing) -> Plutus.always
                    (Nothing, Just ub) -> Plutus.to $ slot ub
                    (Just lb, Nothing) -> Plutus.from $ slot lb
                    (Just lb, Just ub) -> Plutus.interval (slot lb) (slot ub)
                })                             <>
            payFee (Lovelace $ txBodyFee body) <>
            mconcat [collateralInput $ txOutRefToPlutus ref | ref <- Set.toList $ txBodyCollateral body]

    tx2 <- foldM addInput  tx1 $ txBodyTxIns body
    tx3 <- foldM addReferenceInput tx2 $ txBodyTxInsReference body \\ skeletonToRefScriptsORefs skeleton
    tx4 <- foldM addOutput tx3 $ utxosToList $ txBodyUTxOs body
    tx5 <- foldM addMint   tx4 $ Map.toList $ gytxMint skeleton

    e <- liftRun $ do
        logInfo $ show tx5
        sendTx tx5
    case e of
        Left fr -> fail $ show fr
        Right _ -> do
          mtxs <- liftRun $ gets mockTxs
          let tid = case viewr (unLog mtxs) of EmptyR -> error "Absurd (sendSkeleton'): Sequence can't be empty"; (_ :> a) -> txStatId $ snd a in
            case txIdFromPlutus tid of
              Left e'   -> fail $ printf "invalid tid %s, error: %s" (show tid) (show e')
              Right tid' -> return (tx5, tid')

  where
    walletSignatures =
      let walletPubKeyHash = pubKeyHashToPlutus . pubKeyHash . paymentVerificationKey . walletPaymentSigningKey
          walletKeyPair = paymentSigningKeyToLedgerKeyPair . walletPaymentSigningKey
       in Map.fromList . map (\w -> (walletPubKeyHash w, walletKeyPair w))


    -- Updates the wallet state.
    -- Updates extra lovelace required for fees & minimum ada requirements against the wallet sending this transaction.
    updateWalletState :: Wallet -> Api.BundledProtocolParameters Api.BabbageEra -> GYTxBody -> GYTxRunState -> GYTxRunState
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

    slot :: GYSlot -> Fork.Slot
    slot = Fork.Slot . slotToInteger

    dumpBody :: GYTxBody -> GYTxMonadRun ()
    dumpBody body = do
        ins <- mapM utxoAtTxOutRef' $ txBodyTxIns body
        refIns <- mapM utxoAtTxOutRef' $ txBodyTxInsReference body
        liftRun $ logInfo $ printf "fee: %d lovelace\nmint value: %s\nvalidity range: %s\ncollateral: %s\ntotal collateral: %d\ninputs:\n\n%sreference inputs:\n\n%soutputs:\n\n%s"
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

    addInput :: Tx -> GYTxOutRef -> GYTxMonadRun Tx
    addInput tx ref = do
        let ref' = txOutRefToPlutus ref
        utxo <- utxoAtTxOutRef' ref
        case addressToPubKeyHash $ utxoAddress utxo of
            Nothing -> do
                Some w <- findWitness ref
                case w of
                    GYTxInWitnessKey          -> fail $ printf "expected script witness for %s" ref
                    GYTxInWitnessScript s d r -> do
                        (vs, forRef) <- case s of
                            GYInScript v      -> return (Just (validatorToVersioned v), mempty)
                            GYInReference refScriptORef gyScriptV2 -> return (Nothing, toExtra (mempty {Fork.txReferenceInputs = Set.singleton $ Fork.TxIn (txOutRefToPlutus refScriptORef) Nothing, Fork.txScripts = Map.singleton (scriptPlutusHash gyScriptV2) (Versioned Ledger.PlutusV2 $ coerce $ scriptToSerialisedScript gyScriptV2)}))
                        return $ tx <> toExtra (mempty
                            { Fork.txInputs = Set.singleton $ Fork.TxIn ref' $ Just $ Fork.ConsumeScriptAddress
                                vs
                                (redeemerToPlutus r)
                                (datumToPlutus d)
                            }) <> forRef
            Just _  -> return $ tx <> spendPubKey ref'

    addReferenceInput :: Tx -> GYTxOutRef -> GYTxMonadRun Tx
    addReferenceInput tx ref = do
      let ref' = txOutRefToPlutus ref
      utxo <- utxoAtTxOutRef' ref
      case utxoOutDatum utxo of
        GYOutDatumHash dh -> do  -- Caution! Currently we don't support referring datum for such an input! Though have written code here in case framework adds support of it later.
          d <- findDatum dh
          return $ tx <> toExtra (
            mempty {
              Fork.txReferenceInputs = Set.singleton $ Fork.TxIn ref' Nothing,
              Fork.txData = Map.singleton (datumHashToPlutus dh) (datumToPlutus d)
              }
            )
        _InlineOrNone -> return $ tx <> toExtra (
          mempty {
              Fork.txReferenceInputs = Set.singleton $ Fork.TxIn ref' Nothing
            }
          )

    addOutput :: Tx -> GYUTxO -> GYTxMonadRun Tx
    addOutput tx utxo = do
        let o = utxoToPlutus utxo
        dm <- case utxoOutDatum utxo of
                GYOutDatumNone     -> return Map.empty
                GYOutDatumInline _ -> return Map.empty
                GYOutDatumHash dh  -> do
                    d <- findDatum dh
                    return $ Map.singleton (datumHashToPlutus dh) (datumToPlutus d)

        sm <- case utxoRefScript utxo of
                Nothing       -> return Map.empty
                Just (Some s) -> do
                    let sh = scriptPlutusHash s
                        v = Versioned Ledger.PlutusV2 $ coerce $ scriptToSerialisedScript s
                    return $ Map.singleton sh v

        return $ tx <> toExtra mempty
            { Fork.txOutputs = [o]
            , Fork.txData    = dm
            , Fork.txScripts = sm
            }

    addMint :: Tx -> (GYMintScript v, (Map GYTokenName Integer, GYRedeemer)) -> GYTxMonadRun Tx
    addMint tx (mp, (m, r)) = do
        let pid = mintingPolicyIdFromWitness mp
            vmp = mintingPolicyToVersioned mp
            r'  =  redeemerToPlutus r
            v   = valueToPlutus $ foldMap (\(tn, n) -> valueSingleton (GYToken pid tn) n) $ Map.toList m
        return $ tx <> mintValue (TypedPolicy vmp) r' v

    findWitness :: GYTxOutRef -> GYTxMonadRun (Some GYTxInWitness)
    findWitness ref = case find (\GYTxIn{..} -> gyTxInTxOutRef == ref) $ gytxIns skeleton of
                    Nothing -> fail $ printf "missing input for %s" ref
                    Just i  -> return $ Some $ gyTxInWitness i

    findDatum :: GYDatumHash -> GYTxMonadRun GYDatum
    findDatum dh = go $ gytxOuts skeleton
      where
        go :: [GYTxOut v] -> GYTxMonadRun GYDatum
        go []       = fail $ printf "datum hash without corresponding datum: %s" $ show dh
        go (o : os) = case gyTxOutDatum o of
            Nothing                 -> go os
            Just (d, _)
                | hashDatum d == dh -> return d
                | otherwise         -> go os

    validatorToVersioned :: GYValidator v -> Versioned Fork.Validator
    validatorToVersioned v = case validatorVersion v of
        SingPlutusV1 -> Versioned Ledger.PlutusV1 $ coerce $ validatorToSerialisedScript v
        SingPlutusV2 -> Versioned Ledger.PlutusV2 $ coerce $ validatorToSerialisedScript v

    mintingPolicyToVersioned :: GYMintScript v -> Versioned Fork.MintingPolicy
    mintingPolicyToVersioned v = case mintingPolicyVersionFromWitness v of
        PlutusV1 -> Versioned Ledger.PlutusV1 $ coerce $ gyMintScriptToSerialisedScript v
        PlutusV2 -> Versioned Ledger.PlutusV2 $ coerce $ gyMintScriptToSerialisedScript v

skeletonToTxBody :: GYTxSkeleton v -> GYTxMonadRun GYTxBody
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

slotConfig' :: GYTxMonadRun (UTCTime, NominalDiffTime)
slotConfig' = liftRun $ do
    sc <- gets $ mockConfigSlotConfig . mockConfig
    let len  = fromInteger (Fork.scSlotLength sc) / 1000
        zero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $ Fork.scSlotZeroTime sc
    return (zero, len)

systemStart :: GYTxMonadRun Api.SystemStart
systemStart = gyscSystemStart <$> slotConfig

protocolParameters :: GYTxMonadRun (Api.S.BundledProtocolParameters Api.S.BabbageEra)
protocolParameters = do
    pparams <- liftRun $ gets $ mockConfigProtocol . mockConfig
    return $ case pparams of
        AlonzoParams  _ -> error "Run.hs/protocolParameters: Only support babbage era parameters"
        BabbageParams p -> Api.BundleAsShelleyBasedProtocolParameters Api.ShelleyBasedEraBabbage (Api.S.fromLedgerPParams Api.ShelleyBasedEraBabbage p) p

stakePools :: GYTxMonadRun (Set Api.S.PoolId)
stakePools = do
    pids <- liftRun $ gets $ Map.keys . stake'pools . mockStake
    foldM f Set.empty pids
  where
    f :: Set Api.S.PoolId -> PoolId -> GYTxMonadRun (Set Api.S.PoolId)
    f s pid = either
        (\e -> throwError $ GYConversionException $ GYLedgerToCardanoError $ DeserialiseRawBytesError ("stakePools, error: " <> fromString (show e)))
        (\pid' -> return $ Set.insert pid' s)
        $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsStakePoolKey) bs
      where
        Plutus.BuiltinByteString bs = Plutus.getPubKeyHash $ unPoolId pid

eraHistory :: GYTxMonadRun (Api.EraHistory Api.CardanoMode)
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
