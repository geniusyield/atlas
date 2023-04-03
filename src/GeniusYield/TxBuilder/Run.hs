{-# LANGUAGE LambdaCase #-}
{-|
Module      : GeniusYield.TxBuilder.Run
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Run
    ( GYTxMonadRun
    , asRun
    , asRandRun
    , liftRun
    , ownAddress
    , sendSkeleton
    , sendSkeleton'
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
import           Data.List                            ((\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import           Data.Time.Clock                      (NominalDiffTime, UTCTime)
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import qualified Ouroboros.Consensus.Cardano.Block    as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import           Ouroboros.Consensus.Util.Counting    (NonEmpty (NonEmptyCons, NonEmptyOne))
import           Plutus.Model
import qualified Plutus.Model.Fork.Ledger.Slot        as Fork
import qualified Plutus.Model.Fork.Ledger.TimeSlot    as Fork
import qualified Plutus.Model.Fork.Ledger.Tx          as Fork
import           Plutus.Model.Mock.ProtocolParameters
import           Plutus.Model.Stake
import qualified Plutus.V1.Ledger.Interval            as Plutus
import qualified Plutus.V2.Ledger.Api                 as Plutus
import qualified PlutusTx.Builtins.Internal           as Plutus

import           GeniusYield.Imports
import           GeniusYield.Transaction              (GYCoinSelectionStrategy (GYRandomImproveMultiAsset))
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

data GYTxRunEnv = GYTxRunEnv
    { runEnvPaymentSigningKey :: !GYPaymentSigningKey
    , runEnvCollateral        :: !(Maybe GYTxOutRef)
    }

newtype GYTxMonadRun a = GYTxMonadRun
    { unGYTxMonadRun :: ExceptT (Either String GYTxMonadException) (ReaderT GYTxRunEnv (RandT StdGen Run)) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader GYTxRunEnv)

instance MonadRandom GYTxMonadRun where
    getRandomR  = GYTxMonadRun . getRandomR
    getRandom   = GYTxMonadRun getRandom
    getRandomRs = GYTxMonadRun . getRandomRs
    getRandoms  = GYTxMonadRun getRandoms

asRandRun :: GYPaymentSigningKey
          -> Maybe GYTxOutRef
          -> GYTxMonadRun a
          -> RandT StdGen Run (Maybe a)
asRandRun skey mcollateral m = do
    e <- runReaderT (runExceptT $ unGYTxMonadRun m) GYTxRunEnv
            { runEnvPaymentSigningKey = skey
            , runEnvCollateral        = mcollateral
            }
    case e of
        Left (Left err)  -> lift (logError err) >> return Nothing
        Left (Right err) -> lift (logError (show err)) >> return Nothing
        Right a          -> return $ Just a

asRun :: StdGen
      -> GYPaymentSigningKey
      -> Maybe GYTxOutRef
      -> GYTxMonadRun a
      -> Run (Maybe a)
asRun g skey mcollateral m = evalRandT (asRandRun skey mcollateral m) g

ownAddress :: GYTxMonadRun GYAddress
ownAddress = do
    nid <- networkId
    asks $ addressFromPubKeyHash nid . pubKeyHash . paymentVerificationKey . runEnvPaymentSigningKey

liftRun :: Run a -> GYTxMonadRun a
liftRun = GYTxMonadRun . lift . lift . lift

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

    utxoAtTxOutRef ref = do
        mUtxosWithoutRefScripts   <- liftRun $ gets mockUtxos
        mRefScripts <- liftRun $ gets mockRefScripts  -- IMPORTANT: as the fate would have it, our fork because of being old, doesn't contain these inside `mockUtxos` (which is not the case for original)
        let m = mUtxosWithoutRefScripts <> mRefScripts
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
            -- We are in `Maybe` monad, and it's valid for the following to return `Nothing` so we wrap our `Nothing` with `return`.
            s <- case Plutus.txOutReferenceScript o of
                   Nothing -> return Nothing
                   Just sh -> case Map.lookup sh mScripts of
                                Nothing -> return Nothing
                                Just vs  ->
                                  if
                                  | isV1 vs   -> return $ Just (Some $ scriptFromPlutus @'PlutusV1 (versioned'content vs))
                                  | isV2 vs   -> return $ Just (Some $ scriptFromPlutus @'PlutusV2 (versioned'content vs))
                                  | otherwise -> return Nothing

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

    someUTxO = do
        addr        <- ownAddress
        mcollateral <- asks runEnvCollateral
        utxos       <- utxosAtAddress addr
        case find (\ref -> mcollateral /= Just ref) $ utxosRefs utxos of
            Nothing  -> throwError $ GYQueryUTxOException $ GYNoUtxosAtAddress [addr]
            Just ref -> return ref

    randSeed = return 42

sendSkeleton :: GYTxSkeleton v -> GYTxMonadRun GYTxId
sendSkeleton skeleton = snd <$> sendSkeleton' skeleton

sendSkeleton' :: GYTxSkeleton v -> GYTxMonadRun (Tx, GYTxId)
sendSkeleton' skeleton = do
    skey <- asks runEnvPaymentSigningKey
    body <- skeletonToTxBody skeleton

    dumpBody body

    let pkh     = pubKeyHashToPlutus $ pubKeyHash $ paymentVerificationKey skey
        keyPair = paymentSigningKeyToLedgerKeyPair skey
        tx1     =
            toExtra (mempty
                { Fork.txSignatures = Map.singleton pkh keyPair
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
        Left _         -> fail ""
        Right (_, tid) -> case txIdFromPlutus tid of
            Nothing   -> fail $ printf "invalid tid %s" $ show tid
            Just tid' -> return (tx5, tid')

  where

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
                            GYInReference refScriptORef gyScriptV2 -> return (Nothing, toExtra (mempty {Fork.txReferenceInputs = Set.singleton $ Fork.TxIn (txOutRefToPlutus refScriptORef) Nothing, Fork.txScripts = Map.singleton (scriptPlutusHash gyScriptV2) (Versioned Ledger.PlutusV2 $ scriptToPlutus gyScriptV2)}))
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
                        v = Versioned Ledger.PlutusV2 $ scriptToPlutus s
                    return $ Map.singleton sh v

        return $ tx <> toExtra mempty
            { Fork.txOutputs = [o]
            , Fork.txData    = dm
            , Fork.txScripts = sm
            }

    addMint :: Tx -> (Some GYMintingPolicy, (Map GYTokenName Integer, GYRedeemer)) -> GYTxMonadRun Tx
    addMint tx (Some mp, (m, r)) = do
        let pid = mintingPolicyId mp
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

    validatorToVersioned :: GYValidator v -> Versioned Plutus.Validator
    validatorToVersioned v = case validatorVersion v of
        SingPlutusV1 -> Versioned Ledger.PlutusV1 $ validatorToPlutus v
        SingPlutusV2 -> Versioned Ledger.PlutusV2 $ validatorToPlutus v

    mintingPolicyToVersioned :: GYMintingPolicy v -> Versioned Plutus.MintingPolicy
    mintingPolicyToVersioned v = case mintingPolicyVersion v of
        SingPlutusV1 -> Versioned Ledger.PlutusV1 $ mintingPolicyToPlutus v
        SingPlutusV2 -> Versioned Ledger.PlutusV2 $ mintingPolicyToPlutus v

skeletonToTxBody :: GYTxSkeleton v -> GYTxMonadRun GYTxBody
skeletonToTxBody skeleton = do
    ss <- systemStart
    eh <- eraHistory
    pp <- protocolParameters
    ps <- stakePools

    addr        <- ownAddress
    mcollateral <- asks runEnvCollateral
    case mcollateral of
        Nothing         -> throwError $ GYNoSuitableCollateralException 0 addr
        Just collateral -> do
            e <- buildTxCore ss eh pp ps GYRandomImproveMultiAsset (const id) [addr] addr collateral (return [Identity skeleton])
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

protocolParameters :: GYTxMonadRun Api.S.ProtocolParameters
protocolParameters = do
    pparams <- liftRun $ gets $ mockConfigProtocol . mockConfig
    return $ case pparams of
        AlonzoParams  p -> Api.S.fromLedgerPParams Api.ShelleyBasedEraAlonzo  p
        BabbageParams p -> Api.S.fromLedgerPParams Api.ShelleyBasedEraBabbage p

stakePools :: GYTxMonadRun (Set Api.S.PoolId)
stakePools = do
    pids <- liftRun $ gets $ Map.keys . stake'pools . mockStake
    foldM f Set.empty pids
  where
    f :: Set Api.S.PoolId -> PoolId -> GYTxMonadRun (Set Api.S.PoolId)
    f s pid = maybe
        (throwError $ GYConversionException $ GYLedgerToCardanoError $ DeserialiseRawBytesError "stakePools")
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

