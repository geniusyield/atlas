{-|
Module      : GeniusYield.Test.Privnet.Examples.Gift
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Examples.Gift (tests) where

import qualified Cardano.Api                      as Api
import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Lens                     ((&), (.~))
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 (testCaseSteps)

import qualified Data.Map.Strict                  as Map
import           GeniusYield.Imports
import           GeniusYield.Types

import           GeniusYield.Examples.Gift
import           GeniusYield.Examples.Limbo
import           GeniusYield.Examples.Treat
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class

tests :: IO Setup -> TestTree
tests setup = testGroup "gift"
    [ testCaseSteps "plutusV1" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx
        let goldAC = ctxGold ctx

        balance1 <- ctxQueryBalance ctx User1
        balance2 <- ctxQueryBalance ctx User2

        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV1
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx User2 $ grabGifts @PlutusV1 giftValidatorV1
        mapM_ (submitTx ctx User2) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx User1
        balance2' <- ctxQueryBalance ctx User2

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton goldAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton goldAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "plutusV2" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx

        let ironAC = ctxIron ctx

        balance1 <- ctxQueryBalance ctx User1
        balance2 <- ctxQueryBalance ctx User2

        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx User2 $ grabGifts @PlutusV1 giftValidatorV2
        mapM_ (submitTx ctx User2) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx User1
        balance2' <- ctxQueryBalance ctx User2

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "plutusV2-inlinedatum" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx
        let ironAC = ctxIron ctx

        balance1 <- ctxQueryBalance ctx User1
        balance2 <- ctxQueryBalance ctx User2

        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ GYTxOutUseInlineDatum

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx User2 $ grabGifts @PlutusV1 giftValidatorV2
        mapM_ (submitTx ctx User2) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx User1
        balance2' <- ctxQueryBalance ctx User2

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "Checking Vasil feature of Collateral Return and Total Collateral - Collateral input of only ada" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx

        ----------- Create a new user and fund it
        let ironAC = ctxIron ctx
        newUser <- newTempUserCtx ctx User1 (valueFromLovelace 200_000_000 <> valueSingleton ironAC 25)
        ----------- User1 submits some gifts.
        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
        void $ submitTx ctx User1 txBodyPlace
        ---------- New user tries to grab it, since interacting with script, needs to give collateral

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTxBody <- __ctxRunF ctx newUser $ grabGifts @PlutusV1 giftValidatorV2
        grabGiftsTxBody' <- case grabGiftsTxBody of
          Nothing   -> assertFailure "Unable to build tx"
          Just body -> return body
        let returnCollateralOutput = txBodyCollateralReturnOutput grabGiftsTxBody'
            totalCollateral = txBodyTotalCollateralLovelace grabGiftsTxBody'
        info $ printf "Return collateral: %s" (show returnCollateralOutput)
        info $ printf "Total collateral: %s" (show totalCollateral)
        assertBool "Return collateral does not exist" $ returnCollateralOutput /= Api.TxReturnCollateralNone
        assertBool "Total collateral does not exist" $ totalCollateral /= 0
        void $ __submitTx ctx newUser grabGiftsTxBody'

    , testCaseSteps "Matching Reference Script from UTxO" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx

        txBodyRefScript <- ctxRunI ctx User1 $ addRefScript' (validatorToScript giftValidatorV2)

        ref <- do
          let refs = findRefScriptsInBody txBodyRefScript
          ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
              Just ref -> return ref
              Nothing  -> fail "Shouldn't happen: no ref in body"

          void $ submitTx ctx User1 txBodyRefScript
          return ref

        info $ "Reference at " ++ show ref

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- mUtxo <- gyQueryUtxoAtTxOutRef' (ctxQueryUtxos ctx) ref  -- another way
        mUtxo <- ctxRunC ctx User1 $ utxoAtTxOutRef ref
        case mUtxo of
          Just utxo -> maybe (assertFailure "No Reference Script exists in the added UTxO.") (\s -> if s == Some (validatorToScript giftValidatorV2) then info "Script matched, able to read reference script from UTxO." else assertFailure "Mismatch.") (utxoRefScript utxo)
          Nothing -> assertFailure "Couldn't find the UTxO containing added Reference Script."

    , testCaseSteps "refscript" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx
        let ironAC = ctxIron ctx

        balance1 <- ctxQueryBalance ctx User1
        balance2 <- ctxQueryBalance ctx User2

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx User1 $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx User1 body
                return ref

        info $ "Reference at " ++ show ref

        -- put some gifts
        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- NOTE: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (ExtraneousScriptWitnessesUTXOW
        -- Apparently we MUST NOT include the script if there is a utxo input with that script. Even if we consume that utxo.
        grabGiftsTx' <- ctxRunF ctx User2 $ grabGiftsRef ref giftValidatorV2
        mapM_ (submitTx ctx User2) grabGiftsTx'

        -- Check final balance
        balance1' <- ctxQueryBalance ctx User1
        balance2' <- ctxQueryBalance ctx User2

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "refinputs" $ \info -> withSetup setup info $ \ctx -> do
        -- this is a bad test, but for proper one we'll need a script
        -- actually using reference input provided.
        giftCleanup ctx
        let ironAC = ctxIron ctx

        balance1 <- ctxQueryBalance ctx User1
        balance2 <- ctxQueryBalance ctx User2

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx User1 $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx User1 body
                return ref

        info $ "Reference at " ++ show ref

        -- put some gifts
        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- NOTE: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (ExtraneousScriptWitnessesUTXOW
        -- Apparently we MUST NOT include the script if there is a utxo input with that script. Even if we consume that utxo.
        grabGiftsTx' <- ctxRunF ctx User2 $ do
            -- We spend the gifts and give the transaction (unused) reference input
            -- we need to use PlutusV2 here.
            s1 <- grabGifts @PlutusV2 giftValidatorV2
            return (s1 <|> Just (mustHaveRefInput ref))

        mapM_ (submitTx ctx User2) grabGiftsTx'

        -- Check final balance
        balance1' <- ctxQueryBalance ctx User1
        balance2' <- ctxQueryBalance ctx User2

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "refscript_mixup" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test we consume PlutusV1 UTxO and PlutusV2 UTxO
        -- that should be fine, but we are using reference scripts for consuming PlutusV2
        -- and that is not supported.
        --
        let ironAC = ctxIron ctx

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx User1 $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx User1 body
                return ref

        info $ "Reference at " ++ show ref

        -- put some V2 gifts
        txBodyPlaceV2 <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlaceV2

        info "Put V2 gifts"

        -- put some V1 gifts
        txBodyPlaceV1 <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlaceV1

        info "Put V1 gifts"

        -- Try to consume V1 and V2 gifts in the same transaction
        {- Doesn't compile.
        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx User2 $ do
            sV2 <- grabGiftsRef ref giftValidatorV2
            sV1 <- grabGifts giftValidatorV1
            return (liftA2 (<>) sV2 sV1)
        -}

    , testCaseSteps "inline datums V1+V2" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test we consume UTxO with Plutus V1 script
        -- and in the same transaction create an output where we force inline datum usage
        --
        -- This doesn't work.
        let ironAC = ctxIron ctx

        -- place a gift, plutus version V1
        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        {-
        let addNewGiftV2 :: GYTxMonad m => GYTxSkeleton PlutusV2 -> m (GYTxSkeleton PlutusV2)
            addNewGiftV2 skeleton = do
                addr <- scriptAddress giftValidatorV2
                return $ skeleton <> mustHaveOutput GYTxOut
                    { gyTxOutAddress = addr
                    , gyTxOutValue   = valueSingleton ironAC 10
                    , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    }
        -}

        -- this doesnt' work. Mixing V1 scripts with V2 features
        {- Doesn't compile.
        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx User2 $ grabGifts giftValidatorV1 >>= traverse addNewGiftV2
        -}

        -- wait a tiny bit.
        threadDelay 1_000_000

    , testCaseSteps "inline datums V2" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test, there are only V2 scripts
        -- so everything seems to work.
        --
        -- i.e. words we need to make the previous test "inline datums V1+V2" not work:
        -- - we cannot consume V1 script utxos (we know the version!)
        -- - and create outputs with inline datums in the same transaction.
        --
        -- TODO: change gyTxOutDatumInline field to a GADT which can be "true" only when transaction is "V2 scripts only" #25
        --       (https://github.com/geniusyield/atlas/issues/25)
	    --
        -- TODO: later these tests fail due NonOutputSupplimentaryDatums. The reason is wrong grabGifts.
	    --       We should use GyTxInDatum True there for datums #26
	    --       (https://github.com/geniusyield/atlas/issues/26)
        --
        let ironAC = ctxIron ctx

        -- place a gift, plutus version V1
        txBodyPlace <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- TODO: NonOutputSupplimentaryDatums is thrown by other tests when this test is run.
        -- They fail to consume utxos with (inline) datums.
        -- We need to fix utxosDatums to also return whether the datum was inline.
        let addNewGiftV2 :: GYTxMonad m => GYTxSkeleton PlutusV2 -> m (GYTxSkeleton PlutusV2)
            addNewGiftV2 skeleton = do
                addr <- scriptAddress giftValidatorV2
                return $ skeleton <> mustHaveOutput GYTxOut
                    { gyTxOutAddress = addr
                    , gyTxOutValue   = valueSingleton ironAC 10
                    , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    }

        grabGiftsTx' <- ctxRunF ctx User2 $ grabGifts giftValidatorV2 >>= traverse addNewGiftV2
        mapM_ (submitTx ctx User2) grabGiftsTx'

    , testCaseSteps "inlinedatum-v1v2" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test we try to consume v1 and v2 script outputs in the same transaction.
        -- The v2 outputs have inline datums
        --
        -- This seems to be fine, so we can *consume* inline-datum outputs.
        let ironAC = ctxIron ctx

        txBodyPlace1 <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx User1 txBodyPlace1

        txBodyPlace2 <- ctxRunI ctx User1 $ do
            addr <- scriptAddress treatValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ GYTxOutUseInlineDatum

        void $ submitTx ctx User1 txBodyPlace2

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx <- ctxRunF ctx User2 $ do
          s1 <- grabGifts @PlutusV1 giftValidatorV1
          s2 <- grabGifts treatValidatorV2
          return (s1 <|> s2)

        mapM_ (submitTx ctx User2) grabGiftsTx

    , testCaseSteps "inlinedatum-in-v1" $ \info -> withSetup setup info $ \_ctx -> do
        -- in this test we try to consume v1 script output which has inline datums
        -- this doesn't work, and break things.
        return ()

        -- this test is commented out, as it breaks everything else.
        {-
        let ironAC = ctxIron ctx

        txBodyPlace1 <- ctxRunI ctx User1 $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ True

        void $ submitTx ctx User1 txBodyPlace1

        -- wait a tiny bit.
        threadDelay 1_000_000

        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx User2 $ grabGifts giftValidatorV1

        grabGiftsTx <- ctxRunF ctx User2 $ grabGifts giftValidatorV1

        mapM_ (submitTx ctx User2) grabGiftsTx
        -}
    ]

giftCleanup :: Ctx -> IO ()
giftCleanup ctx = do
    threadDelay 1_000_000

    -- grab existing v2 gifts
    grabGiftsTx2 <- ctxRunF ctx User1 $ grabGifts @PlutusV2 giftValidatorV2
    mapM_ (submitTx ctx User1) grabGiftsTx2

    -- grab existing v1 gifts
    grabGiftsTx1 <- ctxRunF ctx User1 $ grabGifts @PlutusV1 giftValidatorV1
    mapM_ (submitTx ctx User1) grabGiftsTx1

    -- grab existing treats
    grabGiftsTx <- ctxRunF ctx User1 $ grabGifts @PlutusV2 treatValidatorV2
    mapM_ (submitTx ctx User1) grabGiftsTx

    threadDelay 1_000_000

grabGifts
    :: forall u v m. (GYTxMonad m, VersionIsGreaterOrEqual v u)
    => GYValidator v
    -> m (Maybe (GYTxSkeleton u))
grabGifts validator = do
    addr <- scriptAddress validator
    utxo <- utxosAtAddress addr
    datums <- utxosDatums utxo

    if null datums
    then return Nothing
    else return $ Just $ mconcat
        [ mustHaveInput GYTxIn
            { gyTxInTxOutRef = oref
            , gyTxInWitness  = GYTxInWitnessScript
                (GYInScript validator)
                (datumFromPlutus' od)
                unitRedeemer
            }
        | (oref, (_addr, _value, od)) <- itoList datums
        ]

-- | Grab gifts using a referenced validator.
grabGiftsRef
    :: GYTxMonad m
    => GYTxOutRef
    -> GYValidator PlutusV2
    -> m (Maybe (GYTxSkeleton PlutusV2))
grabGiftsRef ref validator = do
    addr <- scriptAddress validator
    utxo <- utxosAtAddress addr
    datums <- utxosDatums utxo

    if null datums
    then return Nothing
    else return $ Just $ mconcat
        [ mustHaveInput GYTxIn
            { gyTxInTxOutRef = oref
            , gyTxInWitness  = GYTxInWitnessScript
                (GYInReference ref $ validatorToScript validator)
                (datumFromPlutus' od)
                unitRedeemer
            }
        | (oref, (_addr, _value, od)) <- itoList datums
        ]
