{-|
Module      : GeniusYield.Test.Privnet.Examples.Gift
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

{-# LANGUAGE LambdaCase #-}

module GeniusYield.Test.Privnet.Examples.Gift (tests) where

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S
import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Lens                     ((.~))
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 (testCaseSteps)

import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust)
import           Data.Ratio                       ((%))
import qualified Data.Set                         as Set
import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.Types

import           GeniusYield.Examples.Gift
import           GeniusYield.Examples.Limbo
import           GeniusYield.Examples.Treat
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common     (collateralValue)

tests :: IO Setup -> TestTree
tests setup = testGroup "gift"
    [ testCaseSteps "plutusV1" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx
        let goldAC = ctxGold ctx

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV1
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabGifts  @'PlutusV1 giftValidatorV1
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2' <- ctxQueryBalance ctx (ctxUser2 ctx)

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

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabGifts  @'PlutusV1 giftValidatorV2
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2' <- ctxQueryBalance ctx (ctxUser2 ctx)

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

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ GYTxOutUseInlineDatum

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabGifts  @'PlutusV1 giftValidatorV2
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

        balance1' <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2' <- ctxQueryBalance ctx (ctxUser2 ctx)

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "Checking Vasil feature of Collateral Return and Total Collateral - Multi-asset collateral" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx

        ----------- Create a new user and fund it
        let ironAC = ctxIron ctx
        -- `newUser` just have one UTxO which will be used as collateral.
        newUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 200_000_000 <> valueSingleton ironAC 25) False
        info $ printf "Newly created user's address %s" (show $ userAddr newUser)
        ----------- (ctxUserF ctx) submits some gifts
        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput  (mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ()))
        assertBool "Collateral input shouldn't be set for this transaction" (txBodyCollateral txBodyPlace == mempty)
        assertBool "Return collateral shouldn't be set for this transaction" (txBodyCollateralReturnOutput txBodyPlace == Api.TxReturnCollateralNone)
        assertBool "Total collateral shouldn't be set for this transaction" (txBodyTotalCollateralLovelace txBodyPlace == 0)
        void $ submitTx ctx (ctxUserF ctx) txBodyPlace
        -- wait a tiny bit.
        threadDelay 1_000_000

        info $ printf "UTxOs at this new user"
        newUserUtxos <- ctxRunC ctx newUser $ utxosAtAddress (userAddr newUser)
        forUTxOs_ newUserUtxos (info . show)

        ---------- New user tries to grab it, since interacting with script, needs to give collateral
        grabGiftsTxBody <- ctxRunF ctx newUser $ grabGifts  @'PlutusV1 giftValidatorV2
        grabGiftsTxBody' <- case grabGiftsTxBody of
          Nothing   -> assertFailure "Unable to build tx"
          Just body -> return body
        retCollOutput@(Api.TxReturnCollateral _ (Api.TxOut retCollAddrApi _ _ _)) <- case txBodyCollateralReturnOutput grabGiftsTxBody' of
              Api.TxReturnCollateralNone -> fail "Return collateral is not present"
              retCollOutput' -> return retCollOutput'
        let totalCollateral = txBodyTotalCollateralLovelace grabGiftsTxBody'
            retCollValue = txBodyCollateralReturnOutputValue grabGiftsTxBody'
            retCollAddr = addressFromApi' retCollAddrApi
        info $ printf "Return collateral: %s" (show retCollOutput)
        info $ printf "Total collateral: %s" (show totalCollateral)
        assertBool "Return collateral value is zero" $  retCollValue /= mempty
        assertBool "Total collateral does not exist or value is not positive" $ totalCollateral > 0
        assertBool "Return collateral at different address" $ retCollAddr == userAddr newUser
        pp <- gyGetProtocolParameters $ ctxProviders ctx
        let colls = txBodyCollateral grabGiftsTxBody'
        colls' <- ctxRunC ctx (ctxUserF ctx) $ utxosAtTxOutRefs (Set.toList colls)
        assertBool "Collateral outputs not correctly setup" $ checkCollateral (foldMapUTxOs utxoValue colls') retCollValue (toInteger totalCollateral) (txBodyFee grabGiftsTxBody') (toInteger $ fromJust $ Api.S.protocolParamCollateralPercent pp)
        void $ submitTx ctx newUser grabGiftsTxBody'

    , testCaseSteps "Checking if collateral is reserved in case we send an exact 5 ada only UTxO as collateral (simulating browser's case) + is collateral spendable if we want?" $ \info -> withSetup setup info $ \ctx -> do
        ----------- Create a new user and fund it
        let ironAC = ctxIron ctx
            newUserValue = valueFromLovelace 200_000_000 <> valueSingleton ironAC 25
        newUser <- newTempUserCtx ctx (ctxUserF ctx) newUserValue True

        info $ printf "UTxOs at this new user"
        newUserUtxos <- ctxRunC ctx newUser $ utxosAtAddress (userAddr newUser)
        forUTxOs_ newUserUtxos (info . show)
        fiveAdaUtxo <- case find (\u -> utxoValue u == collateralValue) (utxosToList newUserUtxos) of
                         Nothing           -> fail "Couldn't find a 5-ada-only UTxO"
                         Just fiveAdaUtxo' -> return fiveAdaUtxo'
        assertThrown (\case BuildTxBalancingError (BalancingErrorInsufficientFunds _) -> True; _anyOther -> False) $ ctxRunFWithCollateral ctx newUser (utxoRef fiveAdaUtxo) False $ return $ Identity $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 3_000_000)
        -- Should be reserved if we also perform 5 ada check as it satisfies it.
        assertThrown (\case BuildTxBalancingError (BalancingErrorInsufficientFunds _) -> True; _anyOther -> False) $ ctxRunFWithCollateral ctx newUser (utxoRef fiveAdaUtxo) True $ return $ Identity $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 3_000_000)
        -- Would have thrown error if unable to build body.
        void $ ctxRunI ctx newUser $ return $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 3_000_000)

    , testCaseSteps "Checking for 'BuildTxNoSuitableCollateral' error" $ \info -> withSetup setup info $ \ctx -> do
        ----------- Create a new user and fund it
        let newUserValue = valueFromLovelace 4_000_000
        newUser <- newTempUserCtx ctx (ctxUserF ctx) newUserValue False

        info $ printf "UTxOs at this new user"
        newUserUtxos <- ctxRunC ctx newUser $ utxosAtAddress (userAddr newUser)
        forUTxOs_ newUserUtxos (info . show)
        assertThrown (\case BuildTxNoSuitableCollateral -> True; _anyOther -> False) $ ctxRunI ctx newUser $ return $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 2_000_000)

    , testCaseSteps "Checking if collateral is reserved in case we want it even if it's value is not 5 ada" $ \info -> withSetup setup info $ \ctx -> do
        ----------- Create a new user and fund it
        newUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 40_000_000) False
        -- Add another UTxO to be used as collateral.
        txBody <- ctxRunI ctx (ctxUserF ctx) $ return $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (valueFromLovelace 8_000_000)
        void $ submitTx ctx (ctxUserF ctx) txBody
        info $ printf "UTxOs at this new user"
        newUserUtxos <- ctxRunC ctx newUser $ utxosAtAddress (userAddr newUser)
        forUTxOs_ newUserUtxos (info . show)
        eightAdaUtxo <- case find (\u -> utxoValue u == valueFromLovelace 8_000_000) (utxosToList newUserUtxos) of
                          Nothing -> fail "Couldn't find a 8-ada-only UTxO"
                          Just u  -> return u
        let newUserValue = foldlUTxOs' (\a u -> a <> utxoValue u) mempty newUserUtxos
        assertThrown (\case BuildTxBalancingError (BalancingErrorInsufficientFunds _) -> True; _anyOther -> False) $ ctxRunFWithCollateral ctx newUser (utxoRef eightAdaUtxo) False $ return $ Identity $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 3_000_000)
        -- eight ada utxo won't satisfy 5 ada check and thus would be ignored
        void $ ctxRunFWithCollateral ctx newUser (utxoRef eightAdaUtxo) True $ return $ Identity $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr newUser) (newUserValue `valueMinus` valueFromLovelace 3_000_000)

    , testCaseSteps "Matching Reference Script from UTxO" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx

        txBodyRefScript <- ctxRunI ctx (ctxUserF ctx) $ addRefScript' (validatorToScript giftValidatorV2)

        ref <- do
          let refs = findRefScriptsInBody txBodyRefScript
          ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
              Just ref -> return ref
              Nothing  -> fail "Shouldn't happen: no ref in body"

          void $ submitTx ctx (ctxUserF ctx) txBodyRefScript
          return ref

        info $ "Reference at " ++ show ref

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- mUtxo <- gyQueryUtxoAtTxOutRef' (ctxQueryUtxos ctx) ref  -- another way
        mUtxo <- ctxRunC ctx (ctxUserF ctx) $ utxoAtTxOutRef ref
        case mUtxo of
          Just utxo -> maybe (assertFailure "No Reference Script exists in the added UTxO.") (\s -> if s == Some (validatorToScript giftValidatorV2) then info "Script matched, able to read reference script from UTxO." else assertFailure "Mismatch.") (utxoRefScript utxo)
          Nothing -> assertFailure "Couldn't find the UTxO containing added Reference Script."

    , testCaseSteps "refscript" $ \info -> withSetup setup info $ \ctx -> do
        giftCleanup ctx
        let ironAC = ctxIron ctx

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx (ctxUserF ctx) $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx (ctxUserF ctx) body
                return ref

        info $ "Reference at " ++ show ref

        -- put some gifts
        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- NOTE: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (ExtraneousScriptWitnessesUTXOW
        -- Apparently we MUST NOT include the script if there is a utxo input with that script. Even if we consume that utxo.
        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabGiftsRef ref giftValidatorV2
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

        -- Check final balance
        balance1' <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2' <- ctxQueryBalance ctx (ctxUser2 ctx)

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

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx (ctxUserF ctx) $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx (ctxUserF ctx) body
                return ref

        info $ "Reference at " ++ show ref

        -- put some gifts
        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- NOTE: TxValidationErrorInMode (ShelleyTxValidationError ShelleyBasedEraBabbage (ApplyTxError [UtxowFailure (FromAlonzoUtxowFail (WrappedShelleyEraFailure (ExtraneousScriptWitnessesUTXOW
        -- Apparently we MUST NOT include the script if there is a utxo input with that script. Even if we consume that utxo.
        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ do
            -- We spend the gifts and give the transaction (unused) reference input
            -- we need to use 'PlutusV2 here.
            s1 <- grabGifts  @'PlutusV2 giftValidatorV2
            return (s1 <|> Just (mustHaveRefInput ref))

        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

        -- Check final balance
        balance1' <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2' <- ctxQueryBalance ctx (ctxUser2 ctx)

        let diff1 = valueMinus balance1' balance1
        let diff2 = valueMinus balance2' balance2

        assertEqual "User1 token balance"
            (valueSingleton ironAC (-10))
            (snd (valueSplitAda diff1))

        assertEqual "User2 token balance"
            (valueSingleton ironAC 10)
            (snd (valueSplitAda diff2))

    , testCaseSteps "refscript_mixup" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test we consume 'PlutusV1 UTxO and 'PlutusV2 UTxO
        -- that should be fine, but we are using reference scripts for consuming 'PlutusV2
        -- and that is not supported.
        --
        let ironAC = ctxIron ctx

        -- in this test we create an output with reference script

        -- this creates utxo which looks like
        --
        -- 3c6ad9c5c512c06add1cd6bb513f1e879d5cadbe70f4762d4ff810d37ab9e0c0     1        1081810 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"
        txBodyRefScript <- ctxRunF ctx (ctxUserF ctx) $ addRefScript (validatorToScript giftValidatorV2)

        ref <- case txBodyRefScript of
            Left ref   -> return ref
            Right body -> do
                let refs = findRefScriptsInBody body
                ref <- case Map.lookup (Some (validatorToScript giftValidatorV2)) refs of
                    Just ref -> return ref
                    Nothing  -> fail "Shouldn't happen: no ref in body"

                void $ submitTx ctx (ctxUserF ctx) body
                return ref

        info $ "Reference at " ++ show ref

        -- put some V2 gifts
        txBodyPlaceV2 <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlaceV2

        info "Put V2 gifts"

        -- put some V1 gifts
        txBodyPlaceV1 <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlaceV1

        info "Put V1 gifts"

        -- Try to consume V1 and V2 gifts in the same transaction
        {- Doesn't compile.
        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx (ctxUser2 ctx) $ do
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
        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        {-
        let addNewGiftV2 :: GYTxMonad m => GYTxSkeleton 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
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
        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx (ctxUser2 ctx) $ grabGifts giftValidatorV1 >>= traverse addNewGiftV2
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
        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        -- TODO: NonOutputSupplimentaryDatums is thrown by other tests when this test is run.
        -- They fail to consume utxos with (inline) datums.
        -- We need to fix utxosDatums to also return whether the datum was inline.
        let addNewGiftV2 :: GYTxMonad m => GYTxSkeleton 'PlutusV2 -> m (GYTxSkeleton 'PlutusV2)
            addNewGiftV2 skeleton = do
                addr <- scriptAddress giftValidatorV2
                return $ skeleton <> mustHaveOutput GYTxOut
                    { gyTxOutAddress = addr
                    , gyTxOutValue   = valueSingleton ironAC 10
                    , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
                    , gyTxOutRefS    = Nothing
                    }

        grabGiftsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabGifts giftValidatorV2 >>= traverse addNewGiftV2
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx'

    , testCaseSteps "inlinedatum-v1v2" $ \info -> withSetup setup info $ \ctx -> do
        -- in this test we try to consume v1 and v2 script outputs in the same transaction.
        -- The v2 outputs have inline datums
        --
        -- This seems to be fine, so we can *consume* inline-datum outputs.
        let ironAC = ctxIron ctx

        txBodyPlace1 <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace1

        txBodyPlace2 <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress treatValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ GYTxOutUseInlineDatum

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace2

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabGiftsTx <- ctxRunF ctx (ctxUser2 ctx) $ do
          s1 <- grabGifts  @'PlutusV1 giftValidatorV1
          s2 <- grabGifts treatValidatorV2
          return (s1 <|> s2)

        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx

    , testCaseSteps "inlinedatum-in-v1" $ \info -> withSetup setup info $ \_ctx -> do
        -- in this test we try to consume v1 script output which has inline datums
        -- this doesn't work, and break things.
        return ()

        -- this test is commented out, as it breaks everything else.
        {-
        let ironAC = ctxIron ctx

        txBodyPlace1 <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress giftValidatorV1
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
                & gyTxOutDatumL .~ True

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace1

        -- wait a tiny bit.
        threadDelay 1_000_000

        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx (ctxUser2 ctx) $ grabGifts giftValidatorV1

        grabGiftsTx <- ctxRunF ctx (ctxUser2 ctx) $ grabGifts giftValidatorV1

        mapM_ (submitTx ctx (ctxUser2 ctx)) grabGiftsTx
        -}
    ]

giftCleanup :: Ctx -> IO ()
giftCleanup ctx = do
    threadDelay 1_000_000

    -- grab existing v2 gifts
    grabGiftsTx2 <- ctxRunF ctx (ctxUserF ctx) $ grabGifts  @'PlutusV2 giftValidatorV2
    mapM_ (submitTx ctx (ctxUserF ctx)) grabGiftsTx2

    -- grab existing v1 gifts
    grabGiftsTx1 <- ctxRunF ctx (ctxUserF ctx) $ grabGifts  @'PlutusV1 giftValidatorV1
    mapM_ (submitTx ctx (ctxUserF ctx)) grabGiftsTx1

    -- grab existing treats
    grabGiftsTx <- ctxRunF ctx (ctxUserF ctx) $ grabGifts  @'PlutusV2 treatValidatorV2
    mapM_ (submitTx ctx (ctxUserF ctx)) grabGiftsTx

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
    -> GYValidator 'PlutusV2
    -> m (Maybe (GYTxSkeleton 'PlutusV2))
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

-- | Function to check for consistency of collaterals with respect to ledger laws.
checkCollateral :: Integral a
                => GYValue  -- ^ Sum of values present in collateral inputs.
                -> GYValue  -- ^ Value present in return collateral output.
                -> Integer  -- ^ Total collateral lovelaces.
                -> a        -- ^ Transaction fees.
                -> a        -- ^ Collateral percent (Protocol parameter).
                -> Bool
checkCollateral inputValue returnValue totalCollateralLovelace txFee collPer =
     isEmptyValue balanceOther
  && balanceLovelace >= 0
  && totalCollateralLovelace == balanceLovelace
  && balanceLovelace>= ceiling (txFee * collPer % 100)  -- Api checks via `balanceLovelace * 100 >= txFee * collPer` which IMO works as `balanceLovelace` is an integer & 100 but in general `c >= ceil (a / b)` is not equivalent to `c * b >= a`.
  && inputValue == returnValue <> valueFromLovelace totalCollateralLovelace
  where (balanceLovelace, balanceOther) = valueSplitAda $ inputValue `valueMinus` returnValue
