{-|
Module      : GeniusYield.Test.Privnet.Examples.Treat
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Examples.Treat (tests) where

import           Control.Concurrent               (threadDelay)
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 (testCaseSteps)

import           GeniusYield.Imports
import           GeniusYield.Types

import           GeniusYield.Examples.Treat
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class

tests :: IO Setup -> TestTree
tests setup = testGroup "treat"
    [ testCaseSteps "plutusV1" $ \info -> withSetup setup info $ \ctx -> do
        let goldAC = ctxGold ctx

        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress treatValidatorV1
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        threadDelay 1_000_000

        -- this fails as we tell that script is 'PlutusV1,
        -- but it uses V2 features.
        assertThrown isTxBodyErrorAutoBalance $ ctxRunF ctx (ctxUser2 ctx) $ grabTreats  @'PlutusV1 treatValidatorV1

    -- this is the same tests as for Gift 'PlutusV2.
    , testCaseSteps "plutusV2" $ \info -> withSetup setup info $ \ctx -> do
        let ironAC = ctxIron ctx

        -- grab existing treats to cleanup
        grabTreatsTx <- ctxRunF ctx (ctxUserF ctx) $ grabTreats  @'PlutusV2 treatValidatorV2
        mapM_ (submitTx ctx (ctxUserF ctx)) grabTreatsTx

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        txBodyPlace <- ctxRunI ctx (ctxUserF ctx) $ do
            addr <- scriptAddress treatValidatorV2
            return $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())

        void $ submitTx ctx (ctxUserF ctx) txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        grabTreatsTx' <- ctxRunF ctx (ctxUser2 ctx) $ grabTreats  @'PlutusV2 treatValidatorV2
        mapM_ (submitTx ctx (ctxUser2 ctx)) grabTreatsTx'

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
    ]

grabTreats
    :: forall u v m. (GYTxMonad m, VersionIsGreaterOrEqual v u)
    => GYValidator v
    -> m (Maybe (GYTxSkeleton u))
grabTreats validator = do
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
