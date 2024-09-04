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
import           GeniusYield.TxBuilder

tests :: Setup -> TestTree
tests setup = testGroup "treat"
    [ testCaseSteps "plutusV1" $ \info -> withSetup info setup $ \ctx -> do
        let goldAC = ctxGold ctx

        ctxRun ctx (ctxUserF ctx) $ do
            addr <- scriptAddress treatValidatorV1
            txBodyPlace <- buildTxBody $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
            signAndSubmitConfirmed_ txBodyPlace

        threadDelay 1_000_000

        -- this fails as we tell that script is 'PlutusV1,
        -- but it uses V2 features.
        assertThrown isTxBodyErrorAutoBalance $ ctxRun ctx (ctxUser2 ctx) $ grabTreats  @'PlutusV1 treatValidatorV1 >>= traverse buildTxBody

    -- this is the same tests as for Gift 'PlutusV2.
    , testCaseSteps "plutusV2" $ \info -> withSetup info setup $ \ctx -> do
        let ironAC = ctxIron ctx

        -- grab existing treats to cleanup
        ctxRun ctx (ctxUserF ctx) $ do
            grabTreatsTx <- grabTreats  @'PlutusV2 treatValidatorV2 >>= traverse buildTxBody
            mapM_ signAndSubmitConfirmed grabTreatsTx

        threadDelay 1_000_000

        balance1 <- ctxQueryBalance ctx (ctxUserF ctx)
        balance2 <- ctxQueryBalance ctx (ctxUser2 ctx)

        ctxRun ctx (ctxUserF ctx) $ do
            addr <- scriptAddress treatValidatorV2
            txBodyPlace <- buildTxBody $ mustHaveOutput $ mkGYTxOut addr (valueSingleton ironAC 10) (datumFromPlutusData ())
            signAndSubmitConfirmed_ txBodyPlace

        -- wait a tiny bit.
        threadDelay 1_000_000

        ctxRun ctx (ctxUser2 ctx) $ do
            grabTreatsTx' <- grabTreats  @'PlutusV2 treatValidatorV2 >>= traverse buildTxBody
            mapM_ signAndSubmitConfirmed grabTreatsTx'

        -- wait a tiny bit.
        threadDelay 1_000_000

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
    :: forall u v m. (GYTxUserQueryMonad m, VersionIsGreaterOrEqual v u)
    => GYValidator v
    -> m (Maybe (GYTxSkeleton u))
grabTreats validator = do
    addr <- scriptAddress validator
    utxo <- utxosAtAddress addr Nothing
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
