{-|
Module      : GeniusYield.Test.Privnet.Examples.Misc
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

module GeniusYield.Test.Privnet.Examples.Misc (tests) where

import           Control.Concurrent                     (threadDelay)
import           Test.Tasty                             (TestTree, testGroup)
import           Test.Tasty.HUnit                       (testCaseSteps)

import           GeniusYield.Imports
import           GeniusYield.Scripts.TestToken
import           GeniusYield.Types

import           GeniusYield.Examples.Limbo             (addRefScript)
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Examples.Gift (resolveRefScript)
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class

tests :: Setup -> TestTree
tests setup = testGroup "misc"
    [ testCaseSteps "Reference script for minting policy" $ \info -> withSetup setup info $ \ctx -> do

        utxoAsParam <- ctxRunC ctx (ctxUser2 ctx) $ someUTxO PlutusV1
        let amt    = 1
            tn     = "mintByRef"
            policy = testTokenPolicy amt tn utxoAsParam
            policyAsScript = mintingPolicyToScript policy
            ac = GYToken (mintingPolicyId policy) tn

        txBodyRefScript <- ctxRunF ctx (ctxUserF ctx) $ addRefScript policyAsScript

        refScript <- resolveRefScript ctx txBodyRefScript (Some policyAsScript)
        -- wait a tiny bit.
        threadDelay 1_000_000

        balance <- ctxQueryBalance ctx (ctxUser2 ctx)

        txBodyMint <- ctxRunI ctx (ctxUser2 ctx) $ do
            return $
                 mustHaveInput (GYTxIn utxoAsParam GYTxInWitnessKey)
              <> mustMint (GYMintReference refScript policyAsScript) unitRedeemer tn amt
        void $ submitTx ctx (ctxUser2 ctx) txBodyMint

        -- wait a tiny bit.
        threadDelay 1_000_000

        balance' <- ctxQueryBalance ctx (ctxUser2 ctx)

        let diff = valueMinus balance' balance

        assertEqual "Must have gained 1 mint token" (valueAssetClass diff ac) 1
    ]
