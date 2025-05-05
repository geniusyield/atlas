{- |
Module      : GeniusYield.Test.Privnet.Examples.Misc
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Privnet.Examples.Misc (tests) where

import Control.Concurrent (threadDelay)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import GeniusYield.Scripts.TestToken
import GeniusYield.Types

import Data.Default (Default (..))
import Data.Foldable (find)
import Data.Set qualified as Set
import GeniusYield.Test.Privnet.Asserts
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Examples.Common
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Transaction.Common
import GeniusYield.TxBuilder

tests :: Setup -> TestTree
tests setup =
  testGroup
    "misc"
    [ testCaseSteps "Reference script for minting policy" $ \info -> withSetup info setup $ \ctx -> do
        utxoAsParam <- ctxRun ctx (ctxUser2 ctx) $ someUTxO PlutusV1
        let amt = 1
            tn = "mintByRef"
            policy = testTokenPolicy amt tn utxoAsParam
            policyAsScript = mintingPolicyToScript policy
            ac = GYToken (mintingPolicyId policy) tn

        refScript <- ctxRun ctx (ctxUserF ctx) $ addRefScriptToLimbo policyAsScript
        -- wait a tiny bit.
        threadDelay 1_000_000

        balance <- ctxQueryBalance ctx (ctxUser2 ctx)

        ctxRun ctx (ctxUser2 ctx) $ do
          txBodyMint <-
            buildTxBody @'PlutusV2 $
              mustHaveInput (GYTxIn utxoAsParam GYTxInWitnessKey)
                <> mustMint (GYBuildPlutusScript $ GYBuildPlutusScriptReference refScript policyAsScript) unitRedeemer tn amt
          signAndSubmitConfirmed_ txBodyMint

        -- wait a tiny bit.
        threadDelay 1_000_000

        balance' <- ctxQueryBalance ctx (ctxUser2 ctx)

        let diff = valueMinus balance' balance

        assertEqual "Must have gained 1 mint token" (valueAssetClass diff ac) 1
    , testCaseSteps "Exercising fee utxo" $ \info -> withSetup info setup $ \ctx -> do
        let sponsorUser = ctxUserF ctx
            endUser = ctxUser2 ctx
        utxos <- ctxRunQuery ctx $ utxosAtAddress (userAddr sponsorUser) Nothing
        let mfeeUtxo = find (\GYUTxO {..} -> valueAda utxoValue >= 20_000_000 && valueAssets utxoValue == Set.singleton GYLovelace) (utxosToList utxos)
        info $ "fee utxo: " <> show mfeeUtxo
        balanceBefore <- ctxQueryBalance ctx endUser
        body <- ctxRun ctx endUser $ do
          body <- buildTxBodyWithExtraConfiguration (def {gytxecFeeUtxo = mfeeUtxo}) $ mustHaveOutput $ mkGYTxOutNoDatum (userAddr endUser) mempty
          submitTxBodyConfirmed_ body $ map (GYSomeSigningKey . userPaymentSKey) [sponsorUser, endUser]
          pure body
        info $ "body: " <> show body
        balanceAfter <- ctxQueryBalance ctx endUser
        assertEqual "User balance must have not changed" balanceBefore balanceAfter
    , testCaseSteps "Donation" $ \info -> withSetup info setup $ \ctx -> do
        ctxRun ctx (ctxUserF ctx) $ do
          txBody <-
            buildTxBody @'PlutusV3 $
              mustHaveDonation 1_000_000
          signAndSubmitConfirmed_ txBody
    ]
