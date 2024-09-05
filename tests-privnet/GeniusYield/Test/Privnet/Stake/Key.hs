module GeniusYield.Test.Privnet.Stake.Key (
  stakeKeyTests,
) where

import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Privnet.Stake.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

stakeKeyTests :: Setup -> TestTree
stakeKeyTests setup =
  testGroup
    "stake"
    [ testCaseSteps "exercising stake credential registration, delegation, rewards claiming & de-registration via stake key" $ \info -> withSetup info setup $ \ctx -> do
        stakeIntegrationTest Nothing info ctx
    ]
