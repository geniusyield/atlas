module GeniusYield.Test.Privnet.Stake.Validator (
  stakeValidatorTests,
) where

import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.Test.Privnet.Stake.Utils
import           GeniusYield.Types
import           Test.Tasty                           (TestTree, testGroup)
import           Test.Tasty.HUnit                     (testCaseSteps)

aStakeValidatorHash :: GYStakeValidatorHash
aStakeValidatorHash = stakeValidatorHash aStakeValidator

stakeValidatorTests :: Setup -> TestTree
stakeValidatorTests setup = testGroup "stake"
  [ testCaseSteps "exercising stake credential registration, delegation, rewards claiming & de-registration via stake validator" $ \info -> withSetup info setup $ \ctx -> do
    stakeIntegrationTest (Just aStakeValidatorHash) info ctx
  ]
