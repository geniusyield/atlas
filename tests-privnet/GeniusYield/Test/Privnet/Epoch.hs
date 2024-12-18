module GeniusYield.Test.Privnet.Epoch (
  epochTests,
) where

import Control.Lens (each, (%~), (&))
import Control.Monad (void, when)
import GeniusYield.Test.Privnet.Asserts (assertEqual)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder (GYTxSpecialQueryMonad (protocolParams), waitUntilSlot_)
import GeniusYield.TxBuilder.Class (gyLogInfo')
import GeniusYield.Types
import GeniusYield.Types (slotFromWord64, slotToWord64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

epochTests :: Setup -> TestTree
epochTests setup =
  testGroup
    "epoch"
    [ testCaseSteps "next epoch is obtained correctly" $ \info -> withSetup info setup $ \ctx -> do
        ctxRunQuery ctx $ do
          currentSlot <- slotOfCurrentBlock
          let ns = "epoch"
          gyLogInfo' ns $ "Current slot: " <> show currentSlot
          GYEpochNo epochNo <- slotToEpoch currentSlot
          gyLogInfo' ns $ "Current epoch: " <> show epochNo
          nextEpochStartSlot <- epochToBeginSlot $ GYEpochNo $ epochNo + 1
          gyLogInfo' ns $ "Next epoch start slot: " <> show nextEpochStartSlot
          void $ protocolParams
          waitUntilSlot_ (slotFromWord64 $ slotToWord64 nextEpochStartSlot - 20)
          void $ protocolParams
          waitUntilSlot_ nextEpochStartSlot
          void $ protocolParams
    ]