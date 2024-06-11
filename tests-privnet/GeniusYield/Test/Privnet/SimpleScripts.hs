module GeniusYield.Test.Privnet.SimpleScripts (
  simpleScriptsTests,
) where

import           Control.Lens                   (each, (%~), (&))
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (testCaseSteps)

simpleScriptsTests :: IO Setup -> TestTree
simpleScriptsTests setup = testGroup "simple-scripts"
  [ testCaseSteps "exercising a multi-sig simple script" $ \info -> withSetup setup info $ \ctx -> do
    let user1 = ctxUser2 ctx
        user2 = ctxUser3 ctx
        user3 = ctxUser4 ctx
        (pkh1, pkh2, pkh3) = (user1, user2, user3) & each %~ userPaymentPkh
        multiSigSimpleScript = RequireAllOf [RequireSignature pkh1, RequireSignature pkh2, RequireSignature pkh3]
        multiSigSimpleScriptAddr = addressFromSimpleScript GYPrivnet multiSigSimpleScript
        fundUser = ctxUserF ctx
    info "Sending funds to simple script"
    txBodyFund <- ctxRunI ctx fundUser $
      return $ mustHaveOutput (mkGYTxOutNoDatum multiSigSimpleScriptAddr $ valueFromLovelace 100_000_000)
    txIdFund <- submitTx ctx fundUser txBodyFund
    info $ "Successfully funded the simple script, with tx id: " <> show txIdFund
    let toConsume = txOutRefFromTuple (txIdFund, 0)
    txBodyConsume <- ctxRunI ctx fundUser $
      return $ mustHaveInput $ GYTxIn toConsume (GYTxInWitnessSimpleScript $ GYInSimpleScript multiSigSimpleScript)
    let txConsume = signGYTxBody txBodyConsume (map userPaymentSKey [user1, user2, user3, fundUser])
    txIdConsume <- submitTx' ctx txConsume
    info $ "Successfully consumed the simple script, with tx id: " <> show txIdConsume
  ]
