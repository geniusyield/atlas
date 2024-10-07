module GeniusYield.Test.Privnet.SimpleScripts (
  simpleScriptsTests,
) where

import Control.Lens (each, (%~), (&))
import Control.Monad (when)
import GeniusYield.Test.Privnet.Asserts (assertEqual)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder (GYTxQueryMonad (utxoAtTxOutRef))
import GeniusYield.Types
import GeniusYield.Types.Script (GYAnyScript (GYSimpleScript))
import GeniusYield.Types.UTxO (GYUTxO (utxoRefScript))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

simpleScriptsTests :: Setup -> TestTree
simpleScriptsTests setup =
  testGroup
    "simple-scripts"
    [ testCaseSteps "exercising a multi-sig simple script without giving it as reference" $ \info -> withSetup info setup $ \ctx -> do
        exerciseASimpleScript ctx info False
    , testCaseSteps "exercising a multi-sig simple script when given as a reference" $ \info -> withSetup info setup $ \ctx -> do
        exerciseASimpleScript ctx info True
    ]

exerciseASimpleScript :: Ctx -> (String -> IO ()) -> Bool -> IO ()
exerciseASimpleScript ctx info toUseRefScript = do
  let user1 = ctxUser2 ctx
      user2 = ctxUser3 ctx
      user3 = ctxUser4 ctx
      (pkh1, pkh2, pkh3) = (user1, user2, user3) & each %~ userPaymentPkh
      multiSigSimpleScript = RequireAllOf [RequireSignature pkh1, RequireSignature pkh2, RequireSignature pkh3]
      multiSigSimpleScriptAddr = addressFromSimpleScript (ctxNetworkId ctx) multiSigSimpleScript
      fundUser = ctxUserF ctx
  info "Sending funds to simple script"
  when toUseRefScript $ do
    info "Also attaching script to fund UTxO"
  txIdFund <- ctxRun ctx fundUser $ do
    txBodyFund <- buildTxBody $ mustHaveOutput (GYTxOut {gyTxOutValue = valueFromLovelace 100_000_000, gyTxOutRefS = if toUseRefScript then Just (GYSimpleScript multiSigSimpleScript) else Nothing, gyTxOutDatum = Nothing, gyTxOutAddress = multiSigSimpleScriptAddr})
    signAndSubmitConfirmed txBodyFund
  info $ "Successfully funded the simple script, with tx id: " <> show txIdFund
  info "Now consuming from the simple script"
  let toConsume = txOutRefFromTuple (txIdFund, 0)
  when toUseRefScript $ do
    toConsumeUtxo <- ctxRun ctx fundUser $ utxoAtTxOutRef' toConsume
    assertEqual "Reference script must be equal to actual script" (Just $ GYSimpleScript multiSigSimpleScript) (utxoRefScript toConsumeUtxo)
  txIdConsume <- ctxRun ctx fundUser $ do
    txBodyConsume <- buildTxBody $ mustHaveInput @'PlutusV2 $ GYTxIn toConsume (GYTxInWitnessSimpleScript $ if toUseRefScript then GYInReferenceSimpleScript toConsume multiSigSimpleScript else GYInSimpleScript multiSigSimpleScript)
    submitTxBodyConfirmed txBodyConsume $ userPaymentSKey <$> [user1, user2, user3]
  info $ "Successfully consumed the simple script, with tx id: " <> show txIdConsume
