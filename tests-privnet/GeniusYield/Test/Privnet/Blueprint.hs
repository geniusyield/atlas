{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- Note: See [this](https://stackoverflow.com/a/69678961/20330802) answer on where one can find dumped splice file. As an example, @dist-newstyle/build/aarch64-osx/ghc-9.6.5/atlas-cardano-0.6.0/t/atlas-privnet-tests/build/atlas-privnet-tests/atlas-privnet-tests-tmp/tests-privnet/GeniusYield/Test/Privnet@.
module GeniusYield.Test.Privnet.Blueprint (
  blueprintTests,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import GeniusYield.Test.Privnet.Asserts (assertThrown, isTxBodyErrorAutoBalance)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

$(makeBPTypes "tests/aiken/bar/plutus.json")

$(uponBPTypes "tests/aiken/bar/plutus.json")

blueprintTests :: Setup -> TestTree
blueprintTests setup =
  testGroup
    "blueprint"
    [ testCaseSteps "e2e blueprint validator test" $ \info -> withSetup info setup $ \ctx -> do
        let user = ctxUserF ctx
            val = scriptFromBPSerialisedScript $ applyParamsToBPValidator_baz_baz_spend BPBool1True (BPbaz_ParamConstr0ParamConstr 23 (g "ff")) 10 (g "ddee")
            dat = datumFromPlutusData $ BPbaz_MyDatum0DatumA 10 (g "aabbccdd")
        oref <- ctxRun ctx user $ do
          valAddr <- scriptAddress val
          txBody <- buildTxBody $ mustHaveOutput @'PlutusV3 (GYTxOut {gyTxOutValue = valueFromLovelace 10_000_000, gyTxOutRefS = Nothing, gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum), gyTxOutAddress = valAddr})
          tid <- signAndSubmitConfirmed txBody
          pure $ txOutRefFromTuple (tid, 0)
        let satRedeemer = redeemerFromPlutusData $ BPbaz_MyRedeemer0MyRedeemer 5 (g "aabbccddee")
            unsatRedeemer = redeemerFromPlutusData $ BPbaz_MyRedeemer0MyRedeemer 6 (g "aabbccddee")
        assertThrown isTxBodyErrorAutoBalance $ ctxRun ctx user $ do
          buildTxBody $ mustHaveInput @'PlutusV3 $ GYTxIn oref (GYTxInWitnessScript (GYInScript val) dat unsatRedeemer)
        info "Successfully failed to consume from blueprint script for unsatisfying redeemer"
        tid <- ctxRun ctx user $ do
          txBody <- buildTxBody $ mustHaveInput @'PlutusV3 $ GYTxIn oref (GYTxInWitnessScript (GYInScript val) dat satRedeemer)
          signAndSubmitConfirmed txBody
        info $ "Successfully consumed from blueprint script, with tx id: " <> show tid
    ]
 where
  g :: ByteString -> PlutusTx.BuiltinByteString
  g = PlutusTx.toBuiltin . BS16.decodeLenient
