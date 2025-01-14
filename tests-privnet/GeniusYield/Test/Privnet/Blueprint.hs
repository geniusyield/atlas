{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- Note: See [this](https://stackoverflow.com/a/69678961/20330802) answer on where one can find dumped splice file. As an example, @dist-newstyle/build/aarch64-osx/ghc-9.6.5/atlas-cardano-0.6.0/t/atlas-privnet-tests/build/atlas-privnet-tests/atlas-privnet-tests-tmp/tests-privnet/GeniusYield/Test/Privnet@.
module GeniusYield.Test.Privnet.Blueprint (
  blueprintTests,
) where

import Control.Monad (void)
import Control.Monad.Except (handleError)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import GeniusYield.Test.Privnet.Asserts (isTxBodyErrorAutoBalance)
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Utils (TestInfo (testWallets), Wallets (..))
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)

$(makeBPTypes "tests/aiken/bar/plutus.json")

$(uponBPTypes "tests/aiken/bar/plutus.json")

blueprintTests :: Setup -> TestTree
blueprintTests setup =
  testGroup
    "blueprint"
    [ mkPrivnetTestFor' "e2e blueprint validator test" GYInfo setup $ \(testWallets -> Wallets {w1 = user}) -> do
        let val = scriptFromBPSerialisedScript $ applyParamsToBPValidator_baz_baz_spend BPBool1True (BPbaz_ParamConstr0ParamConstr 23 (g "ff")) 10 (g "ddee")
            dat = datumFromPlutusData $ BPbaz_MyDatum0DatumA 10 (g "aabbccdd")
        oref <- asUser user $ do
          valAddr <- scriptAddress val
          txBody <- buildTxBody $ mustHaveOutput @'PlutusV3 (GYTxOut {gyTxOutValue = valueFromLovelace 10_000_000, gyTxOutRefS = Nothing, gyTxOutDatum = Just (dat, GYTxOutUseInlineDatum), gyTxOutAddress = valAddr})
          tid <- signAndSubmitConfirmed txBody
          pure $ txOutRefFromTuple (tid, 0)
        let satRedeemer = redeemerFromPlutusData $ BPbaz_MyRedeemer0MyRedeemer 5 (g "aabbccddee")
            unsatRedeemer = redeemerFromPlutusData $ BPbaz_MyRedeemer0MyRedeemer 6 (g "aabbccddee")
        handleError
          (\e -> if isTxBodyErrorAutoBalance e then pure () else throwError e)
          $ asUser user
          $ do
            void $ buildTxBody $ mustHaveInput @'PlutusV3 $ GYTxIn oref (GYTxInWitnessScript (GYBuildPlutusScriptInlined val) dat unsatRedeemer)
        lg "Successfully failed to consume from blueprint script for unsatisfying redeemer"
        tid <- asUser user $ do
          txBody <- buildTxBody $ mustHaveInput @'PlutusV3 $ GYTxIn oref (GYTxInWitnessScript (GYBuildPlutusScriptInlined val) dat satRedeemer)
          signAndSubmitConfirmed txBody
        lg $ "Successfully consumed from blueprint script, with tx id: " <> show tid
    ]
 where
  g :: ByteString -> PlutusTx.BuiltinByteString
  g = PlutusTx.toBuiltin . BS16.decodeLenient
  lg = gyLogInfo' "blueprint"
