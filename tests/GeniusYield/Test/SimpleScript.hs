module GeniusYield.Test.SimpleScript (
  simpleScriptTests,
) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import GeniusYield.GYConfig
import GeniusYield.Test.Privnet.Asserts (assertEqual)
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

simpleScriptTests :: [GYCoreConfig] -> TestTree
simpleScriptTests configs =
  testGroup
    "simple-script"
    [ testCase "able to read on-chain simple script" $ do
        forM_ configs $ \config -> withCfgProviders config mempty $ \provider -> do
          utxo <- fromJust <$> gyQueryUtxoAtTxOutRef provider "bce517a8353c663e9f19feb3aaf28e7d1ce8579c988a7e86ca60b5527b1bd4e4#0"
          let sscript = RequireAnyOf [RequireSignature "07fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb"]
          assertEqual "Simple script not equal" (Just $ GYSimpleScript sscript) (utxoRefScript utxo)
    ]
