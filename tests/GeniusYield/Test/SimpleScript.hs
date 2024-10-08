module GeniusYield.Test.SimpleScript (
  simpleScriptTests,
) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import GeniusYield.GYConfig
import GeniusYield.Test.Privnet.Asserts (assertEqual)
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

simpleScriptTests :: [GYCoreConfig] -> TestTree
simpleScriptTests configs =
  testGroup
    "simple-script"
    [ testCase "able to read simple-script from file" $ do
        ss <- readSimpleScript "tests/mock-data/simple-script.json"
        ss
          @?= RequireAllOf
            [ RequireTimeAfter (slotFromWord64 1000)
            , RequireSignature "966e394a544f242081e41d1965137b1bb412ac230d40ed5407821c37"
            , RequireMOf 2 [RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413", RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614", RequireSignature "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538"]
            , RequireAnyOf [RequireSignature "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321", RequireSignature "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8"]
            ]
    , testCase "able to read on-chain simple script" $ do
        forM_ configs $ \config -> withCfgProviders config mempty $ \provider -> do
          utxo <- fromJust <$> gyQueryUtxoAtTxOutRef provider "bce517a8353c663e9f19feb3aaf28e7d1ce8579c988a7e86ca60b5527b1bd4e4#0"
          let sscript = RequireAnyOf [RequireSignature "07fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb"]
          assertEqual "Simple script not equal" (Just $ GYSimpleScript sscript) (utxoRefScript utxo)
    ]
