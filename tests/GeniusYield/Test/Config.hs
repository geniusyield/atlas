module GeniusYield.Test.Config (
    configTests
) where

import           System.FilePath

import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertBool, testCase)

import           GeniusYield.GYConfig

-- | These tests check that we can parse configs
configTests :: TestTree
configTests = testGroup "Config"
    [ testCase "core-local"      $ testParseResult isNodeKupo    "core-local.json"
    , testCase "core-maestro"    $ testParseResult isMaestro     "core-maestro.json"
    , testCase "core-blockfrost" $ testParseResult isBlockfrost  "core-blockfrost.json"
    ]

testParseResult :: (GYCoreProviderInfo -> Bool) -> FilePath -> IO ()
testParseResult expectation filePath =
    coreProviderIO (mockConfigDir </> filePath) >>= assertBool "parses as expected" . expectation

mockConfigDir :: FilePath
mockConfigDir = "tests/mock-configs"
