module GeniusYield.Test.Config (
    configTests,
    coreProviderIO
) where

import           System.FilePath

import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertBool, testCase)

import           GeniusYield.GYConfig

-- | These tests check that we can parse configs
configTests :: TestTree
configTests = testGroup "Config"
    [ testCase "core-local"      $ testParseResult isNodeChainIx "core-local.json"
    , testCase "core-dbsync"     $ testParseResult isDbSync      "core-dbsync.json"
    , testCase "core-maestro"    $ testParseResult isMaestro     "core-maestro.json"
    ]

testParseResult :: (GYCoreProviderInfo -> Bool) -> FilePath -> IO ()
testParseResult expectation filePath =
    coreProviderIO (mockConfigDir </> filePath) >>= assertBool "parses as expected" . expectation

mockConfigDir :: FilePath
mockConfigDir = "tests/mock-configs"
