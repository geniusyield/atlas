module GeniusYield.Test.Config (
    configTests,
) where

import           Control.Exception    (throwIO)
import qualified Data.ByteString.Lazy as LBS
import           System.FilePath

import qualified Data.Aeson           as Aeson

import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertBool, testCase)

import           GeniusYield.GYConfig

-- | These tests check that we can parse configs
configTests :: TestTree
configTests = testGroup "Config"
    [ testCase "core-local"      $ testParseResult isNodeChainIx "core-local.json"
    , testCase "core-dbsync"     $ testParseResult isDbSync      "core-dbsync.json"
    , testCase "core-maestro"    $ testParseResult isMaestro     "core-maestro.json"
    , testCase "core-blockfrost" $ testParseResult isBlockfrost  "core-blockfrost.json"
    ]

testParseResult :: (GYCoreProviderInfo -> Bool) -> FilePath -> IO ()
testParseResult expectation filePath =
    coreProviderIO (mockConfigDir </> filePath) >>= assertBool "parses as expected" . expectation

coreProviderIO :: FilePath -> IO GYCoreProviderInfo
coreProviderIO filePath = do
  bs <- LBS.readFile filePath
  case Aeson.eitherDecode' bs of
    Left err  -> throwIO $ userError err
    Right cfg -> pure cfg

isNodeChainIx :: GYCoreProviderInfo -> Bool
isNodeChainIx GYNodeChainIx{} = True
isNodeChainIx _               = False

isDbSync :: GYCoreProviderInfo -> Bool
isDbSync GYDbSync{} = True
isDbSync _          = False

isMaestro :: GYCoreProviderInfo -> Bool
isMaestro GYMaestro{} = True
isMaestro _           = False

isBlockfrost :: GYCoreProviderInfo -> Bool
isBlockfrost GYBlockfrost{} = True
isBlockfrost _              = False

mockConfigDir :: FilePath
mockConfigDir = "tests/mock-configs"
