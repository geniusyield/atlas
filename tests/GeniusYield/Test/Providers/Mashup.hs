module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api          as Api
import           Data.Maybe           (fromJust)
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Types
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertBool, testCase)

providersMashupTests :: [GYCoreConfig] -> TestTree
providersMashupTests configs =
  testGroup "Providers Mashup"
    [ testCase "Datum lookup - GYLookupDatum" $ do
        dats <- forM configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> fromJust <$> gyLookupDatum "d81d2abd12bdc2c19001a5d659c6aefd3fe4e073a37835b6818c2e676f84c03c"
        assertBool "Datums are not all equal" $ all (== head dats) (tail dats)
    , testCase "Parameters" $ do
        paramsList <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
           protocolParams <- gyGetProtocolParameters provider
           systemStart <- gyGetSystemStart provider
           Api.EraHistory mode interpreter <- gyGetEraHistory provider  -- `mode` here doesn't appear to have `Eq` instance, comparing via it's `Show` instance should be fine.
           stakePools <- gyGetStakePools provider
           slotConfig <- gyGetSlotConfig provider
           pure (protocolParams, systemStart, (show mode, interpreter), stakePools, slotConfig)
        assertBool "Parameters are not all equal" $ all (== head paramsList) (tail paramsList)
    ]

