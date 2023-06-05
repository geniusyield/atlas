module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api          as Api
import           Data.Maybe           (fromJust)
import qualified Data.Set             as Set (fromList)
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
    , testCase "Query UTxOs" $ do

        utxosProviders <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
          let myAddrList = unsafeAddressFromText <$>
                -- TODO: Put more reliable (in sense that UTxOs won't change) addresses here!
                [ "addr_test1wz09gtk5qn8g2lr0qu8hdh9gyjcl396778rz2qphgz4edxs245ja0"  -- This address has UTxOs with datum hashes.
                , "addr_test1wpdz7qwyrpsxrwqe0e4yv3knfmy068euhh4k07ac4wp3kfgjhpd7w"  -- This address has UTxOs with inline datums.
                , "addr_test1qr5zypvu3va5y3q2m8envvd08sj5mams3znp3nh8q6arx4vre0cyeg6lqagujyhvr4ylx5wlgwjs3uyl8z0spz4akxzq6wyfzk"  -- This address has UTxOs with reference scripts.
                ]
          utxosAtAddresses <- gyQueryUtxosAtAddresses provider myAddrList
          utxosAtAddressesWithDatums <- gyQueryUtxosAtAddressesWithDatums provider myAddrList
          pure (utxosAtAddresses, Set.fromList utxosAtAddressesWithDatums)
        assertBool "Utxos are not all equal" $ all (== head utxosProviders) (tail utxosProviders)
    ]

