module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api           as Api
import           Control.Exception     (handle)
import           Data.List             (isInfixOf)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as Set (fromList)
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Providers (SubmitTxException)
import           GeniusYield.Types
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase)

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
          let outputRefs =
                [ "8aba7590148083c96e1ed742defecb6123126bbdb392cef3facb8d968825a983#1"  -- Contains reference script.
                , "0c72765df71ff3739db11c0165bc71c0f3b0a160acec6f4f1448e523064e927e#0"  -- Contains datum hash.
                , "4e2341767958f1fd83f2ec536e1001888db938d374fcae1a1e965dc21a05d0c6#0"  -- Contains inline datum.
                ]
          utxosAtRefs <- gyQueryUtxosAtTxOutRefs provider outputRefs
          pure (utxosAtAddresses, Set.fromList utxosAtAddressesWithDatums, utxosAtRefs)
        assertBool "Utxos are not all equal" $ all (== head utxosProviders) (tail utxosProviders)
    , testCase "Checking presence of error message when submitting an invalid transaction" $ do
        let
            handler :: SubmitTxException -> IO GYTxId
            handler e =
              let errorText = show e
              in ( if "BadInputsUTxO" `isInfixOf` errorText then
                     pure "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"  -- Any transaction ID.
                   else error "Not satisfied"
                 )
        forM_ configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> handle handler $ gySubmitTx . fromRight (error "absurd") $ txFromHexBS "84a400818258208ddcb4a93b82dc42608949cfe366803787699ac81697e8c42f801d968f07acfb010183a300581d70c652c19ea10ab025a2b0880682f96a2794d7cea9bc4782645c0e114c01821a0012593aa1581c432f429b1369039d55cfd0441267ca8a9ec4d696215ac0da009b74eca144744352551a000dbba002820058203bcdfaeb38f66b31354c06cf45b344913cf3b88f5d2d3ee31c1aa7ecbff9da2da20058390025195af85c41b9d97da7f4f215d3e74c9cef7f04739d6ba473ba72a2358a4e4105c08f59e4779070699c7a72566893332f9857db4e742beb01821b0000000a3c96d5dba6581c432f429b1369039d55cfd0441267ca8a9ec4d696215ac0da009b74eca144744352551b0001975cd8067d40581c828c6c5c9df5d8493ba505a8db216f8a84d8f9f27ae62c78bba1852ca145744e4d4b521b002356be2deaa3c0581c84be37380867957f86042ab6e2d5e862c94d31af1f4438a437f61950a144744e54581b00038d7e7ead1500581cc6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87ea1457447454e531b000032856d18223a581ce85deedfffaf2299cb4cbf2645b5de1c7c7e74dba0cda56bdcd40014a146447261676f6e0a581cf99a6dacc5592e35a0def73fc918c8468c1d33712a998a4e4f35833da14474454d501b0000b41477aa6000a20058390025195af85c41b9d97da7f4f215d3e74c9cef7f04739d6ba473ba72a2358a4e4105c08f59e4779070699c7a72566893332f9857db4e742beb011a001b9423021a0002f05d0b5820372bac4dbd2d7520fa2cc98822b9ead1b3b82e51e3f85a3f05e5422783390e1ca10481d8799f581c25195af85c41b9d97da7f4f215d3e74c9cef7f04739d6ba473ba72a2d8799fd8799f581c25195af85c41b9d97da7f4f215d3e74c9cef7f04739d6ba473ba72a2ffd8799fd8799fd8799f581c358a4e4105c08f59e4779070699c7a72566893332f9857db4e742bebffffffffd8799f1b000001887bb57477fffff5f6"
    ]

