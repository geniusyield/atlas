module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api           as Api
import           Control.Concurrent    (threadDelay)
import           Control.Exception     (handle)
import           Data.List             (isInfixOf)
import           Data.Maybe            (fromJust)
import qualified Data.Set              as Set (fromList)
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Providers (SubmitTxException)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase)

providersMashupTests :: [GYCoreConfig] -> TestTree
providersMashupTests configs =
  testGroup "Providers Mashup"
    [ testCase "Datum lookup - GYLookupDatum" $ do
        threadDelay 1_000_000
        dats <- forM configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> fromJust <$> gyLookupDatum "d81d2abd12bdc2c19001a5d659c6aefd3fe4e073a37835b6818c2e676f84c03c"
        assertBool "Datums are not all equal" $ all (== head dats) (tail dats)
    , testCase "Parameters" $ do
        paramsList <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
           threadDelay 1_000_000
           protocolParams <- gyGetProtocolParameters provider
           threadDelay 1_000_000
           systemStart <- gyGetSystemStart provider
           threadDelay 1_000_000
           Api.EraHistory mode interpreter <- gyGetEraHistory provider  -- `mode` here doesn't appear to have `Eq` instance, comparing via it's `Show` instance should be fine.
           threadDelay 1_000_000
           stakePools <- gyGetStakePools provider
           threadDelay 1_000_000
           slotConfig' <- gyGetSlotConfig provider
           pure (protocolParams, systemStart, (show mode, interpreter), stakePools, slotConfig')
        assertBool "Parameters are not all equal" $ all (== head paramsList) (tail paramsList)
    , testCase "Query UTxOs" $ do

        utxosProviders <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
          let addressWithDatumHashes = "addr_test1wz09gtk5qn8g2lr0qu8hdh9gyjcl396778rz2qphgz4edxs245ja0"  -- This address has lots of UTxOs with datum hashes.
              myAddrList = unsafeAddressFromText <$>
                -- TODO: Put more reliable (in sense that UTxOs won't change) addresses here!
                [ addressWithDatumHashes
                , "addr_test1wpdz7qwyrpsxrwqe0e4yv3knfmy068euhh4k07ac4wp3kfgjhpd7w"  -- This address has UTxOs with inline datums.
                , "addr_test1qr5zypvu3va5y3q2m8envvd08sj5mams3znp3nh8q6arx4vre0cyeg6lqagujyhvr4ylx5wlgwjs3uyl8z0spz4akxzq6wyfzk"  -- This address has UTxOs with reference scripts.
                ]
          threadDelay 1_000_000
          utxosAtAddresses' <- gyQueryUtxosAtAddresses provider myAddrList
          threadDelay 1_000_000
          utxosAtAddressesWithDatums' <- gyQueryUtxosAtAddressesWithDatums provider myAddrList
          let refWithDatumHash = "0c72765df71ff3739db11c0165bc71c0f3b0a160acec6f4f1448e523064e927e#0"
              outputRefs =
                [ "8aba7590148083c96e1ed742defecb6123126bbdb392cef3facb8d968825a983#1"  -- Contains reference script.
                ,  refWithDatumHash -- Contains datum hash.
                , "4e2341767958f1fd83f2ec536e1001888db938d374fcae1a1e965dc21a05d0c6#0"  -- Contains inline datum.
                ]
          threadDelay 1_000_000
          utxosAtRefs <- gyQueryUtxosAtTxOutRefs provider outputRefs
          threadDelay 1_000_000
          utxoRefsAtAddress' <- gyQueryUtxoRefsAtAddress provider $ unsafeAddressFromText addressWithDatumHashes
          threadDelay 1_000_000
          utxosAtRefsWithDatums' <- gyQueryUtxosAtTxOutRefsWithDatums provider outputRefs
          threadDelay 1_000_000
          utxoAtRefWithDatum' <- runGYTxQueryMonadNode (cfgNetworkId config) provider $ utxoAtTxOutRefWithDatum refWithDatumHash
          pure (utxosAtAddresses', Set.fromList utxosAtAddressesWithDatums', utxosAtRefs, Set.fromList utxoRefsAtAddress', Set.fromList utxosAtRefsWithDatums', utxoAtRefWithDatum')
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
        forM_ configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> do
          threadDelay 1_000_000
          handle handler $ gySubmitTx . fromRight (error "absurd") $ txFromHexBS "84a30083825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d00825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d01825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d020183a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a3b3b81d5a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a002dc6c0a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a001bc75b021a0002bd25a10081825820b8ee2dc03ba6f88baa7e4430675df3e559d63a1282f7763de71227041e351023584020313e571def2f09145ae6b0eb26e99260d14885789929a354fea4a585d5f053fc2eae86d36f484269b0d95a25abb7acc3b15033565d00afd83af83f24d9be0ef5f6"
    , testCase "Submitting a valid transaction" $ do
        skey <- readPaymentSigningKey "preprod-submit-test-wallet.skey"
        let nid = GYTestnetPreprod
            senderAddress = addressFromPubKeyHash GYTestnetPreprod $ pubKeyHash $ paymentVerificationKey skey
        let totalConfigs = length configs
        forM_ (zip [1 ..] configs) $ \(i, config) -> withCfgProviders config mempty $ \provider@GYProviders {..} -> do
          threadDelay 1_000_000
          senderUTxOs <- runGYTxQueryMonadNode nid provider $ utxosAtAddress senderAddress
          threadDelay 1_000_000
          let totalSenderFunds = foldMapUTxOs utxoValue senderUTxOs
              valueToSend     = totalSenderFunds `valueMinus` valueFromLovelace 5_000_000
              -- This way, UTxO distribution in test wallet should remain same.
          txBody <- runGYTxMonadNode nid provider [senderAddress] senderAddress Nothing $ pure $ mustHaveOutput $ mkGYTxOutNoDatum @'PlutusV2 senderAddress valueToSend
          threadDelay 1_000_000
          let signedTxBody = signGYTxBody txBody [skey]
          printf "Signed tx: %s\n" (txToHex signedTxBody)
          tid    <- gySubmitTx signedTxBody
          printf "Submitted tx: %s\n" tid
          unless (i == totalConfigs) $ threadDelay $ 120 * 1_000_000  -- Wait for 2 minutes so that transaction is seen onchain.
    ]

