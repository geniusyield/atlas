module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api                  as Api
import           Control.Concurrent           (threadDelay)
import           Control.Exception            (handle)
import           Data.Default                 (def)
import           Data.List                    (isInfixOf)
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set (difference, fromList)
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Providers.Common (SubmitTxException, datumFromCBOR)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (assertBool, assertFailure,
                                               testCase)

providersMashupTests :: [GYCoreConfig] -> TestTree
providersMashupTests configs =
  testGroup "Providers Mashup"
    [ testCase "Datum lookup - GYLookupDatum" $ do
        delayBySecond
        dats <- forM configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> fromJust <$> gyLookupDatum "a7ed3e81ef2e98a85c8d5649ed6344b7f7b36a31103ab18395ef4e80b8cac565"  -- A datum hash seen at always fail script's address.
        assertBool "Datums are not all equal" $ allEqual dats
    , testCase "Parameters" $ do
        paramsList <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
           delayBySecond
           pp <- gyGetProtocolParameters provider
           delayBySecond
           ss <- gyGetSystemStart provider
           delayBySecond
           Api.EraHistory interpreter <- gyGetEraHistory provider
           delayBySecond
           sp <- gyGetStakePools provider
           delayBySecond
           slotConfig' <- gyGetSlotConfig provider
           pure (pp, ss, interpreter, sp, slotConfig')
        assertBool "Parameters are not all equal" $ allEqual paramsList
    , testCase "Stake address info" $ do
        saInfos <- forM configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> do
          delayBySecond
          gyGetStakeAddressInfo $ unsafeStakeAddressFromText "stake_test1upx0fuqcjqs4h5vp687d8j2cng4y5wkmelc6wzm5szq04qsm5d0l6"
        assertBool "Stake address info are not all equal" $ allEqual saInfos
    , testCase "Query UTxOs" $ do
        let
            -- Blockfrost is unable to get the preimage of the involved datum hash, thus it's being deleted for in our set so that test still passes.
            utxoBug1 = (GYUTxO {utxoRef = "6d2174d3956d8eb2b3e1e198e817ccf1332a599d5d7320400bfd820490d706be#0", utxoAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt", utxoValue = valueFromList [(GYLovelace,50000000)], utxoOutDatum = GYOutDatumHash "15461aa490b224fe541f3568e5d7704e0d88460cde9f418f700e2b6864d8d3c9", utxoRefScript = Nothing},Just (either (error "absurd - Mashup: parsing datum failed") id $ datumFromCBOR "19077a"))
            utxoBug2 = (GYUTxO {utxoRef = "6d2174d3956d8eb2b3e1e198e817ccf1332a599d5d7320400bfd820490d706be#0", utxoAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt", utxoValue = valueFromList [(GYLovelace,50000000)], utxoOutDatum = GYOutDatumHash "15461aa490b224fe541f3568e5d7704e0d88460cde9f418f700e2b6864d8d3c9", utxoRefScript = Nothing}, Nothing)
            utxoBugSet = Set.fromList [utxoBug1, utxoBug2]
        utxosProviders <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
          let alwaysFailAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt"
              alwaysFailCredential = GYPaymentCredentialByScript "51936f3c98a04b6609aa9b5c832ba1182cf43a58e534fcc05db09d69"  -- Credential of always fail script address.
              ciWalletAddress = unsafeAddressFromText "addr_test1vqrlk2mckwgh60mtlga9nhnp70pztjls64ty589ud7tdd6ckynfpg"
              ciWalletCredential = GYPaymentCredentialByKey "07fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb"  -- Could ofc be derived from address.
              myAddrList = [alwaysFailAddress, ciWalletAddress]  -- always fail script's address. It has all the cases, reference scripts, inline datums, many UTxOs, etc. Besides it, CI wallet's address is also included.
              myCredList = [alwaysFailCredential, ciWalletCredential]
          delayBySecond
          utxosAtAddresses' <- gyQueryUtxosAtAddresses provider myAddrList
          delayBySecond
          utxosAtAddressesWithDatums' <- gyQueryUtxosAtAddressesWithDatums provider myAddrList
          -- All the below refs were taken from always fail address.
          let refWithDatumHash = "dc1c7958f94b7a458dffa224d18b5b8464f81f6360913c26eca4199f67ac6435#1"
              outputRefs =
                [ "930de0fd6718fc87cd99be663f1b1dd099cad6cec3dede49d82b3554a1e8eb86#0"  -- Contains reference script.
                ,  refWithDatumHash -- Contains datum hash.
                , "930de0fd6718fc87cd99be663f1b1dd099cad6cec3dede49d82b3554a1e8eb86#0"  -- Contains inline datum.
                ]
          delayBySecond
          utxosAtRefs <- gyQueryUtxosAtTxOutRefs provider outputRefs
          delayBySecond
          utxoRefsAtAddress' <- gyQueryUtxoRefsAtAddress provider alwaysFailAddress
          delayBySecond
          utxosAtRefsWithDatums' <- gyQueryUtxosAtTxOutRefsWithDatums provider outputRefs
          delayBySecond
          utxoAtRefWithDatum' <- runGYTxQueryMonadIO (cfgNetworkId config) provider $ utxoAtTxOutRefWithDatum refWithDatumHash
          delayBySecond
          utxosAtScriptCredential <- runGYTxQueryMonadIO (cfgNetworkId config) provider $ utxosAtPaymentCredential alwaysFailCredential Nothing
          delayBySecond
          utxosAtScriptCredentialWithAsset <- runGYTxQueryMonadIO (cfgNetworkId config) provider $ utxosAtPaymentCredential alwaysFailCredential (Just "6d24161a60592755dcbcc2c1330bbe968f913acc15ec40f0be3873ee.61757468")
          delayBySecond
          utxosAtKeyCredential <- runGYTxQueryMonadIO (cfgNetworkId config) provider $ utxosAtPaymentCredential ciWalletCredential Nothing
          delayBySecond
          utxosAtScriptCredentialWithDatums <- runGYTxQueryMonadIO (cfgNetworkId config) provider $ utxosAtPaymentCredentialWithDatums alwaysFailCredential Nothing
          delayBySecond
          utxosAtScriptAddressWithAsset <- gyQueryUtxosAtAddress provider alwaysFailAddress (Just "6d24161a60592755dcbcc2c1330bbe968f913acc15ec40f0be3873ee.61757468")  -- An asset I saw by random chance.
          -- TODO: Write variant of above for with datums.
          delayBySecond
          utxosAtPaymentCredentials' <- gyQueryUtxosAtPaymentCredentials provider myCredList
          delayBySecond
          utxosAtPaymentCredentialsWithDatums' <- gyQueryUtxosAtPaymentCredsWithDatums provider myCredList
          -- Following is commented out due to an apparent bug in Blockfrost.
          -- delayBySecond
          -- utxosAtScriptAddressWithAssetAndDatums <- gyQueryUtxosAtAddressWithDatums provider (unsafeAddressFromText "addr_test1wz2mzj532enpgu5vgwxuh249fpknx5ft9wxse2876z0mp2q89ye7k") (Just "c6e65ba7878b2f8ea0ad39287d3e2fd256dc5c4160fc19bdf4c4d87e.7447454e53")
          pure (utxosAtAddresses', Set.fromList utxosAtAddressesWithDatums' `Set.difference` utxoBugSet, utxosAtRefs, Set.fromList utxoRefsAtAddress', Set.fromList utxosAtRefsWithDatums', utxoAtRefWithDatum', utxosAtScriptCredential <> utxosAtKeyCredential, Set.fromList utxosAtScriptCredentialWithDatums `Set.difference` utxoBugSet, utxosAtScriptAddressWithAsset, utxosAtScriptCredentialWithAsset, utxosAtPaymentCredentials', Set.fromList utxosAtPaymentCredentialsWithDatums' `Set.difference` utxoBugSet
             -- , Set.fromList utxosAtScriptAddressWithAssetAndDatums
               )
        assertBool "Utxos are not all equal" $ allEqual utxosProviders
    , testCase "Checking presence of error message when submitting an invalid transaction" $ do
        let
            handler :: SubmitTxException -> IO GYTxId
            handler e =
              let errorText = show e
              in ( if "BadInputsUTxO" `isInfixOf` errorText then
                     pure "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"  -- Any transaction ID.
                   else error $ "Not satisfied, error text: " <> errorText
                 )
        forM_ configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> do
          delayBySecond
          handle handler $ gySubmitTx . fromRight (error "absurd") $ txFromHexBS "84a30083825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d00825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d01825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d020183a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a3b3b81d5a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a002dc6c0a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a001bc75b021a0002bd25a10081825820b8ee2dc03ba6f88baa7e4430675df3e559d63a1282f7763de71227041e351023584020313e571def2f09145ae6b0eb26e99260d14885789929a354fea4a585d5f053fc2eae86d36f484269b0d95a25abb7acc3b15033565d00afd83af83f24d9be0ef5f6"
    , testCase "Submitting a valid transaction" $ do
        skey <- readPaymentSigningKey "preprod-submit-test-wallet.skey"
        let nid = GYTestnetPreprod
            senderAddress = addressFromPaymentKeyHash GYTestnetPreprod $ paymentKeyHash $ paymentVerificationKey skey
        forM_ configs $ \config -> withCfgProviders config mempty $ \provider@GYProviders {..} -> do
          delayBySecond
          senderUTxOs <- runGYTxQueryMonadIO nid provider $ utxosAtAddress senderAddress Nothing
          delayBySecond
          let totalSenderFunds = foldMapUTxOs utxoValue senderUTxOs
              valueToSend     = totalSenderFunds `valueMinus` valueFromLovelace 5_000_000
              -- This way, UTxO distribution in test wallet should remain same.
          txBody <- runGYTxBuilderMonadIO nid provider [senderAddress] senderAddress Nothing $ buildTxBody $ mustHaveOutput $ mkGYTxOutNoDatum @'PlutusV2 senderAddress valueToSend
          delayBySecond
          let signedTxBody = signGYTxBody txBody [skey]
          printf "Signed tx: %s\n" (txToHex signedTxBody)
          tid    <- gySubmitTx signedTxBody
          printf "Submitted tx: %s\n" tid
          gyAwaitTxConfirmed (GYAwaitTxParameters {maxAttempts = 20, confirmations = 1, checkInterval = 10_000_000}) tid
    , testCase "Await Tx Confirmed - Submitted Tx" $
        forM_ configs $ \config -> withCfgProviders config mempty $
            \GYProviders {..} -> gyAwaitTxConfirmed def "c67b57d63e846c6dc17f0c2647893d5f7376690cde62b8b392ecfcb75a4697e2"  -- A transaction id which generated UTxO at the always fail address. Thus it is guaranteed to be always there (unless of course network is respun).
    , testCase "Await Tx Confirmed - Timeout for non-existing Tx" $ do
        let handleAwaitTxException (GYAwaitTxException _) = return ()
        forM_ configs $ \config -> withCfgProviders config mempty $
          \GYProviders {..} -> handle handleAwaitTxException $ do
              gyAwaitTxConfirmed def{maxAttempts = 2, checkInterval = 1_000_000} "9b50152cc5cfca6a842f32b1e886a3ffdc1a1704fa87a15a88837996b6a9df36"  -- <-- A non-existing transaction id.
              assertFailure "Exepected GYAwaitTxException to be raised"
    ]
  where
    delayBySecond = threadDelay 1_000_000

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs
