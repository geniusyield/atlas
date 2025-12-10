module GeniusYield.Test.Hydra (
  hydraTests,
) where

import GeniusYield.GYConfig
import GeniusYield.Imports ((&))
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

aliceKey :: GYSigningKey 'GYKeyRolePayment
aliceKey = "5f9b911a636479ed83ba601ccfcba0ab9a558269dc19fdea910d27e5cdbb5fc8"

aliceVKey :: GYVerificationKey GYKeyRolePayment
aliceVKey = getVerificationKey aliceKey

aliceVKeyHash :: GYKeyHash GYKeyRolePayment
aliceVKeyHash = verificationKeyHash aliceVKey

hydraTests :: GYCoreConfig -> TestTree
hydraTests config =
  testGroup
    "hydra"
    [ testCase "able to query, build and submit a hydra transaction" $ do
        withCfgProviders config mempty $ \provider -> do
          let nid = cfgNetworkId config
              aliceAddress = addressFromPaymentKeyHash nid aliceVKeyHash
          txBody <-
            runGYTxBuilderMonadIO nid provider [aliceAddress] aliceAddress Nothing $ do
              aliceUtxos <- utxosAtAddress aliceAddress Nothing
              gyLogInfo' "" (show aliceUtxos)
              let aliceUtxo = utxosToList aliceUtxos & head
              let skel = mustHaveInput (GYTxIn (utxoRef aliceUtxo) GYTxInWitnessKey)
              buildTxBody skel
          print txBody
          txId <- runGYTxMonadIO nid provider (AGYPaymentSigningKey aliceKey) Nothing [aliceAddress] aliceAddress Nothing $ do
            signedTx <- signTxBody txBody
            submitTx signedTx
          print txId
    ]
