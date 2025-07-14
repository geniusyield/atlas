module GeniusYield.Test.Hydra (
  hydraTests,
) where

import Data.Foldable (for_)
import GeniusYield.GYConfig
import GeniusYield.Imports ((&))
import GeniusYield.Transaction (GYCoinSelectionStrategy)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

aliceKey :: GYSigningKey 'GYKeyRolePayment
aliceKey = "e4c36e5403e6a02ef4821a34bb71d504916df0ddea476f797a5639110bc1bd52"

aliceVKey = getVerificationKey aliceKey

aliceVKeyHash = verificationKeyHash aliceVKey

hydraTests :: GYCoreConfig -> TestTree
hydraTests config =
  testGroup
    "hydra"
    [ testCase "able to query, build and submit a hydra transaction" $ do
        withCfgProviders config mempty $ \provider@GYProviders {..} -> do
          let nid = cfgNetworkId config
              aliceAddress = addressFromPaymentKeyHash nid aliceVKeyHash
              aliceActualAddress = unsafeAddressFromText "addr_test1vqx5tu4nzz5cuanvac4t9an4djghrx7hkdvjnnhstqm9kegvm6g6c" -- Not really sure if this is of alice, could be of bob or carol.
          print aliceKey
          print aliceVKey
          print aliceVKeyHash
          print aliceAddress
          utxos <- runGYTxQueryMonadIO nid provider $ do
            utxosAtPaymentCredential (GYCredentialByKey aliceVKeyHash) Nothing
          print utxos
          txBody <-
            runGYTxBuilderMonadIO nid provider [aliceActualAddress] aliceActualAddress Nothing $ do
              aliceUtxos <- utxosAtAddress aliceActualAddress Nothing
              gyLogInfo' "" (show aliceUtxos)
              let aliceUtxo = utxosToList aliceUtxos & head
              let skel = mustHaveInput (GYTxIn (utxoRef aliceUtxo) GYTxInWitnessKey)
              buildTxBody skel
          print txBody
    ]
