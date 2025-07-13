module GeniusYield.Test.Hydra (
  hydraTests,
) where

import Data.Foldable (for_)
import GeniusYield.GYConfig
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
          print $ aliceKey
          print $ aliceVKey
          print $ aliceVKeyHash
          print $ aliceAddress
          utxos <- runGYTxQueryMonadIO nid provider $ do
            utxosAtPaymentCredential (GYCredentialByKey aliceVKeyHash) Nothing
          print utxos
          utxos2 <-
            runGYTxQueryMonadIO nid provider $ do
              utxoAtTxOutRef $ txOutRefFromTuple ("f0a39560ea80ccc68e8dffb6a4a077c8927811f06c5d9058d0fa2d1a8d047d20", 0)
          print utxos2
    ]
