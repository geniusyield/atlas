module GeniusYield.Test.Stake
  ( stakeTests
  ) where

import           Data.Foldable           (find, for_)
import           Data.Maybe              (isJust)
import           GeniusYield.GYConfig
import           GeniusYield.Transaction (GYCoinSelectionStrategy (GYLegacy, GYRandomImproveMultiAsset))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertBool, testCase)

stakeTests :: GYCoreConfig -> TestTree
stakeTests config =
  testGroup "stake"
    [ testCase "able to build balanced transaction involving withdrawal" $ do
        withCfgProviders config mempty $ \provider@GYProviders {..} -> do
          -- This stake credential and it's corresponding address was found from net, and in case is not valid anymore, it's easy to replace it with a valid one. This test was written as there was some trouble faced in accumulation of rewards in our private testnet.
          let addr = unsafeAddressFromText "addr_test1qqynu5d8p9yc7garta6z4g34e2tlzye5ty8uy6ljmffnpnjv7ncp3yppt0gcr50u60y43x32fgadhnl35u9hfqyql2pqepxt0y"
              stakeAddr = unsafeStakeAddressFromText "stake_test1upx0fuqcjqs4h5vp687d8j2cng4y5wkmelc6wzm5szq04qsm5d0l6"
          -- Check if there is a UTxO in the given addr, with value greater than 5 ada.
          utxos <- gyQueryUtxosAtAddress provider addr Nothing
          assertBool "Not a single UTxO found at given address with value greater than 5 ada" $ isJust $ find (\utxo -> utxoValue utxo `valueGreaterOrEqual` valueFromLovelace 5_000_000) (utxosToList utxos)
          -- Check if the withdrawal amount is positive.
          stakeAddrInfo <- gyGetStakeAddressInfo stakeAddr
          assertBool "No positive rewards available for withdrawal" $ gyStakeAddressInfoAvailableRewards stakeAddrInfo > 0
          for_ [minBound .. maxBound] $ \strat ->
            testWithdrawalWithStrategy strat stakeAddrInfo addr stakeAddr config provider
    ]

testWithdrawalWithStrategy :: GYCoinSelectionStrategy -> GYStakeAddressInfo -> GYAddress -> GYStakeAddress -> GYCoreConfig -> GYProviders -> IO ()
testWithdrawalWithStrategy strat GYStakeAddressInfo {..} addr stakeAddr config provider = do
  txBody <- runGYTxMonadNodeWithStrategy strat (cfgNetworkId config) provider [addr] addr Nothing $ pure $ mustHaveWithdrawal (GYTxWdrl stakeAddr gyStakeAddressInfoAvailableRewards GYTxWdrlWitnessKey)
  -- Check if tx is balanced (sum inputs + withdrawal == sum outputs + tx fees).
  let inputRefs = txBodyTxIns txBody
      outputUtxos = txBodyUTxOs txBody
  inputUtxos <- gyQueryUtxosAtTxOutRefs provider inputRefs
  let inputVal = foldMapUTxOs utxoValue inputUtxos
      outputVal = foldMapUTxOs utxoValue outputUtxos
      txFees = txBodyFee txBody
  assertBool "Transaction is not balanced" $ inputVal <> valueFromLovelace (fromIntegral gyStakeAddressInfoAvailableRewards) == outputVal <> valueFromLovelace txFees
