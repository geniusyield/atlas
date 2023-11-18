module BetRef.Tests.TakePot
    ( takeBetPotTests
    ) where

import           Control.Monad.Reader
import           Test.Tasty                     (TestTree, testGroup)

import           Plutus.Model

import           BetRef.Operations
import           OnChain.Compiled
import           BetRef.Tests.PlaceBet

import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-- | Our unit tests for taking the bet pot operation
takeBetPotTests :: TestTree
takeBetPotTests = testGroup "Take bet pot"
    [ testRun "Balance check after taking bet pot" $ takeBetsTrace 400 1_000 (valueFromLovelace 10_000_000)
      [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
      , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
      , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
      , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
      , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
      ]
      4 w2 (Just 0_398_617)
    , testRun "Must fail if attempt to take is by wrong guesser" $ mustFail . takeBetsTrace 400 1_000 (valueFromLovelace 10_000_000)
      [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
      , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
      , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
      , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
      , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
      ]
      5 w2 Nothing
    , testRun "Must fail even if old guess was closest but updated one is not" $ mustFail . takeBetsTrace 400 1_000 (valueFromLovelace 10_000_000)
      [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
      , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
      , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
      , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
      , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
      ] 2 w2 Nothing
    ]

-- | Run to call the `takeBets` operation.
takeBetsRun :: GYTxOutRef -> BetRefParams -> GYTxOutRef -> GYTxOutRef -> GYTxMonadRun GYTxId
takeBetsRun refScript brp toConsume refInput = do
  addr <- ownAddress
  skeleton <- takeBets refScript brp toConsume addr refInput
  sendSkeleton skeleton

-- | Trace for taking bet pot.
takeBetsTrace :: Integer                                            -- ^ slot for betUntil
              -> Integer                                            -- ^ slot for betReveal
              -> GYValue                                            -- ^ bet step
              -> [(Wallets -> Wallet, OracleAnswerDatum, GYValue)]  -- ^ List denoting the bets
              -> Integer                                            -- ^ Actual answer
              -> (Wallets -> Wallet)                                -- ^ Taker
              -> Maybe Integer                                      -- ^ Expected fees
              -> Wallets -> Run ()  -- Our continuation function
takeBetsTrace betUntil' betReveal' betStep walletBets answer getTaker mExpectedFees ws@Wallets{..} = do
  (brp, refScript) <- computeParamsAndAddRefScript betUntil' betReveal' betStep ws
  multipleBetsTraceCore brp refScript walletBets ws
  -- Now lets take the bet
  mMRef <- runWallet w1 $ addRefInput True (walletAddress w8) (datumFromPlutusData $ OracleAnswerDatum answer)
  let taker = getTaker ws
  case mMRef of
    Just (Just refInput) -> do
      void $ runWallet taker $ do
        betRefAddr <- betRefAddress brp
        [_scriptUtxo@GYUTxO {utxoRef, utxoValue}] <- utxosToList <$> utxosAtAddress betRefAddr Nothing
        waitUntilSlot $ slotFromApi (fromInteger betReveal')
        case mExpectedFees of
          Just expectedFees ->
            withWalletBalancesCheck [taker := utxoValue <> valueNegate (valueFromLovelace expectedFees)] $ do
              takeBetsRun refScript brp utxoRef refInput
          Nothing -> takeBetsRun refScript brp utxoRef refInput
    _anyOtherMatch -> fail "Couldn't place reference input successfully"