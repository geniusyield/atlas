module GeniusYield.Test.Unified.BetRef.TakePot
    ( takeBetPotTests
    ) where

import           Test.Tasty                                       (TestTree, testGroup)

import           GeniusYield.Test.Unified.BetRef.Operations
import           GeniusYield.Test.Unified.OnChain.BetRef.Compiled
import           GeniusYield.Test.Unified.BetRef.PlaceBet

import           GeniusYield.Imports
import           GeniusYield.Test.Clb
import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-- | Our unit tests for taking the bet pot operation
takeBetPotTests :: TestTree
takeBetPotTests = testGroup "Take bet pot"
    [ mkTestFor "Balance check after taking bet pot" $ takeBetsTrace 400 1_000
        (valueFromLovelace 10_000_000)
        [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
        , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
        , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
        , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
        , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
        ]
        4 w2
    , mkTestFor "Must fail if attempt to take is by wrong guesser" $ mustFail . takeBetsTrace
        400 1_000 (valueFromLovelace 10_000_000)
        [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
        , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
        , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
        , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
        , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
        ]
        5 w2
    , mkTestFor "Must fail even if old guess was closest but updated one is not" $
        mustFail . takeBetsTrace 400 1_000 (valueFromLovelace 10_000_000)
            [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
            , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
            , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
            , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
            , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
            ]
            2 w2
    ]

-- | Run to call the `takeBets` operation.
takeBetsRun :: GYTxMonad m => GYTxOutRef -> BetRefParams -> GYTxOutRef -> GYTxOutRef -> m GYTxId
takeBetsRun refScript brp toConsume refInput = do
  addr <- fmap (!! 0) ownAddresses -- FIXME:
  skeleton <- takeBets refScript brp toConsume addr refInput
  buildTxBody skeleton >>= signAndSubmitConfirmed

-- | Trace for taking bet pot.
takeBetsTrace :: Integer                                            -- ^ slot for betUntil
              -> Integer                                            -- ^ slot for betReveal
              -> GYValue                                            -- ^ bet step
              -> [(Wallets -> User, OracleAnswerDatum, GYValue)]    -- ^ List denoting the bets
              -> Integer                                            -- ^ Actual answer
              -> (Wallets -> User)                                  -- ^ Taker
              -> Wallets -> GYTxMonadClb ()  -- Our continuation function
takeBetsTrace betUntil' betReveal' betStep walletBets answer getTaker ws@Wallets{..} = do
  (brp, refScript) <- computeParamsAndAddRefScript betUntil' betReveal' betStep ws
  multipleBetsTraceCore brp refScript walletBets ws
  -- Now lets take the bet
  ref <- asUser w1 $ addRefInput True (userAddr w8) (datumFromPlutusData $ OracleAnswerDatum answer)
  let taker = getTaker ws
  case ref of
    Just refInput -> do
      betRefAddr <- betRefAddress brp
      [_scriptUtxo@GYUTxO {utxoRef, utxoValue}] <- utxosToList <$> utxosAtAddress betRefAddr Nothing
      waitUntilSlot_ $ slotFromApi (fromInteger betReveal')
      void . withWalletBalancesCheckSimple [taker := utxoValue] . asUser taker
        $ takeBetsRun refScript brp utxoRef refInput
    _ -> fail "Couldn't place reference input successfully"
