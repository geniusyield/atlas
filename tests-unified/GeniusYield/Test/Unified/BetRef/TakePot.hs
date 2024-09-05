module GeniusYield.Test.Unified.BetRef.TakePot (
  takeBetPotTests,
) where

import Control.Monad.Except (handleError)
import Test.Tasty (
  TestTree,
  testGroup,
 )

import GeniusYield.Test.Unified.BetRef.Operations
import GeniusYield.Test.Unified.BetRef.PlaceBet
import GeniusYield.Test.Unified.OnChain.BetRef.Compiled

import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

-- | Our unit tests for taking the bet pot operation
takeBetPotTests :: Setup -> TestTree
takeBetPotTests setup =
  testGroup
    "Take bet pot"
    [ mkTestFor "Balance check after taking bet pot" takeBetsTest
    , mkPrivnetTestFor_ "Balance check after taking bet pot - privnet" takeBetsTest
    , mkTestFor "Must fail if attempt to take is by wrong guesser" $ mustFail . wrongGuesserTakeBetsTest
    , mkPrivnetTestFor_ "Must fail if attempt to take is by wrong guesser - privnet" $ mustFailPrivnet . wrongGuesserTakeBetsTest
    , mkTestFor "Must fail even if old guess was closest but updated one is not" $ mustFail . badUpdatedGuessTakeBetsTest
    , mkPrivnetTestFor_ "Must fail even if old guess was closest but updated one is not - privnet" $ mustFailPrivnet . badUpdatedGuessTakeBetsTest
    ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup
    takeBetsTest :: (GYTxGameMonad m) => TestInfo -> m ()
    takeBetsTest TestInfo {..} =
      takeBetsTrace
        400
        1_000
        (valueFromLovelace 10_000_000)
        [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
        , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
        , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
        , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
        , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> valueSingleton testGoldAsset 1_000)
        ]
        4
        w2
        testWallets
    wrongGuesserTakeBetsTest :: (GYTxGameMonad m) => TestInfo -> m ()
    wrongGuesserTakeBetsTest TestInfo {..} =
      takeBetsTrace
        400
        1_000
        (valueFromLovelace 10_000_000)
        [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
        , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
        , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
        , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
        , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> valueSingleton testGoldAsset 1_000)
        ]
        5
        w2
        testWallets
    badUpdatedGuessTakeBetsTest :: (GYTxGameMonad m) => TestInfo -> m ()
    badUpdatedGuessTakeBetsTest TestInfo {..} =
      takeBetsTrace
        400
        1_000
        (valueFromLovelace 10_000_000)
        [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
        , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
        , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
        , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
        , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> valueSingleton testGoldAsset 1_000)
        ]
        2
        w2
        testWallets
    -- Must fail with script execution error (which is fired in the body error auto balance).
    mustFailPrivnet =
      handleError
        ( \case
            GYBuildTxException GYBuildTxBodyErrorAutoBalance {} -> pure ()
            e -> throwError e
        )

-- | Run to call the `takeBets` operation.
takeBetsRun :: (GYTxMonad m) => GYTxOutRef -> BetRefParams -> GYTxOutRef -> GYTxOutRef -> m GYTxId
takeBetsRun refScript brp toConsume refInput = do
  addr <- ownChangeAddress
  skeleton <- takeBets refScript brp toConsume addr refInput
  buildTxBody skeleton >>= signAndSubmitConfirmed

-- | Trace for taking bet pot.
takeBetsTrace ::
  (GYTxGameMonad m) =>
  -- | slot for betUntil
  Integer ->
  -- | slot for betReveal
  Integer ->
  -- | bet step
  GYValue ->
  -- | List denoting the bets
  [(Wallets -> User, OracleAnswerDatum, GYValue)] ->
  -- | Actual answer
  Integer ->
  -- | Taker
  (Wallets -> User) ->
  Wallets ->
  m () -- Our continuation function
takeBetsTrace betUntil' betReveal' betStep walletBets answer getTaker ws@Wallets {..} = do
  currSlot <- slotToInteger <$> slotOfCurrentBlock
  let betUntil = currSlot + betUntil'
      betReveal = currSlot + betReveal'
  (brp, refScript) <- computeParamsAndAddRefScript betUntil betReveal betStep ws
  multipleBetsTraceCore brp refScript walletBets ws
  -- Now lets take the bet
  refInput <- asUser w1 $ addRefInput True (userAddr w8) (datumFromPlutusData $ OracleAnswerDatum answer)
  let taker = getTaker ws
  betRefAddr <- betRefAddress brp
  _scriptUtxo@GYUTxO {utxoRef, utxoValue} <- head . utxosToList <$> utxosAtAddress betRefAddr Nothing
  waitUntilSlot_ $ slotFromApi (fromInteger betReveal)
  withWalletBalancesCheckSimple [taker := utxoValue]
    . asUser taker
    . void
    $ takeBetsRun refScript brp utxoRef refInput
