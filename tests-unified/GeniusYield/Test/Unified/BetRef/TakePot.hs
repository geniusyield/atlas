module GeniusYield.Test.Unified.BetRef.TakePot (
  takeBetPotTests,
  takeBetPotTestsClb,
) where

import Control.Monad.Except (handleError)
import Control.Monad.Extra (maybeM)
import Data.Maybe (listToMaybe)
import Test.Tasty (
  TestTree,
  testGroup,
 )

import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Unified.BetRef.Operations
import GeniusYield.Test.Unified.BetRef.PlaceBet
import GeniusYield.Test.Unified.OnChain.BetRef.Compiled
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

takeBetPotTestsClb :: TestTree
takeBetPotTestsClb =
  testGroup
    "Take bet pot"
    [ mkTestFor "Just take bet pot" takeBetsTest
    , mkTestFor "Take by wrong guesser" $
        mustFail . wrongGuesserTakeBetsTest
    , mkTestFor "The first bet matters" $
        mustFail . badUpdatedGuessTakeBetsTest
    ]

-- | Our unit tests for taking the bet pot operation
takeBetPotTests :: Setup -> TestTree
takeBetPotTests setup =
  testGroup
    "Take bet pot"
    [ mkPrivnetTestFor_ "Just take bet pot" takeBetsTest
    , mkPrivnetTestFor_ "Take by wrong guesser" $
        mustFailPrivnet . wrongGuesserTakeBetsTest
    , mkPrivnetTestFor_ "The first bet matters" $
        mustFailPrivnet . badUpdatedGuessTakeBetsTest
    ]
 where
  mkPrivnetTestFor_ = flip mkPrivnetTestFor setup
  -- Must fail with script execution error (which is fired in the body error auto balance).
  mustFailPrivnet =
    handleError
      ( \case
          GYBuildTxException GYBuildTxBodyErrorAutoBalance {} -> pure ()
          e -> throwError e
      )

takeBetsTest :: GYTxGameMonad m => TestInfo -> m ()
takeBetsTest TestInfo {..} =
  mkTakeBetsTest
    400
    1_000
    (valueFromLovelace 10_000_000)
    [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
    , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
    , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
    , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
    ,
      ( w4
      , OracleAnswerDatum 5
      , valueFromLovelace 65_000_000
          <> valueSingleton testGoldAsset 1_000
      )
    ]
    4
    w2
    testWallets

wrongGuesserTakeBetsTest :: GYTxGameMonad m => TestInfo -> m ()
wrongGuesserTakeBetsTest TestInfo {..} =
  mkTakeBetsTest
    400
    1_000
    (valueFromLovelace 10_000_000)
    [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
    , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
    , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
    , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
    ,
      ( w4
      , OracleAnswerDatum 5
      , valueFromLovelace 65_000_000
          <> valueSingleton testGoldAsset 1_000
      )
    ]
    5
    w2
    testWallets

badUpdatedGuessTakeBetsTest :: GYTxGameMonad m => TestInfo -> m ()
badUpdatedGuessTakeBetsTest TestInfo {..} =
  mkTakeBetsTest
    400
    1_000
    (valueFromLovelace 10_000_000)
    [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
    , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
    , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
    , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
    ,
      ( w4
      , OracleAnswerDatum 5
      , valueFromLovelace 65_000_000
          <> valueSingleton testGoldAsset 1_000
      )
    ]
    2
    w2
    testWallets

-- | Trace for taking bet pot.
mkTakeBetsTest ::
  GYTxGameMonad m =>
  Integer ->
  Integer ->
  GYValue ->
  [Bet] ->
  Integer ->
  -- | Pot taker
  (Wallets -> User) ->
  Wallets ->
  m ()
mkTakeBetsTest betUntil betReveal betStep walletBets answer getTaker ws@Wallets {..} = do
  (brp, refScript, script) <- runDeployScript betUntil betReveal betStep ws
  runMultipleBets brp refScript script walletBets ws
  -- Now lets take the bet
  refInput <- asUser w1 $ addRefInput True (userAddr w8) (datumFromPlutusData $ OracleAnswerDatum answer)
  let taker = getTaker ws
  betRefAddr <- scriptAddress script
  GYUTxO {utxoRef, utxoValue} <-
    head . utxosToList
      <$> utxosAtAddress betRefAddr Nothing
  currSlot <- slotToInteger <$> slotOfCurrentBlock
  let waitUntil = slotFromApi (fromInteger $ currSlot + betReveal)
  gyLogDebug' "" $ "waiting till slot: " <> show waitUntil
  waitUntilSlot_ waitUntil
  withWalletBalancesCheckSimple [taker := utxoValue]
    . void
    $ takeBetsRun refScript script brp utxoRef refInput taker

-- | Run to call the `takeBets` operation.
takeBetsRun ::
  ( GYTxGameMonad m
  , v `VersionIsGreaterOrEqual` 'PlutusV2
  ) =>
  GYTxOutRef ->
  GYValidator v ->
  BetRefParams ->
  GYTxOutRef ->
  GYTxOutRef ->
  User ->
  m GYTxId
takeBetsRun refScript script brp toConsume refInput taker = do
  asUser taker $ do
    addr <-
      maybeM
        (throwAppError $ someBackendError "No own addresses")
        pure
        $ listToMaybe <$> ownAddresses
    skeleton <- takeBets refScript script brp toConsume addr refInput
    buildTxBody skeleton >>= signAndSubmitConfirmed
