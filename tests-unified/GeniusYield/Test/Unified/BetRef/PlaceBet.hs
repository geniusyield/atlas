module GeniusYield.Test.Unified.BetRef.PlaceBet
    ( placeBetTests
    , placeBetTestsClb
    , runDeployScript
    , runMultipleBets
    , Bet
    ) where

import           Control.Monad.Except                             (handleError)
import           Control.Monad.Extra                              (maybeM)
import qualified Data.Set                                         as Set
import qualified Data.Text                                        as T
import           Data.Maybe                                       (listToMaybe)
import           Test.Tasty                                       (TestTree,
                                                                   testGroup)

import GeniusYield.Test.Unified.BetRef.Operations
import GeniusYield.Test.Unified.OnChain.BetRef.Compiled

import           GeniusYield.Test.Unified.BetRef.Operations
import           GeniusYield.Test.Unified.OnChain.BetRef.Compiled
import           GeniusYield.Imports
import           GeniusYield.HTTP.Errors
import           GeniusYield.Imports
import           GeniusYield.Test.Clb
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types


-- | Test environment 'WalletInfo' among other things provides nine wallets that
-- be used in tests. For convinience we assign some meaningful names to them.
admin, oracle, holder :: Wallets -> User
admin  = w1 -- Runs some administrative action, e.g. deplys the script
oracle = w8 -- A user that is going to reveal the answer
holder = w9 -- A user to store the reference script

-- | Test suite for the emulator
placeBetTestsClb :: TestTree
placeBetTestsClb = testGroup "Place bet"
    [ mkTestFor "Simple tx" $ simpleTxTest
    , mkTestFor "Placing first bet" firstBetTest'
    , mkTestFor "Multiple bets" multipleBetsTest
    , mkTestFor "Multiple bets - to small step" $ mustFail . failingMultipleBetsTest
    ]

-- | Test suite for a private testnet
placeBetTests :: Setup -> TestTree
placeBetTests setup = testGroup "Place bet"
    [ mkPrivnetTestFor_ "Simple tx" $ simpleTxTest
    , mkPrivnetTestFor_ "Placing first bet" firstBetTest'
    , mkPrivnetTestFor_ "Multiple bets" multipleBetsTest
    , mkPrivnetTestFor' "Multiple bets - too small step" GYDebug setup $
        handleError
          ( \case
              GYBuildTxException GYBuildTxBodyErrorAutoBalance {} -> pure ()
              e -> throwError e
          )
          . failingMultipleBetsTest
    ]
  where
    mkPrivnetTestFor_ = flip mkPrivnetTestFor setup

-- -----------------------------------------------------------------------------
-- Simple tx
-- -----------------------------------------------------------------------------

-- | Trace for a super-simple spending transaction. This function combines
-- the  runner and the test for simplicity's sake.
simpleTxTest :: GYTxGameMonad m => TestInfo -> m ()
simpleTxTest (testWallets -> Wallets{w1}) = do
  withWalletBalancesCheckSimple [w1 := valueFromLovelace (-100_000_000)] .
   asUser w1 $ do
    skeleton <- mkTrivialTx
    gyLogDebug' "" $ printf "tx skeleton: %s" (show skeleton)
    txId <- buildTxBody skeleton >>= signAndSubmitConfirmed
    gyLogDebug' "" $ printf "tx submitted, txId: %s" txId

-- Pretend off-chain code written in 'GYTxUserQueryMonad m'
mkTrivialTx :: GYTxUserQueryMonad m => m (GYTxSkeleton 'PlutusV2)
mkTrivialTx = do
  addr <- maybeM (throwAppError $ someBackendError "No own addresses")
    pure  $ listToMaybe <$> ownAddresses
  gyLogDebug' "" $ printf "ownAddr: %s" (show addr)
  pkh <- addressToPubKeyHash' addr
  let targetAddr = unsafeAddressFromText "addr_test1qr2vfntpz92f9pawk8gs0fdmhtfe32pqcx0s8fuztxaw3p5pjay24kygaj4g8uevf89ewxzvsdc60wln8spzm2al059q8a9w3x"
  return $
    mustHaveOutput
      ( GYTxOut
          { gyTxOutAddress = targetAddr
          , gyTxOutValue = valueFromLovelace 100_000_000
          , gyTxOutDatum = Nothing
          , gyTxOutRefS = Nothing
          }
      )
      <> mustBeSignedBy pkh

-- -----------------------------------------------------------------------------
-- First bet
-- -----------------------------------------------------------------------------

-- | Run to call the `placeBet` operation.
runPlaceBet
  :: GYTxGameMonad m
  => GYTxOutRef                        -- ^ Script output reference
  -> BetRefParams                      -- ^ Parameters
  -> OracleAnswerDatum                 -- ^ Bet guess
  -> GYValue                           -- ^ Bet value
  -> Maybe GYTxOutRef                  -- ^ Ref output with existing bets
  -> User                              -- ^ User that plays bet
  -> m GYTxId
runPlaceBet refScript brp guess bet mPrevBets user = do
  gyLogDebug' ""
    $ printf "placing a bet with guess %s and value %s"
      (show guess) (show bet)
  asUser user $ do
    addr <- maybeM (throwAppError $ someBackendError "No own addresses")
      pure  $ listToMaybe <$> ownAddresses
    -- Call the operation
    skeleton <- placeBet refScript brp guess bet addr mPrevBets
    buildTxBody skeleton >>= signAndSubmitConfirmed

firstBetTest' :: GYTxGameMonad m => TestInfo -> m ()
firstBetTest' = firstBetTest
  40
  100
  (valueFromLovelace 200_000_000)
  (OracleAnswerDatum 3)
  (valueFromLovelace 20_000_000)

-- | Test for placing the first bet.
firstBetTest
  :: GYTxGameMonad m
  => Integer
  -> Integer
  -> GYValue
  -> OracleAnswerDatum
  -> GYValue
  -> TestInfo
  -> m ()
firstBetTest betUntil betReveal betStep dat bet (testWallets -> ws@Wallets{w1}) = do
  (brp, refScript) <- runDeployScript betUntil betReveal betStep ws
  withWalletBalancesCheckSimple [w1 := valueNegate bet] $ do
    void $ runPlaceBet refScript brp dat bet Nothing w1

-- -----------------------------------------------------------------------------
-- Multiple bets
-- -----------------------------------------------------------------------------

-- This is an alias for fields of `Wallet` datatype
type Wallet = Wallets -> User

-- This type represent a bet made by a wallet
type Bet = (Wallet, OracleAnswerDatum, GYValue)

multipleBetsTest :: GYTxGameMonad m => TestInfo -> m ()
multipleBetsTest TestInfo{..} = mkMultipleBetsTest
  400 1_000 (valueFromLovelace 10_000_000)
  [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
  , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
  , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
  , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
  , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000
                                <> valueSingleton testGoldAsset 1_000)
  ]
  testWallets

failingMultipleBetsTest :: GYTxGameMonad m => TestInfo -> m ()
failingMultipleBetsTest TestInfo{..} = mkMultipleBetsTest
  400 1_000 (valueFromLovelace 10_000_000)
  [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
  , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
  , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
  , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
  , (w4, OracleAnswerDatum 5, valueFromLovelace 55_000_000
                                <> valueSingleton testGoldAsset 1_000)
  ]
  testWallets

-- | Makes a test case for placing multiple bets.
mkMultipleBetsTest
  :: GYTxGameMonad m
  => Integer                 -- ^ Number of slots for betting
  -> Integer                 -- ^ Number of slots for revealing
  -> GYValue                 -- ^ Bet step
  -> [Bet]                   -- ^ List denoting the bets
  -> Wallets                 -- ^ Wallets available
  -> m ()
mkMultipleBetsTest betUntil betReveal betStep bets ws = do
  -- Deploy script
  (brp, refScript) <- runDeployScript betUntil betReveal betStep ws
  -- Get the balance
  balanceBefore <- getBalance
  gyLogDebug' "" $ printf "balanceBeforeAllTheseOps: %s" (mconcat balanceBefore)
  -- Run operations
  runMultipleBets brp refScript bets ws
  -- Get the balance again
  balanceAfter <- getBalance
  gyLogDebug' "" $ printf "balanceAfterAllTheseOps: %s" (mconcat balanceAfter)
  -- Check the difference
  verify $ zip3
    walletsAndBets
    balanceBefore
    balanceAfter
  where
    -- | Returns the balances for all wallets that play the game
    getBalance :: GYTxGameMonad m => m [GYValue]
    getBalance = traverse
      (\(wallet, _) -> queryBalances $ userAddresses' wallet)
      walletsAndBets

    -- | Builds the list of wallets and their respective bets made.
    -- The idea here is that if we encounter a new wallet,
    -- i.e., wallet for whose we haven't yet computed value lost,
    -- we calculate the total once so we can ignore other entries
    -- for this wallet.
    -- FIXME: very ineffective, can be simplified drastically.
    walletsAndBets ::  [(User, GYValue)]
    walletsAndBets = go bets Set.empty []
      where
        go [] _ acc = acc
        go allBets@((getWallet, _, _) : remBets) set acc =
          let wallet = getWallet ws
              addr = userAddr wallet
          in
            if Set.member addr set
              then go remBets set acc -- already summed
              else go
                    remBets
                    (Set.insert addr set)
                    ((wallet := totalBets wallet allBets mempty) : acc)

        -- | Recursive functions that sums all bets for the corresponding wallet.
        totalBets :: User -> [Bet] -> GYValue -> GYValue
        totalBets _ [] acc = acc
        totalBets wallet ((getWallet, _, bet) : remBets) acc =
            totalBets wallet remBets $
              if getWallet ws == wallet
                  then acc <> valueNegate bet
                  else acc


    -- | Function to verify that the wallet indeed lost by /roughly/ the bet amount.
    -- We say /roughly/ as fees is assumed to be within (0, 1 ada].
    verify :: GYTxGameMonad m => [((User, GYValue), GYValue, GYValue)] -> m ()
    verify [] = return ()
    verify (((wallet, diff), vBefore, vAfter) : xs) =
      let vAfterWithoutFees = vBefore <> diff
          (expectedAdaWithoutFees, expectedOtherAssets) = valueSplitAda vAfterWithoutFees
          (actualAda, actualOtherAssets) = valueSplitAda vAfter
          threshold = 1_500_000  -- 1.5 ada
      in
        if expectedOtherAssets == actualOtherAssets
            && actualAda < expectedAdaWithoutFees
            && expectedAdaWithoutFees - threshold <= actualAda
        then verify xs
        else
          throwAppError . someBackendError . T.pack $
            printf "For wallet %s expected value (without fees) %s but actual is %s"
              (show $ userAddr wallet)
              (show vAfterWithoutFees)
              (show vAfter)

-- | Runner for multiple bets.
runMultipleBets
  :: GYTxGameMonad m
  => BetRefParams
  -> GYTxOutRef                 -- ^ Reference script
  -> [Bet]
  -> Wallets
  -> m ()
runMultipleBets brp refScript bets ws = go bets True
  where
    go [] _ = return ()
    go ((getWallet, dat, bet) : remBets) isFirst = do
      if isFirst then do
        gyLogInfo' "" "placing the first bet"
        void $ runPlaceBet refScript brp dat bet Nothing (getWallet ws)
        go remBets False
      else do
        gyLogInfo' "" "placing a next bet"
        -- need to get previous bet utxo
        betRefAddr <- betRefAddress brp
        GYUTxO{utxoRef} <- head . utxosToList <$> utxosAtAddress betRefAddr Nothing
        gyLogDebug' "" $ printf "previous bet utxo: %s" utxoRef
        void $ runPlaceBet refScript brp dat bet (Just utxoRef) (getWallet ws)
        go remBets False

-- -----------------------------------------------------------------------------
-- Auxiliary runners
-- -----------------------------------------------------------------------------

-- | Runner to build and submit a transaction that deploys the reference script.
runDeployScript
  :: GYTxGameMonad m
  => Integer                        -- ^ Bet Until slot
  -> Integer                        -- ^ Bet Reveal slot
  -> GYValue                        -- ^ Bet step value
  -> Wallets
  -> m (BetRefParams, GYTxOutRef)
runDeployScript betUntil betReveal betStep ws = do
  (params, script) <- mkScript betUntil betReveal (userPkh $ oracle ws) betStep
  asUser (admin ws) $ do
    let sAddr = userAddr (holder ws)
    gyLogDebug' "" $ printf "Ref script storage addr: %s" (show sAddr)
    refScript <- addRefScript sAddr script
    gyLogDebug' "" $ printf "Ref script deployed, ref output is: %s" (show refScript)
    pure (params, refScript)
