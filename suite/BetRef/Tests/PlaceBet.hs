module BetRef.Tests.PlaceBet
    ( placeBetTests
    , computeParamsAndAddRefScript
    -- , multipleBetsTraceCore
    ) where

import           Control.Monad.Reader
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as Set
import           Test.Tasty                     (TestTree, testGroup)

import           Plutus.Model

import           BetRef.Operations
import           OnChain.Compiled

import           GeniusYield.Test.Utils
import           GeniusYield.Types
import GeniusYield.Imports (printf)
import GeniusYield.TxBuilder.Clb
    ( GYTxMonadClb,
      Wallet(walletName),
      walletAddress,
      ownAddress,
      sendSkeleton, dumpUtxoState )
import GeniusYield.TxBuilder (gyLogDebug', slotToBeginTime, utxosAtAddress, logMsg, GYTxSkeleton, addressToPubKeyHash', mustHaveOutput, mustBeSignedBy)

-- | Our unit tests for placing bet operation
placeBetTests :: TestTree
placeBetTests = testGroup "Place Bet"
    [ testRunGYClb "Balance checks after placing first bet" $ firstBetTrace (OracleAnswerDatum 3) (valueFromLovelace 20_000_000) 0_176_941
    -- , testRun "Balance checks with multiple bets" $ multipleBetsTraceWrapper 400 1_000 (valueFromLovelace 10_000_000)
    --   [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
    --   , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
    --   , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
    --   , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
    --   , (w4, OracleAnswerDatum 5, valueFromLovelace 65_000_000 <> fakeGold 1_000)
    --   ]
    -- , testRun "Not adding atleast bet step amount should fail" $ mustFail . multipleBetsTraceWrapper 400 1_000 (valueFromLovelace 10_000_000)
    --   [ (w1, OracleAnswerDatum 1, valueFromLovelace 10_000_000)
    --   , (w2, OracleAnswerDatum 2, valueFromLovelace 20_000_000)
    --   , (w3, OracleAnswerDatum 3, valueFromLovelace 30_000_000)
    --   , (w2, OracleAnswerDatum 4, valueFromLovelace 50_000_000)
    --   , (w4, OracleAnswerDatum 5, valueFromLovelace 55_000_000 <> fakeGold 1_000)]
    ]

-- | Trace for placing the first bet.
firstBetTrace :: OracleAnswerDatum  -- ^ Guess
              -> GYValue            -- ^ Bet
              -> Integer            -- ^ Expected fees
              -> Wallets -> GYTxMonadClb ()  -- Our continuation function
              -- -> GYTxMonadClb ()  -- Our continuation function
firstBetTrace dat bet expectedFees Wallets{w1} = do
  gyLogDebug' "" "Hey!"
  void $ runWalletGYClb w1 simpleTest

  -- First step: Get the required parameters for initializing our parameterized script and add the corresponding reference script
  -- (brp, refScript) <- computeParamsAndAddRefScript 40 100 (valueFromLovelace 200_000_000) ws
  -- void $ runWalletGYClb w1 $ do  -- following operations are ran by first wallet, `w1`
  -- -- Second step: Perform the actual run.
  --   withWalletBalancesCheckClb [w1 := valueNegate (valueFromLovelace expectedFees <> bet)] $ do
  --     placeBetRun refScript brp dat bet Nothing

simpleTest :: GYTxMonadClb ()
simpleTest = do
  addr <- ownAddress
  gyLogDebug' "" $ "My own address is: " <> show addr
  skeleton <- trivialTx addr
  gyLogDebug' "" $ printf "tx skeleton: %s" (show skeleton)
  dumpUtxoState
  void $ sendSkeleton skeleton
  dumpUtxoState

trivialTx :: GYAddress -> GYTxMonadClb (GYTxSkeleton 'PlutusV2)
trivialTx ownAddr = do
  gyLogDebug' "" $ printf "ownAddr: %s" (show ownAddr)

  pkh <- addressToPubKeyHash' ownAddr
  let targetAddr = unsafeAddressFromText "addr_test1qr2vfntpz92f9pawk8gs0fdmhtfe32pqcx0s8fuztxaw3p5pjay24kygaj4g8uevf89ewxzvsdc60wln8spzm2al059q8a9w3x"
  -- let targetAddr = unsafeAddressFromText "addr1q82vfntpz92f9pawk8gs0fdmhtfe32pqcx0s8fuztxaw3p5pjay24kygaj4g8uevf89ewxzvsdc60wln8spzm2al059qytcwae"
  return $
    mustHaveOutput
      (GYTxOut
        { gyTxOutAddress = targetAddr
        , gyTxOutValue = valueFromLovelace 100_000_000
        , gyTxOutDatum = Nothing
        , gyTxOutRefS    = Nothing
        })
      <> mustBeSignedBy pkh


-- | Function to compute the parameters for the contract and add the corresponding refernce script.
computeParamsAndAddRefScript
  :: Integer                                    -- ^ Bet Until slot
  -> Integer                                    -- ^ Bet Reveal slot
  -> GYValue                                    -- ^ Bet step value
  -> Wallets -> GYTxMonadClb (BetRefParams, GYTxOutRef)  -- Our continuation
computeParamsAndAddRefScript betUntil' betReveal' betStep Wallets{..} = do
  let betUntil = slotFromApi (fromInteger betUntil')
      betReveal = slotFromApi (fromInteger betReveal')
  fmap fromJust $ runWalletGYClb w1 $ do
    betUntilTime <- slotToBeginTime betUntil
    betRevealTime <- slotToBeginTime betReveal
    let brp = BetRefParams (pubKeyHashToPlutus $ walletPubKeyHash w8) (timeToPlutus betUntilTime) (timeToPlutus betRevealTime) (valueToPlutus betStep)  -- let oracle be wallet `w8`.
    mORef <- addRefScriptClb (walletAddress w9) (betRefValidator' brp)
    case mORef of
      Nothing        -> fail "Couldn't find index of the Reference Script in outputs"
      Just refScript -> return (brp, refScript)

-- | Run to call the `placeBet` operation.
placeBetRun :: GYTxOutRef -> BetRefParams -> OracleAnswerDatum -> GYValue -> Maybe GYTxOutRef -> GYTxMonadClb GYTxId
placeBetRun refScript brp guess bet mPreviousBetsUtxoRef = do
  addr <- ownAddress
  skeleton <- placeBet refScript brp guess bet addr mPreviousBetsUtxoRef
  gyLogDebug' "" $ printf "tx skeleton: %s" (show skeleton)
  sendSkeleton skeleton

-- -- | Trace which allows for multiple bets.
-- multipleBetsTraceWrapper
--   :: Integer                                            -- ^ slot for betUntil
--   -> Integer                                            -- ^ slot for betReveal
--   -> GYValue                                            -- ^ bet step
--   -> [(Wallets -> Wallet, OracleAnswerDatum, GYValue)]  -- ^ List denoting the bets
--   -> Wallets -> Run ()                                  -- Our continuation function
-- multipleBetsTraceWrapper betUntil' betReveal' betStep walletBets ws = do
--   -- First step: Get the required parameters for initializing our parameterized script and add the corresponding reference script
--   (brp, refScript) <- computeParamsAndAddRefScript betUntil' betReveal' betStep ws
--   -- Second step: Perform the actual bet operations
--   multipleBetsTraceCore brp refScript walletBets ws

-- -- | Trace which allows for multiple bets.
-- multipleBetsTraceCore
--   :: BetRefParams
--   -> GYTxOutRef                                         -- ^ Reference script
--   -> [(Wallets -> Wallet, OracleAnswerDatum, GYValue)]  -- ^ List denoting the bets
--   -> Wallets -> Run ()                                  -- Our continuation function
-- multipleBetsTraceCore brp refScript walletBets ws@Wallets{..} = do
--   let
--       -- | Perform the actual bet operation by the corresponding wallet.
--       performBetOperations [] _ = return ()
--       performBetOperations ((getWallet, dat, bet) : remWalletBets) isFirst = do
--         if isFirst then do
--           void $ runWallet (getWallet ws) $ do
--             void $ placeBetRun refScript brp dat bet Nothing
--           performBetOperations remWalletBets False
--         else do
--           -- need to get previous bet utxo
--           void $ runWallet (getWallet ws) $ do
--             betRefAddr <- betRefAddress brp
--             [_scriptUtxo@GYUTxO {utxoRef}] <- utxosToList <$> utxosAtAddress betRefAddr Nothing
--             void $ placeBetRun refScript brp dat bet (Just utxoRef)
--           performBetOperations remWalletBets False
--       -- | To sum the bet amount for the corresponding wallet.
--       sumWalletBets _wallet [] acc = acc
--       sumWalletBets wallet ((getWallet, _dat, bet) : remWalletBets) acc = sumWalletBets wallet remWalletBets (if getWallet ws == wallet then acc <> valueNegate bet else acc)
--       -- | Idea here is that for each wallet, we want to know how much has been bet. If we encounter a new wallet, i.e., wallet for whose we haven't yet computed value lost, we call `sumWalletBets` on it.
--       getBalanceDiff [] _set acc = acc
--       getBalanceDiff wlBets@((getWallet, _dat, _bet) : remWalletBets) set acc =
--         let wallet = getWallet ws
--             wallet'sName = walletName wallet
--         in
--           if Set.member wallet'sName set then getBalanceDiff remWalletBets set acc
--           else
--            getBalanceDiff remWalletBets (Set.insert wallet'sName set) ((wallet := sumWalletBets wallet wlBets mempty) : acc)
--       balanceDiffWithoutFees = getBalanceDiff walletBets Set.empty []
--   balanceBeforeAllTheseOps <- fmap fromJust $ runWallet w1 $ traverse (\(wallet, _value) -> balance wallet) balanceDiffWithoutFees
--   performBetOperations walletBets True
--   balanceAfterAllTheseOps <- fmap fromJust $ runWallet w1 $ traverse (\(wallet, _value) -> balance wallet) balanceDiffWithoutFees
--   -- pure ()
--   void $ runWallet w1 $ verify (zip3 balanceDiffWithoutFees balanceBeforeAllTheseOps balanceAfterAllTheseOps)
--   where
--     -- | Function to verify that the wallet indeed lost by /roughly/ the bet amount. We say /roughly/ as fees is assumed to be within (0, 1 ada].
--     verify [] = return ()
--     verify (((wallet, diff), vBefore, vAfter) : xs) =
--       let vAfterWithoutFees = vBefore <> diff
--           (expectedAdaWithoutFees, expectedOtherAssets) = valueSplitAda vAfterWithoutFees
--           (actualAda, actualOtherAssets) = valueSplitAda vAfter
--           -- threshold = valueFromLovelace 1_000_000  -- 1 ada
--           threshold = 1_000_000  -- 1 ada
--       in if expectedOtherAssets == actualOtherAssets && actualAda < expectedAdaWithoutFees && expectedAdaWithoutFees - threshold <= actualAda then verify xs
--              -- valueGreater vAfterWithoutFees vAfter && valueGreaterOrEqual vAfter (valueMinus vAfterWithoutFees threshold) then verify xs
--          else fail ("For wallet " <> walletName wallet <> " expected value (without fees) " <> show vAfterWithoutFees <> " but actual is " <> show vAfter)