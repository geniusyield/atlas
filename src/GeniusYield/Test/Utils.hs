{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : GeniusYield.Test.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Utils
    ( Run
    , testRun
    , Wallet (..)
    , Wallets (..)
    , newWallet
    , runWallet
    , runWallet'
    , walletAddress
    , walletPubKeyHash
    , balance
    , withBalance
    , withWalletBalancesCheck
    , withWalletBalancesCheckSimple
    , getBalance
    , getBalances
    , runWithWalletBalancesCheck
    , waitUntilSlot
    , waitNSlotsGYTxMonad
    , findLockedUtxosInBody
    , utxosInBody
    , addRefScript
    , expectInsufficientFunds
    , addRefInput
    , fakeGold, fakeIron
    , afterAllSucceed
    , feesFromLovelace
    , withMaxQCTests
    , pattern (:=)
    ) where

import           Control.Monad.Random
import           Control.Monad.State
import           Data.List                     (findIndex)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromJust)
import           Data.Semigroup                (Sum (..))
import           Data.Typeable
import           Plutus.Model                  hiding (currentSlot)
import qualified Cardano.Simple.Ledger.Slot    as Fork
import qualified Cardano.Simple.Ledger.Tx      as Fork
import qualified PlutusLedgerApi.V1.Value      as Plutus
import qualified PlutusLedgerApi.V2            as Plutus2
import qualified Test.Tasty                    as Tasty
import qualified Test.Tasty.QuickCheck         as Tasty
import qualified Test.Tasty.Runners            as Tasty

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- tasty tools
-------------------------------------------------------------------------------

-- | Runs the second 'Tasty.TestTree' after all tests in the first 'Tasty.TestTree' succeed
afterAllSucceed :: Tasty.TestTree -> Tasty.TestTree -> Tasty.TestTree
afterAllSucceed = Tasty.after Tasty.AllSucceed . pat where
    pat :: Tasty.TestTree -> String
    pat dep = case dep of
        Tasty.SingleTest tn _        -> tn
        Tasty.TestGroup tn _         -> tn
        Tasty.After _ _ dep'         -> pat dep'
        Tasty.PlusTestOptions _ dep' -> pat dep'
        Tasty.WithResource _ f       -> pat (f (fail "Not running IO"))
        Tasty.AskOptions f           -> pat (f mempty)

-------------------------------------------------------------------------------
-- QC
-------------------------------------------------------------------------------

-- | Adjust the number of QuickCheck cases to generate.
withMaxQCTests :: Int -> Tasty.TestTree -> Tasty.TestTree
withMaxQCTests n = Tasty.adjustOption f where
    f :: Tasty.QuickCheckTests -> Tasty.QuickCheckTests
    f (Tasty.QuickCheckTests m) = Tasty.QuickCheckTests (min m n)

-------------------------------------------------------------------------------
-- test assets
-------------------------------------------------------------------------------

class    FromFakeCoin a                 where fromFakeCoin :: FakeCoin -> a
instance FromFakeCoin FakeCoin          where fromFakeCoin = id
instance FromFakeCoin GYAssetClass      where fromFakeCoin = fromRight (error "invalid asset class") . assetClassFromPlutus . fakeCoin
instance FromFakeCoin Plutus.AssetClass where fromFakeCoin = fakeCoin

-- | This allows to write e.g. @'fakeGold' 1000 :: GYValue@.
instance (a ~ Integer, b ~ GYValue) => FromFakeCoin (a -> b) where
    fromFakeCoin c = fromRight (error "invalid value") . valueFromPlutus . fakeValue c

-- | Fake \"Gold\" coin to use during tests.
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeGold :: FromFakeCoin a => a
fakeGold = fromFakeCoin $ FakeCoin "Gold"

-- | Fake \"Iron\" coin to use during tests
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeIron :: FromFakeCoin a => a
fakeIron = fromFakeCoin $ FakeCoin "Iron"

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

{- | Given a test name, runs the trace for every wallet, checking there weren't
     errors.
-}
testRun :: String -> (Wallets -> Run a) -> Tasty.TestTree
testRun name run = do
    testNoErrorsTrace v defaultBabbage name $ do
        ws <- evalRandT wallets pureGen
        run ws
  where
    v = valueToPlutus $ valueFromLovelace 1_000_000_000_000_000 <>
                        fakeGold                  1_000_000_000 <>
                        fakeIron                  1_000_000_000

    w = valueFromLovelace 1_000_000_000_000 <>
        fakeGold                  1_000_000 <>
        fakeIron                  1_000_000

    wallets :: RandT StdGen Run Wallets
    wallets = Wallets <$> newWallet "w1" w
                      <*> newWallet "w2" w
                      <*> newWallet "w3" w
                      <*> newWallet "w4" w
                      <*> newWallet "w5" w
                      <*> newWallet "w6" w
                      <*> newWallet "w7" w
                      <*> newWallet "w8" w
                      <*> newWallet "w9" w


-- | Available wallets.
data Wallets = Wallets
    { w1 :: !Wallet
    , w2 :: !Wallet
    , w3 :: !Wallet
    , w4 :: !Wallet
    , w5 :: !Wallet
    , w6 :: !Wallet
    , w7 :: !Wallet
    , w8 :: !Wallet
    , w9 :: !Wallet
    } deriving (Show, Eq, Ord)

-- | Given a name and an initial fund, create a testing wallet.
newWallet :: String -> GYValue -> RandT StdGen Run Wallet
newWallet n v = do
    pkh  <- lift . newUser $ valueToPlutus v
    nid  <- lift networkIdRun
    mkp  <- lift $ getUserSignKey pkh
    case mkp of
        Nothing -> fail $ "error creating user with pubkey hash " <> show pkh
        Just kp -> return $
                       Wallet
                        { walletPaymentSigningKey = paymentSigningKeyFromLedgerKeyPair kp
                        , walletNetworkId         = nid
                        , walletName              = n
                        }

-- | Runs a `GYTxMonadRun` action using the given wallet.
runWallet :: Wallet -> GYTxMonadRun a -> Run (Maybe a)
runWallet w action = flip evalRandT pureGen $ asRandRun w action

-- | Version of `runWallet` that fails if `Nothing` is returned by the action.
runWallet' :: Wallet -> GYTxMonadRun a -> Run a
runWallet' w action = do
    ma <- runWallet w action
    case ma of
        Nothing -> fail $ printf "Run wallet action returned Nothing"
        Just a  -> return a

-- | Gets a GYPubKeyHash of a testing wallet.
walletPubKeyHash :: Wallet -> GYPubKeyHash
walletPubKeyHash = fromJust . addressToPubKeyHash . walletAddress

{- | Gets the balance from anything that `HasAddress`. The usual case will be a
     testing wallet.
-}
balance :: HasAddress a => a -> GYTxMonadRun GYValue
balance a = do
    nid <- networkId
    case addressFromPlutus nid $ toAddress a of
        Left err   -> fail $ show err
        Right addr -> do
            utxos <- utxosAtAddress addr
            return $ foldMapUTxOs utxoValue utxos

{- | Computes a `GYTxMonadRun` action and returns the result and how this action
     changed the balance of some "Address".
-}
withBalance :: HasAddress a => String -> a -> GYTxMonadRun b -> GYTxMonadRun (b, GYValue)
withBalance n a m = do
    old <- balance a
    b   <- m
    new <- balance a
    let diff = new `valueMinus` old
    liftRun $ logInfo $ printf "%s:\nold balance: %s\nnew balance: %s\ndiff: %s" n old new diff
    return (b, diff)

{- | Computes a 'GYTxMonadRun' action, checking that the 'Wallet' balances
        change according to the input list.

Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheck :: [(Wallet, GYValue)] -> GYTxMonadRun a -> GYTxMonadRun a
withWalletBalancesCheck [] m            = m
withWalletBalancesCheck ((w, v) : xs) m = do
    (b, diff) <- withBalance (walletName w) w $ withWalletBalancesCheck xs m
    unless (diff == v) $
        fail $ printf "expected balance difference of %s for wallet %s, but the actual difference was %s" v (walletName w) diff
    return b

{- | Computes a 'GYTxMonadRun' action, checking that the 'Wallet' balances
        change according to the input list. This is a simplified version of `withWalletBalancesCheck` where the input list need not consider lovelaces required for fees & to satisfy the min ada requirements as these are added automatically. It is therefore recommended to use this function over `withWalletBalancesCheck` to avoid hardcoding the lovelaces required for fees & min ada constraints.

Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheckSimple :: [(Wallet, GYValue)] -> GYTxMonadRun a -> GYTxMonadRun a
withWalletBalancesCheckSimple wallValueDiffs m = do
  bs <- mapM (balance . fst) wallValueDiffs
  a <- m
  walletExtraLovelaceMap <- gets walletExtraLovelace
  bs' <- mapM (balance . fst) wallValueDiffs

  forM_ (zip3 wallValueDiffs bs' bs) $
    \((w, v), b', b) ->
      let newBalance = case Map.lookup (walletName w) walletExtraLovelaceMap of
            Nothing -> b'
            Just (extraLovelaceForFees, extraLovelaceForMinAda) -> b' <> valueFromLovelace (coerce $ extraLovelaceForFees <> extraLovelaceForMinAda)
          diff = newBalance `valueMinus` b
        in unless (diff == v) $ fail $
            printf "Wallet: %s. Old balance: %s. New balance: %s. New balance after adding extra lovelaces %s. Expected balance difference of %s, but the actual difference was %s" (walletName w) b b' newBalance v diff
  return a

-- | Given a wallet returns its balance.
getBalance :: HasCallStack => Wallet -> Run GYValue
getBalance w = fromJust <$> runWallet w (balance w)

-- | Given a list of wallets returns its balances.
getBalances :: HasCallStack => [Wallet] -> Run [GYValue]
getBalances = mapM getBalance

{- | Computes a 'Run' action, checking that the 'Wallet' balances change according
     to the input list.

Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
runWithWalletBalancesCheck
    :: HasCallStack
    => Wallets
    -> [(Wallets -> Wallet, GYValue)]
    -> Run a
    -> Run a
runWithWalletBalancesCheck ws bsCheck m = do
    let wsToCheck = map (($ ws) . fst) bsCheck

    bs <- getBalances wsToCheck
    a <- m
    bs' <- getBalances wsToCheck

    forM_ (zip3 bsCheck bs' bs) $
        \((w, v), b', b) ->
            let diff = b' `valueMinus` b
            in unless (diff == v) $ fail $
               printf "expected balance difference of %s for wallet %s, but the actual difference was %s"
                      v (walletName $ w ws) diff
    return a

-- | Waits N slots.
waitNSlotsGYTxMonad :: Integer -> GYTxMonadRun ()
waitNSlotsGYTxMonad = liftRun . waitNSlots . Fork.Slot

-- | Waits until a certain 'GYSlot'.
-- Fails if the given slot is greater than the current slot.
waitUntilSlot :: GYSlot -> GYTxMonadRun ()
waitUntilSlot slot = do
    now <- slotOfCurrentBlock
    let d = slotToInteger slot - slotToInteger now
    if | d < 0     -> fail $ printf "can't wait for slot %d, because current slot is %d" (slotToInteger slot) (slotToInteger now)
       | d == 0    -> return ()
       | otherwise -> liftRun $ waitNSlots $ Fork.Slot d

{- | Returns the list of outputs of the transaction for the given address.
     Returns Nothing if it fails to decode an address contained in the
      transaction outputs.
-}
findLockedUtxosInBody :: Num a => GYNetworkId -> GYAddress -> Fork.Tx -> Maybe [a]
findLockedUtxosInBody netId addr Fork.Tx{txOutputs = os} =
  let
    findAllMatches (_    , []                             , acc) = Just acc
    findAllMatches (index, Plutus2.TxOut addr' _ _ _ : os', acc) = either
        (const Nothing)
        (\addr'' -> if addr'' == addr
            then findAllMatches (index + 1, os', index : acc)
            else findAllMatches (index + 1, os', acc))
        (addressFromPlutus netId addr')
  in
    findAllMatches (0, os, [])

-- | Given PSM transaction and the corresponding transaction id, gives the list of UTxOs generated by that body /provided they still exist/. This function is usually expected to be called immediately after the transaction's submission.
utxosInBody :: GYTxQueryMonad m => Fork.Tx -> GYTxId -> m [Maybe GYUTxO]
utxosInBody Fork.Tx{txOutputs = os} txId = mapM (\i -> utxoAtTxOutRef (txOutRefFromTuple (txId, fromInteger $ toInteger i))) [0 .. (length os - 1)]

-- | Adds the given script to the given address and returns the reference for it.
addRefScript :: GYAddress -> GYValidator 'PlutusV2 -> GYTxMonadRun (Maybe GYTxOutRef)
addRefScript addr script = do
    let script' = validatorToScript script
    (Tx _ txBody, txId) <- sendSkeleton' (mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) { gyTxOutRefS = Just script' }) []
    -- now need to find utxo at given address which has the given reference script hm...
    let index = findIndex (\o -> Plutus2.txOutReferenceScript o == Just (scriptPlutusHash script')) (Fork.txOutputs txBody)
    return $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< index

-- | Expect the transaction building to fail with a 'BalancingErrorInsufficientFunds' error
expectInsufficientFunds :: Wallet -> GYTxSkeleton v -> Run ()
expectInsufficientFunds w skeleton = do
    m <- runWallet w $ catchError (Nothing <$ sendSkeleton skeleton) (return . Just)
    case m of
        Nothing       -> error "impossible case"
        Just Nothing  -> logError "expected transaction to fail, but it didn't"
        Just (Just e) -> case insufficientFunds e of
            Nothing -> logError $ "expected transaction to fail because of insufficientFunds, but it failed for another reason: " <> show e
            Just v  -> logInfo $ printf "transaction failed as expected due to insufficient funds: %s" v
  where
    insufficientFunds :: GYTxMonadException -> Maybe GYValue
    insufficientFunds (GYApplicationException e) = case cast e of
        Just (BuildTxBalancingError (BalancingErrorInsufficientFunds v)) -> Just v
        _                                                                -> Nothing
    insufficientFunds _                          = Nothing

-- | Adds an input (whose datum we'll refer later) and returns the reference to it.
addRefInput:: Bool       -- ^ Whether to inline this datum?
           -> GYAddress  -- ^ Where to place this output?
           -> GYDatum    -- ^ Our datum.
           -> GYTxMonadRun (Maybe GYTxOutRef)
addRefInput toInline addr dat = do
  (Tx _ txBody, txId) <- sendSkeleton' (mustHaveOutput $ GYTxOut addr mempty (Just (dat, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing) []
  liftRun $ logInfo $ printf "Added reference input with txId %s" txId
  outputsWithResolvedDatums <- mapM (resolveDatumFromPlutusOutput . Plutus2.txOutDatum ) (Fork.txOutputs txBody)
  let mIndex = findIndex (\d -> Just dat == d) outputsWithResolvedDatums
  return $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< mIndex

resolveDatumFromPlutusOutput :: GYTxQueryMonad m => Plutus2.OutputDatum -> m (Maybe GYDatum)
resolveDatumFromPlutusOutput (Plutus2.OutputDatum d)      = return $ Just $ datumFromPlutus d
resolveDatumFromPlutusOutput (Plutus2.OutputDatumHash dh) = lookupDatum $ unsafeDatumHashFromPlutus dh
resolveDatumFromPlutusOutput Plutus2.NoOutputDatum        = return Nothing

{- | Abstraction for explicitly building a Value representing the fees of a
     transaction.
-}
feesFromLovelace :: Integer -> GYValue
feesFromLovelace = valueFromLovelace

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- | Pattern to create pairs easily.
pattern (:=) :: x -> y -> (x, y)
pattern (:=) x y = (x, y)

infix 0 :=

-------------------------------------------------------------------------------
-- Preset StdGen
-------------------------------------------------------------------------------

pureGen :: StdGen
pureGen = mkStdGen 42
