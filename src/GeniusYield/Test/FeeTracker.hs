{-|
Module      : GeniusYield.Test.FeeTracker
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

module GeniusYield.Test.FeeTracker (
  FeeTrackerGame,
  FeeTracker,
  ftgLift,
  ftLift,
  withWalletBalancesCheckSimple,
  withWalletBalancesCheckSimpleIgnoreMinDepFor,
  withoutFeeTracking
) where

import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.Foldable              (foldMap')
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as M
import           Data.Monoid
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LTE

import qualified Data.Aeson                 as Aeson

import           GeniusYield.HTTP.Errors    (someBackendError)
import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types

type FeesLovelace = Sum Integer
type MinAdaLovelace = Sum Integer

-- | Extra lovelaces that were gained or lost by a user which a smart contract need not be expecting.
data UserExtraLovelace = UserExtraLovelace
    { uelFees   :: !FeesLovelace
    -- ^ Lovelaces lost to fees.
    , uelMinAda :: !MinAdaLovelace
    -- ^ Lovelaces lost to min ada deposit(s).
    -- Also takes into account any min ada deposit _gained_ from utxo(s).
    }
  deriving stock (Eq, Ord, Show)

instance Semigroup UserExtraLovelace where
    UserExtraLovelace a b <> UserExtraLovelace x y = UserExtraLovelace (a <> x) (b <> y)

instance Monoid UserExtraLovelace where
    mempty = UserExtraLovelace mempty mempty

-- | Track extra lovelace per user.
-- Note: This does the tracking during transaction building.
-- If you do not wish to submit said transaction, you should not have it tracked.
-- Use 'withoutFeeTracking' etc in those cases.
newtype FeeTrackerState = FeeTrackerState { feesPerUser :: Map GYPubKeyHash UserExtraLovelace }
  deriving stock (Eq, Ord, Show)

instance Semigroup FeeTrackerState where
    FeeTrackerState fees <> FeeTrackerState fees' = FeeTrackerState (M.unionWith (<>) fees fees')

instance Monoid FeeTrackerState where
    mempty = FeeTrackerState mempty

stSingleton :: GYPubKeyHash -> UserExtraLovelace -> FeeTrackerState
stSingleton k = FeeTrackerState . M.singleton k

-- | A wrapper around 'GYTxMonad' that "injects" code around transaction building and submitting to track fees.
newtype FeeTracker m a = FeeTracker (FeeTrackerState -> m (a, FeeTrackerState))
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState FeeTrackerState
           , MonadRandom
           , GYTxQueryMonad
           , GYTxSpecialQueryMonad
           , GYTxUserQueryMonad
           , GYTxMonad
           )
  via StateT FeeTrackerState m

-- The context cannot be inferred since it contains non-type variables (i.e 'GYTxMonadException')
-- Must use standalone deriving with explicit context.
deriving
  via StateT FeeTrackerState m
  instance MonadError GYTxMonadException m => MonadError GYTxMonadException (FeeTracker m)

-- | Perform a special action supported by the specific wrapped monad instance by lifting it to 'FeeTracker'.
ftLift :: Functor m => m a -> FeeTracker m a
ftLift act = FeeTracker $ \s -> (, s) <$> act

-- | Override given transaction building function to track extra lovelace per transaction.
wrapBodyBuilder :: GYTxUserQueryMonad m => ([GYTxSkeleton v] -> m GYTxBuildResult) -> [GYTxSkeleton v] -> FeeTracker m GYTxBuildResult
wrapBodyBuilder f skeletons = do
    ownPkh <- ownChangeAddress >>= addressToPubKeyHash'
    res <- ftLift $ f skeletons
    let helpers txBodies = forM_ (zip skeletons (NE.toList txBodies)) (helper ownPkh)
    case res of
        GYTxBuildSuccess          txBodies -> helpers txBodies
        GYTxBuildPartialSuccess _ txBodies -> helpers txBodies
        _                                  -> pure ()
    pure res
  where
    helper ownPkh (skeleton, txBody) = do
            -- Actual outputs with their blueprints (counterpart from skeleton)
            -- NOTE: This relies on proper ordering. 'txBodyUTxOs txBody' is expected to have the same order
            -- as the outputs in the skeleton. The extra balancing outputs at the end of the list of 'txBodyUTxOs txBody'
            -- should be truncated by 'zip'.
        let outsWithBlueprint = zip (gytxOuts skeleton) . utxosToList $ txBodyUTxOs txBody
            feeExtraLovelace = stSingleton ownPkh mempty { uelFees = Sum $ txBodyFee txBody }
            depositsExtraLovelace = foldMap'
                (\(blueprint, actual) ->
                    let targetAddr = gyTxOutAddress blueprint
                        deposit = Sum . flip valueAssetClass GYLovelace $ utxoValue actual `valueMinus` gyTxOutValue blueprint
                        -- These two will cancel out if the ada is going to own address.
                        ownLostDeposit = stSingleton ownPkh mempty { uelMinAda = deposit }
                        otherGainedDeposit = maybe mempty (`stSingleton` mempty { uelMinAda = negate deposit }) $ addressToPubKeyHash targetAddr
                    in ownLostDeposit <> otherGainedDeposit
                )
                outsWithBlueprint
        modify' (\prev -> prev <> feeExtraLovelace <> depositsExtraLovelace)

-- | Override transaction building code of the inner monad to track extra lovelace per transaction.
instance GYTxBuilderMonad m => GYTxBuilderMonad (FeeTracker m) where
    type TxBuilderStrategy (FeeTracker m) = TxBuilderStrategy m
    buildTxBodyWithStrategy strat skeleton = do
        res <- wrapBodyBuilder (\x -> GYTxBuildSuccess . NE.singleton <$> buildTxBodyWithStrategy @m strat (head x)) [skeleton]
        case res of
            GYTxBuildSuccess bodies -> pure $ NE.head bodies
            _ -> error "FeeTracker.buildTxBodyWithStrategy: Absurd"
    buildTxBodyParallelWithStrategy strat = wrapBodyBuilder $ buildTxBodyParallelWithStrategy strat
    buildTxBodyChainingWithStrategy strat = wrapBodyBuilder $ buildTxBodyChainingWithStrategy strat

-- | Run an action and ignore any tracked fees.
-- Useful for building a tx body without the intent to submit it later. Thereby ignoring all the tracked fees
-- from that txbody that won't actually take effect in the wallet (since it won't be submitted).
withoutFeeTracking :: Monad m => FeeTracker m a -> FeeTracker m a
withoutFeeTracking act = do
    s <- get
    a <- act
    put s
    pure a

-- | A wrapper around 'GYTxGameMonad' that uses 'FeeTracker' as its 'GYTxMonad' to track extra lovelaces per transaction.
newtype FeeTrackerGame m a = FeeTrackerGame (FeeTrackerState -> m (a, FeeTrackerState))
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState FeeTrackerState
           , MonadRandom
           , GYTxQueryMonad
           , GYTxSpecialQueryMonad
           )
  via StateT FeeTrackerState m

-- The context cannot be inferred since it contains non-type variables (i.e 'GYTxMonadException')
-- Must use standalone deriving with explicit context.
deriving
  via StateT FeeTrackerState m
  instance MonadError GYTxMonadException m => MonadError GYTxMonadException (FeeTrackerGame m)

evalFtg :: Functor f => FeeTrackerGame f b -> f b
evalFtg (FeeTrackerGame act) = fst <$> act mempty

-- | Perform a special action supported by the specific wrapped monad instance by lifting it to 'FeeTrackerGame'.
ftgLift :: Functor m => m a -> FeeTrackerGame m a
ftgLift act = FeeTrackerGame $ \s -> (, s) <$> act

instance GYTxGameMonad m => GYTxGameMonad (FeeTrackerGame m) where
    type TxMonadOf (FeeTrackerGame m) = FeeTracker (TxMonadOf m)
    asUser u (FeeTracker act) = FeeTrackerGame $ asUser u . act
    waitUntilSlot = ftgLift . waitUntilSlot
    waitForNextBlock = ftgLift waitForNextBlock

{- Note [Proper GYTxMonad overriding with FeeTracker]

It's important for the 'GYTxMonad' code block to be _instantiated_ as 'FeeTracker m'
for the appropriate code overriding to take place. Specifically, if you have a code block
of type 'GYTxMonad', which then gets type inferred and instantiated to be 'GYTxMonadClb' for
example, and then said 'GYTxMonadClb' is wrapped to obtain a 'FeeTracker GYTxMonadClb', no
overriding has taken place.

Instead, that codeblock needs to be type inferred and instantiated to be 'FeeTracker GYTxMonadClb'.
At the end of the day, the code block is universally quantified. And therefore, its instantiation
is chosen at the call site. And the methods to fire are also chosen at the same site.

This is why 'withWalletBalancesCheckSimple' takes a 'FeeTrackerGame m' as its input. The idea is that,
when this function is applied to a code block of type 'GYTxGameMonad', the type is then instantiated
as 'FeeTrackerGame'. With the help of our injective type family 'TxMonadOf', all the inner 'GYTxMonad's
will then be instantiated as 'FeeTracker m', and all the proper methods will now fire. The 'm' can be decided
at the very end.

In contrast, if 'withWalletBalancesCheckSimple' took a normal 'GYTxGameMonad m => m', it might end up being inferred
and instantiated as 'GYTxMonadClb' (for example), which will then be wrapped with the 'FeeTrackerGame' constructor
within the 'withWalletBalancesCheckSimple' function body. But that will achieve no overriding, the methods have already
been chosen!
-}

{- | Computes a 'GYTxMonadClb' action, checking that the 'Wallet' balances
        change according to the input list. This is a simplified version of `withWalletBalancesCheck` where the input list need not consider lovelaces required for fees & to satisfy the min ada requirements as these are added automatically. It is therefore recommended to use this function over `withWalletBalancesCheck` to avoid hardcoding the lovelaces required for fees & min ada constraints.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheckSimple :: GYTxGameMonad m => [(User, GYValue)] -> FeeTrackerGame m a -> m a
withWalletBalancesCheckSimple wallValueDiffs = withWalletBalancesCheckSimpleIgnoreMinDepFor wallValueDiffs mempty

-- | Variant of `withWalletBalancesCheckSimple` that only accounts for transaction fees and not minimum ada deposits.
withWalletBalancesCheckSimpleIgnoreMinDepFor :: GYTxGameMonad m => [(User, GYValue)] -> Set User -> FeeTrackerGame m a -> m a
withWalletBalancesCheckSimpleIgnoreMinDepFor wallValueDiffs ignoreMinDepFor m = evalFtg $ do
    bs <- mapM (queryBalances . userAddresses' . fst) wallValueDiffs
    a <- m
    walletExtraLovelaceMap <- gets feesPerUser
    bs' <- mapM (queryBalances . userAddresses' . fst) wallValueDiffs

    forM_ (zip3 wallValueDiffs bs' bs) $
        \((w, v), b', b) ->
        let pkh = userPkh w
            newBalance = case M.lookup pkh walletExtraLovelaceMap of
                Nothing -> b'
                Just UserExtraLovelace {uelFees, uelMinAda} -> b' <> valueFromLovelace (getSum $ uelFees <> if w `S.member` ignoreMinDepFor then mempty else uelMinAda)
            diff = newBalance `valueMinus` b
        in unless (diff == v) . throwAppError . someBackendError . T.pack $
            printf
                (  "Wallet PKH: %s.\n"
                ++ "Old balance: %s.\n"
                ++ "New balance: %s.\n"
                ++ "New balance after adding extra lovelaces %s.\n"
                ++ "    Expected balance difference of: %s\n"
                ++ "    But the actual difference was: %s"
                )
                (encodeJsonText pkh)
                (encodeJsonText b)
                (encodeJsonText b')
                (encodeJsonText newBalance)
                (encodeJsonText v)
                (encodeJsonText diff)
    pure a
  where
    encodeJsonText :: ToJSON a => a -> Text
    encodeJsonText = LT.toStrict . LTE.decodeUtf8 . Aeson.encode
