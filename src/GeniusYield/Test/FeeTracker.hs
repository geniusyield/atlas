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
  withWalletBalancesCheckSimpleIgnoreMinDepFor
) where

import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Data.Foldable (foldMap')
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as M
import           Data.Monoid
import qualified Data.Set                   as S
import qualified Data.Text                  as T

import           GeniusYield.HTTP.Errors    (someBackendError)
import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types

type FeesLovelace = Sum Integer
type MinAdaLovelace = Sum Integer

-- | Extra lovelace consumed by tx fees and utxo min ada deposits for the transactions submitted by a user.
data UserExtraLovelace = UserExtraLovelace { uelFees :: FeesLovelace, uelMinAda :: MinAdaLovelace }
  deriving stock (Eq, Ord, Show)

instance Semigroup UserExtraLovelace where
    UserExtraLovelace a b <> UserExtraLovelace x y = UserExtraLovelace (a <> x) (b <> y)

instance Monoid UserExtraLovelace where
    mempty = UserExtraLovelace mempty mempty

-- | Track extra lovelace per transaction and submitted transactions. Only the submitted transactions' extra
-- lovelace is considered in the end.
data FeeTrackerState = FeeTrackerState { feesPerTx :: !(Map GYTxId UserExtraLovelace), submittedTxIds :: ![GYTxId] }
  deriving stock (Eq, Ord, Show)

instance Semigroup FeeTrackerState where
    FeeTrackerState fees txIds <> FeeTrackerState fees' txIds' = FeeTrackerState (M.unionWith (<>) fees fees') (txIds <> txIds')

instance Monoid FeeTrackerState where
    mempty = FeeTrackerState mempty mempty

insertFeesPerTx :: GYTxId -> UserExtraLovelace -> FeeTrackerState -> FeeTrackerState
insertFeesPerTx txId extraLovelace st = st { feesPerTx = M.insert txId extraLovelace $ feesPerTx st }

addSubmittedTx :: GYTxId -> FeeTrackerState -> FeeTrackerState
addSubmittedTx txId st = st { submittedTxIds = txId : submittedTxIds st }

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
    userAddress <- ownChangeAddress
    res <- ftLift $ f skeletons
    let helpers txBodies = forM_ (zip skeletons (NE.toList txBodies)) (helper userAddress)
    case res of
        GYTxBuildSuccess          txBodies -> helpers txBodies
        GYTxBuildPartialSuccess _ txBodies -> helpers txBodies
        _ -> pure ()
    pure res
  where

    helper userAddress (skeleton, txBody) = do
        let txId = txBodyTxId txBody
            -- Actual outputs with their blueprints (counterpart from skeleton)
            -- NOTE: This relies on proper ordering. 'txBodyUTxOs txBody' is expected to have the same order
            -- as the outputs in the skeleton. The extra balancing outputs at the end of the list of 'txBodyUTxOs txBody'
            -- should be truncated by 'zip'.
            outsWithBlueprint = zip (gytxOuts skeleton) . utxosToList $ txBodyUTxOs txBody
        modify' . insertFeesPerTx txId $ UserExtraLovelace
            { uelFees    = Sum $ txBodyFee txBody
            , uelMinAda  = Sum . flip valueAssetClass GYLovelace $
                foldMap'
                (\(blueprint, actual) ->
                    -- If this additional ada is coming back to one's own self, we need not account for it.
                    if gyTxOutAddress blueprint == userAddress then mempty
                    else utxoValue actual `valueMinus` gyTxOutValue blueprint
                )
                outsWithBlueprint
            }

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

-- | Override transaction submitting code of the inner monad to track submitted transaction ids.
instance GYTxMonad m => GYTxMonad (FeeTracker m) where
    signTxBody = ftLift . signTxBody
    signTxBodyWithStake = ftLift . signTxBodyWithStake
    submitTx tx = do
        txId <- ftLift $ submitTx tx
        modify $ addSubmittedTx txId
        pure txId
    awaitTxConfirmed' p = ftLift . awaitTxConfirmed' p

-- | A wrapper around 'GYTxGameMonad' that uses 'FeeTracker' as its 'GYTxMonad' to track extra lovelaces per transaction.
newtype FeeTrackerGame m a = FeeTrackerGame (Map GYAddress FeeTrackerState -> m (a, Map GYAddress FeeTrackerState))
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (Map GYAddress FeeTrackerState)
           , MonadRandom
           , GYTxQueryMonad
           , GYTxSpecialQueryMonad
           )
  via StateT (Map GYAddress FeeTrackerState) m

-- The context cannot be inferred since it contains non-type variables (i.e 'GYTxMonadException')
-- Must use standalone deriving with explicit context.
deriving
  via StateT (Map GYAddress FeeTrackerState) m
  instance MonadError GYTxMonadException m => MonadError GYTxMonadException (FeeTrackerGame m)

evalFtg :: Functor f => FeeTrackerGame f b -> f b
evalFtg (FeeTrackerGame act) = fst <$> act mempty

-- | Convert 'FeeTrackerState' to the effective extra lovelace map per user. Filtering out irrelevant transactions (not submitted).
walletExtraLovelace :: Map GYAddress FeeTrackerState -> Map GYAddress UserExtraLovelace
walletExtraLovelace m = M.map (\FeeTrackerState {feesPerTx} -> foldMap snd . filter ((`S.member` validTxIds) . fst) $ M.assocs feesPerTx) m
  where
    validTxIds = S.fromList . concatMap submittedTxIds $ M.elems m

-- | Perform a special action supported by the specific wrapped monad instance by lifting it to 'FeeTrackerGame'.
ftgLift :: Functor m => m a -> FeeTrackerGame m a
ftgLift act = FeeTrackerGame $ \s -> (, s) <$> act

instance GYTxGameMonad m => GYTxGameMonad (FeeTrackerGame m) where
    type TxMonadOf (FeeTrackerGame m) = FeeTracker (TxMonadOf m)
    asUser u (FeeTracker act) = FeeTrackerGame $ \s -> do
        (a, innerS) <- asUser u $ act mempty
        pure (a, M.insertWith (<>) (userChangeAddress u) innerS s)
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
    walletExtraLovelaceMap <- gets walletExtraLovelace
    bs' <- mapM (queryBalances . userAddresses' . fst) wallValueDiffs

    forM_ (zip3 wallValueDiffs bs' bs) $
        \((w, v), b', b) ->
        let addr = userChangeAddress w
            newBalance = case M.lookup addr walletExtraLovelaceMap of
                Nothing -> b'
                Just UserExtraLovelace {uelFees, uelMinAda} -> b' <> valueFromLovelace (getSum $ uelFees <> if w `S.member` ignoreMinDepFor then mempty else uelMinAda)
            diff = newBalance `valueMinus` b
        in unless (diff == v) . throwAppError . someBackendError . T.pack $
            printf "Wallet: %s. Old balance: %s. New balance: %s. New balance after adding extra lovelaces %s. Expected balance difference of %s, but the actual difference was %s" addr b b' newBalance v diff
    pure a
