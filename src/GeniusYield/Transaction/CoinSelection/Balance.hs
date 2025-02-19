{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

{- |
Module      : GeniusYield.Transaction.CoinSelection.Balance
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Largely a simplified version of [@Cardano.CoinSelection.Balance@](https://github.com/cardano-foundation/cardano-wallet/blob/master/lib/coin-selection/lib/Cardano/CoinSelection/Balance.hs).
-}
module GeniusYield.Transaction.CoinSelection.Balance (

) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Conway.Core (eraProtVerHigh)
import Control.Monad.Extra (
  andM,
  (<=<),
 )
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (
  ExceptT (ExceptT),
  except,
 )
import Data.Bifunctor qualified
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable qualified as F
import Data.Generics.Internal.VL.Lens (
  view,
 )
import Data.List qualified as L
import Data.List.NonEmpty (
  NonEmpty (..),
 )
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Semigroup (mtimesDefault)
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text.Class (
  ToText (toText),
  fromText,
 )
import GHC.IsList (fromList)
import GeniusYield.Imports
import GeniusYield.Transaction.CoinSelection.UTxOIndex (SelectionFilter (..), UTxOIndex)
import GeniusYield.Transaction.CoinSelection.UTxOIndex qualified as UTxOIndex
import GeniusYield.Transaction.CoinSelection.UTxOSelection (IsUTxOSelection, UTxOSelection, UTxOSelectionNonEmpty)
import GeniusYield.Transaction.CoinSelection.UTxOSelection qualified as UTxOSelection
import GeniusYield.Transaction.Common
import GeniusYield.Types
import GeniusYield.Utils

type UTxO = GYTxOutRef

data ValueSizeAssessment
  = -- | Indicates that the size of a value does not exceed the maximum
    -- size that can be included in a transaction output.
    ValueSizeWithinLimit
  | -- | Indicates that the size of a value exceeds the maximum size
    -- that can be included in a transaction output.
    ValueSizeExceedsLimit
  deriving (Eq, Generic, Show)

-- TODO: Having this 'v' lingering around is nuisance.

-- | Specifies all constraints required for coin selection.
data SelectionConstraints v = SelectionConstraints
  { tokenBundleSizeAssessor ::
      GYValue ->
      ValueSizeAssessment
  -- ^ Assesses the size of a value relative to the upper limit of
  -- what can be included in a transaction output.
  -- TODO: Make it just a Bool?
  , computeMinimumAdaQuantity ::
      GYAddress ->
      GYValue ->
      Natural
  -- ^ Computes the minimum ada quantity required for a given output.
  , -- TODO: Can we make it GYTxOut something?

    computeMinimumCost ::
      SelectionSkeleton ->
      Natural
  -- ^ Computes the minimum cost of a given selection skeleton.
  -- TODO: This can be cleverly used to not make use of logarithmic overestimation function. To study if it's only for fees or also for min deposits, if min deposit then which one?
  -- TODO: Whether I can be precise here also depends upon how many times is this called. Our initial implementation had logarithmic iteration.
  , {- FIXME: Remove this comment.
    , maximumLengthChangeAddress
        :: Address ctx
    -}
    changeAddress ::
      GYAddress
  , maximumOutputAdaQuantity ::
      Natural
  -- ^ Specifies the largest ada quantity that can appear in the token
  -- bundle of an output.
  , maximumOutputTokenQuantity ::
      Natural
  -- ^ Specifies the largest non-ada quantity that can appear in the
  -- token bundle of an output.
  -- FIXME: Delete this comment.
  , nullAddress ::
      GYAddress
      -- TODO: This is supposed to be an empty bytestring, not a complete gyaddress. Study if it creates an issue and whether it can be omitted.
  }
  deriving Generic

-- | Specifies all parameters that are specific to a given selection.
data SelectionParams f = SelectionParams
  { outputsToCover ::
      !(f (GYAddress, GYValue))
  -- ^ The complete set of outputs to be covered.
  -- TODO: Shall I be changing it's representation to say GYTxOut v? Or just GYValue?
  , utxoAvailable ::
      !(UTxOSelection UTxO)
  -- ^ Specifies a set of UTxOs that are available for selection as
  -- inputs and optionally, a subset that has already been selected.
  --
  -- Further entries from this set will be selected to cover any deficit.
  , extraCoinSource ::
      !Natural
  -- ^ An extra source of ada.
  , extraCoinSink ::
      !Natural
  -- ^ An extra sink for ada.
  , assetsToMint ::
      !GYValue
  -- ^ Assets to mint: these provide input value to a transaction.
  --
  -- By minting tokens, we generally decrease the burden of the selection
  -- algorithm, allowing it to select fewer UTxO entries in order to
  -- cover the required outputs.
  , assetsToBurn ::
      !GYValue
  -- ^ Assets to burn: these consume output value from a transaction.
  --
  -- By burning tokens, we generally increase the burden of the selection
  -- algorithm, requiring it to select more UTxO entries in order to
  -- cover the burn.
  --
  -- Note that it contains positive entries only.
  , selectionStrategy ::
      SelectionStrategy
  -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
  }
  deriving Generic

{- | Indicates a choice of selection strategy.

A 'SelectionStrategy' determines __how much__ of each asset the selection
algorithm will attempt to select from the available UTxO set, relative to
the minimum amount necessary to make the selection balance.

The default 'SelectionStrategy' is 'SelectionStrategyOptimal', which when
specified will cause the selection algorithm to attempt to select around
__/twice/__ the minimum possible amount of each asset from the available
UTxO set, making it possible to generate change outputs that are roughly
the same sizes and shapes as the user-specified outputs.

Specifying 'SelectionStrategyMinimal' will cause the selection algorithm to
only select __just enough__ of each asset from the available UTxO set to
meet the minimum amount. The selection process will terminate as soon as
the minimum amount of each asset is covered.

The "optimal" strategy is recommended for most situations, as using this
strategy will help to ensure that a wallet's UTxO distribution can evolve
over time to resemble the typical distribution of payments made by the
wallet owner.  This increases the likelihood that future selections will
succeed, and lowers the amortized cost of future transactions.

The "minimal" strategy is recommended only for situations where it is not
possible to create a selection with the "optimal" strategy. It is advised to
use this strategy only when necessary, as it increases the likelihood of
generating change outputs that are much smaller than user-specified outputs.
If this strategy is used regularly, the UTxO set can evolve to a state where
the distribution no longer resembles the typical distribution of payments
made by the user. This increases the likelihood that future selections will
not succeed, and increases the amortized cost of future transactions.
-}
data SelectionStrategy
  = SelectionStrategyMinimal
  | SelectionStrategyOptimal
  deriving (Bounded, Enum, Eq, Show)

-- TODO: Rename it appropriately and have other strategy type encapsulate it.

{- | Indicates whether the balance of available UTxO entries is sufficient.

See 'computeUTxOBalanceSufficiency'.
-}
data UTxOBalanceSufficiency
  = -- | Indicates that the UTxO balance is sufficient.
    UTxOBalanceSufficient
  | -- | Indicates that the UTxO balance is insufficient.
    UTxOBalanceInsufficient
  deriving (Eq, Show)

{- | Gives more information about UTxO balance sufficiency.

See 'computeUTxOBalanceSufficiencyInfo'.
-}
data UTxOBalanceSufficiencyInfo = UTxOBalanceSufficiencyInfo
  { available :: GYValue
  -- ^ See 'computeUTxOBalanceAvailable'.
  , required :: GYValue
  -- ^ See 'computeUTxOBalanceRequired'.
  , difference :: GYValue
  -- ^ The difference between 'available' and 'required'.
  -- TODO: I need to be careful with how this difference is calculated. The thing is am I violating an invariant?
  , sufficiency :: UTxOBalanceSufficiency
  -- ^ Whether or not the balance is sufficient.
  }
  deriving (Eq, Generic, Show)

-- | Computes the balance of UTxO entries available for selection.
computeUTxOBalanceAvailable ::
  SelectionParams f -> GYValue
computeUTxOBalanceAvailable =
  UTxOSelection.availableBalance . view #utxoAvailable

-- | Computes the balance of UTxO entries required to be selected.
computeUTxOBalanceRequired ::
  Foldable f => SelectionParams f -> GYValue
computeUTxOBalanceRequired = fst . computeDeficitInOut

computeBalanceInOut ::
  Foldable f => SelectionParams f -> (GYValue, GYValue)
computeBalanceInOut params =
  (balanceIn, balanceOut)
 where
  balanceIn =
    view #assetsToMint params
      <> valueFromLovelace (fromIntegral (view #extraCoinSource params))
  balanceOut =
    view #assetsToBurn params
      <> valueFromLovelace (fromIntegral (view #extraCoinSink params))
      <> F.foldMap snd (view #outputsToCover params)

computeDeficitInOut ::
  Foldable f => SelectionParams f -> (GYValue, GYValue)
computeDeficitInOut params =
  (deficitIn, deficitOut)
 where
  deficitIn =
    balanceOut `valueMonus` balanceIn
  deficitOut =
    balanceIn `valueMonus` balanceOut
  (balanceIn, balanceOut) =
    computeBalanceInOut params

{- | Computes the UTxO balance sufficiency.

See 'UTxOBalanceSufficiency'.
-}
computeUTxOBalanceSufficiency ::
  Foldable f => SelectionParams f -> UTxOBalanceSufficiency
computeUTxOBalanceSufficiency = sufficiency . computeUTxOBalanceSufficiencyInfo

{- | Computes information about the UTxO balance sufficiency.

See 'UTxOBalanceSufficiencyInfo'.
-}
computeUTxOBalanceSufficiencyInfo ::
  Foldable f => SelectionParams f -> UTxOBalanceSufficiencyInfo
computeUTxOBalanceSufficiencyInfo params =
  UTxOBalanceSufficiencyInfo {available, required, difference, sufficiency}
 where
  available = computeUTxOBalanceAvailable params
  required = computeUTxOBalanceRequired params
  sufficiency =
    if required `valueLessOrEqual` available
      then UTxOBalanceSufficient
      else UTxOBalanceInsufficient
  difference =
    if sufficiency == UTxOBalanceSufficient
      then available `valueMonus` required
      else required `valueMonus` available

{- | Indicates whether or not the UTxO balance is sufficient.

The balance of available UTxO entries is sufficient if (and only if) it
is greater than or equal to the required balance.
-}
isUTxOBalanceSufficient ::
  Foldable f => SelectionParams f -> Bool
isUTxOBalanceSufficient params =
  case computeUTxOBalanceSufficiency params of
    UTxOBalanceSufficient -> True
    UTxOBalanceInsufficient -> False

-- TODO: I likely would need to either get rid of this selection skeleton or make it useful.

{- | A skeleton selection that can be used to estimate the cost of a final
  selection.

Change outputs are deliberately stripped of their asset quantities, as the
fee estimation function must be agnostic to the magnitudes of these
quantities.

Increasing or decreasing the quantity of a particular asset in a change
output must not change the estimated cost of a selection.
-}
data SelectionSkeleton = SelectionSkeleton
  { skeletonInputCount ::
      !Int
  , skeletonOutputs ::
      ![(GYAddress, GYValue)]
  , skeletonChange ::
      ![Set GYAssetClass]
  }

-- | The result of performing a successful selection.
data SelectionResult f = SelectionResult
  { inputsSelected ::
      !(NonEmpty (UTxO, GYValue))
  -- ^ A (non-empty) list of inputs selected from 'utxoAvailable'.
  , extraCoinSource ::
      !Natural
  -- ^ An extra source of ada.
  , extraCoinSink ::
      !Natural
  -- ^ An extra sink for ada.
  , outputsCovered ::
      !(f (GYAddress, GYValue))
  -- ^ A list of outputs covered.
  , changeGenerated ::
      ![GYValue]
  -- ^ A list of generated change outputs.
  , assetsToMint ::
      !GYValue
  -- ^ The assets to mint.
  , assetsToBurn ::
      !GYValue
  -- ^ The assets to burn.
  }
  deriving Generic

{- | Indicates the difference between total input value and total output value
  of a 'SelectionResult'.

There are two possibilities:

 - 'SelectionSurplus'

   Indicates a surplus, when the total input value is greater than or equal
   to the total output value.

 - 'SelectionDeficit'

   Indicates a deficit, when the total input value is NOT greater than or
   equal to the total output value.
-}
data SelectionDelta a
  = SelectionSurplus a
  | SelectionDeficit a
  deriving (Eq, Functor, Show)

{- | Calculates the selection delta for all assets.

See 'SelectionDelta'.
-}
selectionDeltaAllAssets ::
  Foldable f => SelectionResult f -> SelectionDelta GYValue
selectionDeltaAllAssets result
  | balanceOut `valueLessOrEqual` balanceIn =
      SelectionSurplus $ balanceIn `valueMonus` balanceOut
  | otherwise =
      SelectionDeficit $ balanceOut `valueMonus` balanceIn
 where
  balanceIn =
    assetsToMint
      <> valueFromLovelace (fromIntegral extraCoinSource)
      <> F.foldMap snd inputsSelected
  balanceOut =
    assetsToBurn
      <> valueFromLovelace (fromIntegral extraCoinSink)
      <> F.foldMap snd outputsCovered
      <> F.fold changeGenerated
  SelectionResult
    { assetsToMint
    , assetsToBurn
    , extraCoinSource
    , extraCoinSink
    , inputsSelected
    , outputsCovered
    , changeGenerated
    } = result

{- | Calculates the ada selection delta.

See 'SelectionDelta'.
-}
selectionDeltaCoin ::
  Foldable f => SelectionResult f -> SelectionDelta Natural
selectionDeltaCoin = fmap (fromIntegral . (`valueAssetClass` GYLovelace)) . selectionDeltaAllAssets

-- TODO: Was this above function ever used?

-- | Indicates whether or not a selection result has a valid surplus.
selectionHasValidSurplus ::
  Foldable f => SelectionConstraints v -> SelectionResult f -> Bool
selectionHasValidSurplus constraints selection =
  case selectionDeltaAllAssets selection of
    SelectionSurplus s -> surplusIsValid s
    SelectionDeficit _ -> False
 where
  surplusIsValid :: GYValue -> Bool
  surplusIsValid =
    andM
      [ surplusHasNoNonAdaAssets
      , surplusNotBelowMinimumCost
      , surplusNotAboveMaximumCost
      ]

  -- None of the non-ada assets can have a surplus.
  surplusHasNoNonAdaAssets :: GYValue -> Bool
  surplusHasNoNonAdaAssets (valueNonAda -> nonAdaSurplus) =
    nonAdaSurplus == mempty

  -- The surplus must not be less than the minimum cost.
  surplusNotBelowMinimumCost :: GYValue -> Bool
  surplusNotBelowMinimumCost (valueAda -> adaSurplus) =
    adaSurplus >= fromIntegral (selectionMinimumCost constraints selection)

  -- The surplus must not be greater than the maximum cost.
  surplusNotAboveMaximumCost :: GYValue -> Bool
  surplusNotAboveMaximumCost (valueAda -> adaSurplus) =
    adaSurplus <= fromIntegral (selectionMaximumCost constraints selection)

{- | Calculates the ada selection surplus, assuming there is a surplus.

If there is a surplus, then this function returns that surplus.
If there is a deficit, then this function returns zero.

Use 'selectionDeltaCoin' if you wish to handle the case where there is
a deficit.
-}
selectionSurplusCoin :: Foldable f => SelectionResult f -> Natural
selectionSurplusCoin result =
  case selectionDeltaCoin result of
    SelectionSurplus surplus -> surplus
    SelectionDeficit _ -> 0

-- | Converts a selection into a skeleton.
selectionSkeleton ::
  Foldable f => SelectionResult f -> SelectionSkeleton
selectionSkeleton s =
  SelectionSkeleton
    { skeletonInputCount = F.length (view #inputsSelected s)
    , skeletonOutputs = F.toList (view #outputsCovered s)
    , skeletonChange = valueAssets <$> view #changeGenerated s
    }

-- | Computes the minimum required cost of a selection.
selectionMinimumCost ::
  Foldable f => SelectionConstraints v -> SelectionResult f -> Natural
selectionMinimumCost c = view #computeMinimumCost c . selectionSkeleton

{- | Computes the maximum acceptable cost of a selection.

This function acts as a safety limit to ensure that fees of selections
produced by 'performSelection' are not excessively high.

Ideally, we'd always be able to generate selections with fees that are
precisely equal to 'selectionMinimumCost'. However, in some situations
it may be necessary to exceed this cost very slightly.

This function provides a conservative upper bound to a selection cost
that we can reference from within property tests.

See 'selectionHasValidSurplus'.
-}
selectionMaximumCost ::
  Foldable f => SelectionConstraints v -> SelectionResult f -> Natural
selectionMaximumCost c sr = let mc = selectionMinimumCost c sr in mc + mc

-- | Represents the set of errors that may occur while performing a selection.
data SelectionBalanceError
  = BalanceInsufficient
      BalanceInsufficientError
  | UnableToConstructChange
      UnableToConstructChangeError
  | EmptyUTxO
  deriving (Generic, Eq, Show)

{- | Indicates that the balance of available UTxO entries is insufficient to
  cover the balance required.

See 'computeUTxOBalanceSufficiency'.
-}
data BalanceInsufficientError = BalanceInsufficientError
  { utxoBalanceAvailable ::
      !GYValue
  -- ^ The balance of 'utxoAvailable'.
  , utxoBalanceRequired ::
      !GYValue
  -- ^ The balance of 'outputsToCover'.
  , utxoBalanceShortfall ::
      !GYValue
  -- ^ The shortfall between 'utxoBalanceAvailable' and
  -- 'utxoBalanceRequired'.
  --
  -- Equal to the 'valueMonus' of 'utxoBalanceAvailable' from
  -- 'utxoBalanceRequired'.
  }
  deriving (Generic, Eq, Show)

mkBalanceInsufficientError ::
  GYValue -> GYValue -> BalanceInsufficientError
mkBalanceInsufficientError utxoBalanceAvailable utxoBalanceRequired =
  BalanceInsufficientError
    { utxoBalanceAvailable
    , utxoBalanceRequired
    , utxoBalanceShortfall
    }
 where
  utxoBalanceShortfall =
    utxoBalanceRequired `valueMonus` utxoBalanceAvailable

data UnableToConstructChangeError = UnableToConstructChangeError
  { requiredCost ::
      !Natural
  -- ^ The minimal required cost needed for the transaction to be
  -- considered valid. This does not include min Ada values.
  , shortfall ::
      !Natural
  -- ^ The additional coin quantity that would be required to cover the
  -- selection cost and minimum coin quantity of each change output.
  -- TODO: What's this "selection cost" that they mention here?
  }
  deriving (Generic, Eq, Show)

type PerformSelection m f v =
  SelectionConstraints v ->
  SelectionParams f ->
  m (Either SelectionBalanceError (SelectionResult f))

{- | Performs a coin selection and generates change values in one step.

Provided that 'isUTxOBalanceSufficient' returns 'True' for the given
selection criteria, this function guarantees to return a 'SelectionResult'
for which 'selectionHasValidSurplus' returns 'True'.
-}
performSelection ::
  forall m v.
  (HasCallStack, MonadRandom m) =>
  PerformSelection m [] v
performSelection = performSelectionEmpty performSelectionNonEmpty

{- | Transforms a coin selection function that requires a non-empty list of
  outputs into a function that accepts an empty list of outputs.

If the original list is already non-empty, this function does not alter the
parameters or the result in any way, such that:

   params == transformParams params
   result == transformResult result

If the original list is empty, this function:

  1. applies a balance-preserving transformation to the parameters, adding
     a single minimal ada-only output to act as a change generation target,
     such that:

         computeUTxOBalanceSufficiencyInfo params ==
         computeUTxOBalanceSufficiencyInfo (transformParams params)

  2. applies an inverse transformation to the result, removing the output,
     such that:

         selectionSurplus result ==
         selectionSurplus (transformResult result)

         selectionHasValidSurplus constraints result ==>
         selectionHasValidSurplus constraints (transformResult result)
-}
performSelectionEmpty ::
  forall m v.
  Functor m =>
  PerformSelection m NonEmpty v ->
  PerformSelection m [] v
performSelectionEmpty performSelectionFn constraints params =
  fmap transformResult
    <$> performSelectionFn constraints (transformParams params)
 where
  transformParams ::
    SelectionParams [] ->
    SelectionParams NonEmpty
  transformParams p@SelectionParams {..} =
    p
      { extraCoinSource =
          transform (dummyCoin +) (const id) extraCoinSource
      , outputsToCover =
          transform (const (dummyOutput :| [])) const outputsToCover
      }

  transformResult ::
    SelectionResult NonEmpty ->
    SelectionResult []
  transformResult r@SelectionResult {..} =
    r
      { extraCoinSource =
          transform (\ecs -> if ecs >= dummyCoin then ecs - dummyCoin else 0) (const id) extraCoinSource
      , outputsCovered =
          transform (const []) (const . F.toList) outputsCovered
      }

  transform :: a -> (NonEmpty (GYAddress, GYValue) -> a) -> a
  transform x y = maybe x y $ NE.nonEmpty $ view #outputsToCover params

  -- A dummy output that is added before calling 'performSelectionNonEmpty'
  -- and removed immediately after selection is complete.
  --
  dummyOutput :: (GYAddress, GYValue)
  dummyOutput = (dummyAddress, valueFromLovelace $ fromIntegral dummyCoin)

  -- A dummy 'Address' value for the dummy output.
  --
  -- We can use a null address here, as 'performSelectionNonEmpty' does not
  -- verify the minimum ada quantities of user-specified outputs, and hence
  -- we do not need to provide a valid address.
  --
  -- Using a null address allows us to minimize any overestimation in cost
  -- resulting from the use of a dummy output.
  --
  -- TODO: How exactly does it compute to and to "which" cost?
  dummyAddress = nullAddress constraints

  -- A dummy 'Coin' value for the dummy output.
  --
  -- This value is chosen to be as small as possible in order to minimize
  -- any overestimation in cost resulting from the use of a dummy output.
  --
  -- However, we cannot choose a value of zero, since the change generation
  -- algorithm requires that the total ada balance of all outputs is
  -- non-zero, so instead we specify the smallest possible non-zero value.
  --
  dummyCoin :: Natural
  dummyCoin = 1

performSelectionNonEmpty ::
  forall m ctx.
  (HasCallStack, MonadRandom m) =>
  PerformSelection m NonEmpty ctx
performSelectionNonEmpty constraints params
  -- Is the total available UTXO balance sufficient?
  | not utxoBalanceSufficient =
      pure $
        Left $
          BalanceInsufficient $
            mkBalanceInsufficientError
              utxoBalanceAvailable
              utxoBalanceRequired
  | otherwise = do
      maybeSelection <-
        runSelectionNonEmpty
          RunSelectionParams
            { utxoAvailable
            , minimumBalance = utxoBalanceRequired
            , selectionStrategy
            }
      case maybeSelection of
        Nothing ->
          -- If it was not possible to select even a single UTxO, it must
          -- mean that there were no UTxOs available to select.
          pure $ Left EmptyUTxO
        Just selection ->
          -- If we have a non-empty selection of UTxOs, we know that the
          -- total balance of selected assets must be greater than or
          -- equal to the minimum required amount, as we have already
          -- ruled out the possibility that the available balance is
          -- insufficient.
          makeChangeRepeatedly selection
 where
  SelectionConstraints
    { tokenBundleSizeAssessor
    , computeMinimumAdaQuantity
    , computeMinimumCost
    , maximumOutputAdaQuantity
    , maximumOutputTokenQuantity
    , changeAddress
    } = constraints
  SelectionParams
    { outputsToCover
    , utxoAvailable
    , extraCoinSource
    , extraCoinSink
    , assetsToMint
    , assetsToBurn
    , selectionStrategy
    } = params

  utxoBalanceAvailable :: GYValue
  utxoBalanceAvailable = computeUTxOBalanceAvailable params

  utxoBalanceRequired :: GYValue
  utxoBalanceRequired = computeUTxOBalanceRequired params

  utxoBalanceSufficient :: Bool
  utxoBalanceSufficient = isUTxOBalanceSufficient params

  -- Given a UTxO index that corresponds to a valid selection covering
  -- 'outputsToCover', 'predictChange' yields a non-empty list of assets
  -- expected for change outputs.
  --
  -- There's a chicken-and-egg situation when it comes to calculating
  -- transaction fees. On the one hand, we need to know the shape of the
  -- final transaction to calculate its cost. But in order to construct the
  -- transaction, we need to know what its cost is.
  --
  -- So, in order to not duplicate the logic from 'makeChange', we first
  -- calculate a pre-selection considering the case where we have no fees to
  -- pay, and no minimum value. This is *guaranteed to succeed* and will
  -- yield a selection with change outputs in the final shape (modulo
  -- amounts).
  --
  -- The result of calling 'predictChange' with a valid input selection
  -- should satisfy:
  --
  --     length predictedChange === length outputsToCover
  --
  --     flat predictChange `isSubsetOf` assets selectedInputs
  --
  --     ∃ params. / isRight (performSelection params) =>
  --         Right predictedChange === assets <$> performSelection params
  --
  --     (That is, the predicted change is necessarily equal to the change
  --     assets of the final resulting selection).
  --
  predictChange :: UTxOSelectionNonEmpty UTxO -> [Set GYAssetClass]
  predictChange s =
    either
      (const $ invariantResultWithNoCost $ UTxOSelection.selectedIndex s)
      (fmap (\val -> GYLovelace `Set.delete` valueAssets val))
      ( makeChange
          MakeChangeCriteria
            { minCoinFor = noMinimumCoin
            , bundleSizeAssessor = tokenBundleSizeAssessor
            , requiredCost = noCost
            , extraCoinSource
            , extraCoinSink
            , inputBundles
            , outputBundles
            , assetsToMint
            , assetsToBurn
            , maximumOutputAdaQuantity
            , maximumOutputTokenQuantity
            }
      )
   where
    inputBundles = snd <$> UTxOSelection.selectedList s
    outputBundles = snd <$> outputsToCover

    noMinimumCoin :: GYValue -> Natural
    noMinimumCoin = const 0

    noCost :: Natural
    noCost = 0

  -- This function takes the given selection skeleton as a way to evaluate
  -- the cost of a final selection, and then calls 'makeChange' repeatedly
  -- until it succeeds.
  --
  -- Between each call, it selects an extra ada-only input to inject
  -- additional ada to construct change outputs.
  --
  -- Eventually it returns just a final selection, or 'Nothing' if no more
  -- ada-only inputs are available.
  --
  -- This function also takes a set of tokens that are to be burned, and
  -- hence although one or more inputs will be consumed for them, this
  -- function won't make associated outputs for them.
  --
  makeChangeRepeatedly ::
    UTxOSelectionNonEmpty UTxO ->
    m
      ( Either
          SelectionBalanceError
          (SelectionResult NonEmpty)
      )
  makeChangeRepeatedly s = case mChangeGenerated of
    Right change
      | length change >= length outputsToCover ->
          -- We've succeeded in making at least the optimal number of change
          -- outputs, and can terminate here.
          --
          -- Note that we can't use an exact length equality check here, as
          -- the 'makeChange' function will split up change outputs if they
          -- are oversized in any way. (See 'splitOversizedMaps'.)
          --
          -- It is therefore possible for 'makeChange' to generate more change
          -- outputs than the number of user-specified outputs.
          --
          pure $ Right $ mkSelectionResult change
    Right change ->
      -- We've succeeded in making change outputs, but the number of
      -- change outputs is fewer than optimal, because the supply of ada
      -- was insufficient. Try again with more ada to see if it leads to
      -- an improvement:
      selectOneEntry s >>= \case
        Just s' ->
          makeChangeRepeatedly s'
        Nothing ->
          -- There is no more ada available. Terminate with a
          -- less-than-optimal number of change outputs.
          pure $ Right $ mkSelectionResult change
    Left changeErr ->
      -- We've failed to make any change outputs, because the supply of
      -- ada was insufficient. Try again with more ada.
      selectOneEntry s >>= \case
        Just s' ->
          makeChangeRepeatedly s'
        Nothing ->
          -- There is no more ada available, and we were unable to
          -- make any change. At this point we must simply give up.
          pure $ Left $ UnableToConstructChange changeErr
   where
    mChangeGenerated :: Either UnableToConstructChangeError [GYValue]
    mChangeGenerated =
      makeChange
        MakeChangeCriteria
          { minCoinFor = computeMinimumAdaQuantity changeAddress
          , bundleSizeAssessor = tokenBundleSizeAssessor
          , requiredCost
          , extraCoinSource
          , extraCoinSink
          , inputBundles = snd <$> inputsSelected
          , outputBundles = snd <$> outputsToCover
          , assetsToMint
          , assetsToBurn
          , maximumOutputAdaQuantity
          , maximumOutputTokenQuantity
          }

    mkSelectionResult :: [GYValue] -> SelectionResult NonEmpty
    mkSelectionResult changeGenerated =
      SelectionResult
        { inputsSelected
        , extraCoinSource
        , extraCoinSink
        , changeGenerated = changeGenerated
        , outputsCovered = outputsToCover
        , assetsToMint
        , assetsToBurn
        }

    selectOneEntry = selectQuantityOf GYLovelace

    requiredCost =
      computeMinimumCost
        SelectionSkeleton
          { skeletonInputCount = UTxOSelection.selectedSize s
          , skeletonOutputs = NE.toList outputsToCover
          , skeletonChange
          }

    skeletonChange = predictChange s
    inputsSelected = UTxOSelection.selectedList s

  invariantResultWithNoCost inputs_ =
    error $
      unlines
        -- This should be impossible, as the 'makeChange' function should
        -- always succeed if there's no extra cost or minimum value to assign.
        -- This is because it is called with the result of 'runSelection',
        -- which only terminates successfully if the target was satisfied.
        [ "performSelection: couldn't construct change for a selection with no "
        , "minimum coin value and no cost!"
        , "inputs: " <> show inputs_
        , "extra coin source: " <> show extraCoinSource
        , "extra coin sink: " <> show extraCoinSink
        , "outputs: " <> show outputsToCover
        ]

--------------------------------------------------------------------------------
-- Running a selection (without making change)
--------------------------------------------------------------------------------

-- | Parameters for 'runSelection'.
data RunSelectionParams u = RunSelectionParams
  { utxoAvailable :: UTxOSelection u
  -- ^ UTxO entries available for selection.
  , minimumBalance :: GYValue
  -- ^ Minimum balance to cover.
  , selectionStrategy :: SelectionStrategy
  -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
  }
  deriving (Eq, Generic, Show)

runSelectionNonEmpty ::
  (MonadRandom m, Ord u) =>
  RunSelectionParams u ->
  m (Maybe (UTxOSelectionNonEmpty u))
runSelectionNonEmpty =
  runSelectionNonEmptyWith (selectQuantityOf GYLovelace)
    <=< runSelection

runSelectionNonEmptyWith ::
  Monad m =>
  (UTxOSelection u -> m (Maybe (UTxOSelectionNonEmpty u))) ->
  UTxOSelection u ->
  m (Maybe (UTxOSelectionNonEmpty u))
runSelectionNonEmptyWith selectSingleEntry result =
  UTxOSelection.toNonEmpty result
    & maybe
      (result & selectSingleEntry)
      (pure . Just)

runSelection ::
  forall m u.
  (MonadRandom m, Ord u) =>
  RunSelectionParams u ->
  m (UTxOSelection u)
runSelection params =
  runRoundRobinM utxoAvailable UTxOSelection.fromNonEmpty selectors
 where
  RunSelectionParams
    { utxoAvailable
    , minimumBalance
    , selectionStrategy
    } = params

  -- NOTE: We run the 'coinSelector' last, because we know that every input
  -- necessarily has a non-zero ada amount. By running the other selectors
  -- first, we increase the probability that the coin selector will be able
  -- to terminate without needing to select an additional coin.
  selectors :: [UTxOSelection u -> m (Maybe (UTxOSelectionNonEmpty u))]
  selectors =
    reverse (coinSelector : fmap (assetSelector . Data.Bifunctor.second fromInteger) minimumAssetQuantities)
   where
    assetSelector =
      runSelectionStep
        . assetSelectionLens selectionStrategy
    coinSelector =
      runSelectionStep $
        coinSelectionLens
          selectionStrategy
          minimumCoinQuantity

  (fromInteger -> minimumCoinQuantity, valueToList -> minimumAssetQuantities) =
    valueSplitAda minimumBalance

assetSelectionLens ::
  (MonadRandom m, Ord u) =>
  SelectionStrategy ->
  (GYAssetClass, Natural) ->
  SelectionLens m (UTxOSelection u) (UTxOSelectionNonEmpty u)
assetSelectionLens strategy (asset, minimumAssetQuantity) =
  SelectionLens
    { currentQuantity = selectedAssetQuantity asset
    , updatedQuantity = selectedAssetQuantity asset
    , minimumQuantity = minimumAssetQuantity
    , selectQuantity = selectQuantityOf asset
    , selectionStrategy = strategy
    }

coinSelectionLens ::
  (MonadRandom m, Ord u) =>
  SelectionStrategy ->
  -- | Minimum ada quantity.
  Natural ->
  SelectionLens m (UTxOSelection u) (UTxOSelectionNonEmpty u)
coinSelectionLens strategy minimumCoinQuantity =
  SelectionLens
    { currentQuantity = selectedCoinQuantity
    , updatedQuantity = selectedCoinQuantity
    , minimumQuantity = minimumCoinQuantity
    , selectQuantity = selectQuantityOf GYLovelace
    , selectionStrategy = strategy
    }

selectQuantityOf ::
  (MonadRandom m, Ord u) =>
  IsUTxOSelection utxoSelection u =>
  GYAssetClass ->
  utxoSelection u ->
  m (Maybe (UTxOSelectionNonEmpty u))
selectQuantityOf a =
  selectMatchingQuantity
    [ SelectSingleton a
    , SelectPairWith a
    , SelectAnyWith a
    ]

{- | Selects a UTxO entry that matches one of the specified filters.

This function traverses the specified list of filters from left to right, in
descending order of priority.

When considering a particular filter:

   - if the function is able to select a UTxO entry that matches, it
     terminates with an updated selection state that includes the entry.

   - if the function is not able to select a UTxO entry that matches, it
     traverses to the next filter available.

This function returns 'Nothing' if (and only if) it traverses the entire
list of filters without successfully selecting a UTxO entry.
-}
selectMatchingQuantity ::
  forall m utxoSelection u.
  (MonadRandom m, Ord u) =>
  IsUTxOSelection utxoSelection u =>
  -- | A list of selection filters to be traversed from left-to-right,
  -- in descending order of priority.
  NonEmpty (SelectionFilter GYAssetClass) ->
  -- | The current selection state.
  utxoSelection u ->
  -- | An updated selection state that includes a matching UTxO entry,
  -- or 'Nothing' if no such entry could be found.
  m (Maybe (UTxOSelectionNonEmpty u))
selectMatchingQuantity filters s =
  (updateState =<<)
    <$> UTxOIndex.selectRandomWithPriority
      (UTxOSelection.leftoverIndex s)
      filters
 where
  updateState ::
    ((u, GYValue), UTxOIndex u) -> Maybe (UTxOSelectionNonEmpty u)
  updateState ((i, _b), _remaining) = UTxOSelection.select i s

--------------------------------------------------------------------------------
-- Running a selection step
--------------------------------------------------------------------------------

{- | Provides a lens on the current selection state.

A 'SelectionLens' gives 'runSelectionStep' just the information it needs to
make a decision, and no more.
-}
data SelectionLens m state state' = SelectionLens
  { currentQuantity ::
      state ->
      Natural
  , updatedQuantity ::
      state' ->
      Natural
  , selectQuantity ::
      state ->
      m (Maybe state')
  , minimumQuantity ::
      Natural
  , selectionStrategy ::
      SelectionStrategy
  }

{- | Runs just a single step of a coin selection.

It returns an updated state if (and only if) the updated selection
represents an improvement over the selection in the previous state.

An improvement, for a given token quantity, is defined in the following way:

   - If the total selected token quantity of the previous selection had
     not yet reached 100% of the output token quantity, any additional
     selection is considered to be an improvement.

   - If the total selected token quantity of the previous selection had
     already reached or surpassed 100% of the output token quantity, any
     additional selection is considered to be an improvement if and only
     if it takes the total selected token quantity closer to the target
     token quantity, but not further away.
-}
runSelectionStep ::
  forall m state state'.
  Monad m =>
  SelectionLens m state state' ->
  state ->
  m (Maybe state')
runSelectionStep lens s
  | currentQuantity s < minimumQuantity =
      selectQuantity s
  | otherwise =
      (requireImprovement =<<) <$> selectQuantity s
 where
  SelectionLens
    { currentQuantity
    , updatedQuantity
    , minimumQuantity
    , selectQuantity
    , selectionStrategy
    } = lens

  requireImprovement :: state' -> Maybe state'
  requireImprovement s'
    | updatedDistanceFromTarget s' < currentDistanceFromTarget s = Just s'
    | otherwise = Nothing

  currentDistanceFromTarget :: state -> Natural
  currentDistanceFromTarget = distance targetQuantity . currentQuantity

  updatedDistanceFromTarget :: state' -> Natural
  updatedDistanceFromTarget = distance targetQuantity . updatedQuantity

  targetMultiplier :: Natural
  targetMultiplier = case selectionStrategy of
    SelectionStrategyMinimal -> 1
    SelectionStrategyOptimal -> 2

  targetQuantity :: Natural
  targetQuantity = minimumQuantity * targetMultiplier

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

-- | Criteria for the 'makeChange' function.
data MakeChangeCriteria minCoinFor bundleSizeAssessor = MakeChangeCriteria
  { minCoinFor :: minCoinFor
  -- ^ A function that computes the minimum required ada quantity for a
  -- particular output.
  , bundleSizeAssessor :: bundleSizeAssessor
  -- ^ A function to assess the size of a token bundle.
  , requiredCost :: Natural
  -- ^ The minimal (and optimal) delta between the total ada balance
  -- of all input bundles and the total ada balance of all output and
  -- change bundles, where:
  --
  --    delta = getCoin (fold inputBundles)
  --          - getCoin (fold outputBundles)
  --          - getCoin (fold changeBundles)
  --
  -- This typically captures fees plus key deposits.
  , extraCoinSource :: Natural
  -- ^ An extra source of ada.
  , extraCoinSink :: Natural
  -- ^ An extra sink for ada.
  , inputBundles :: NonEmpty GYValue
  -- ^ Token bundles of selected inputs.
  , outputBundles :: NonEmpty GYValue
  -- ^ Token bundles of original outputs.
  , assetsToMint :: GYValue
  -- ^ Assets to mint: these provide input value to a transaction.
  , assetsToBurn :: GYValue
  -- ^ Assets to burn: these consume output value from a transaction.
  , maximumOutputAdaQuantity ::
      Natural
  -- ^ Specifies the largest ada quantity that can appear in the token
  -- bundle of an output.
  , maximumOutputTokenQuantity ::
      Natural
  -- ^ Specifies the largest non-ada quantity that can appear in the
  -- token bundle of an output.
  -- TODO: This was TokenQuantity. Does my move towards Natural violate some invariant?
  }
  deriving (Eq, Generic, Show)

makeChange = undefined

--------------------------------------------------------------------------------
-- Round-robin processing
--------------------------------------------------------------------------------

runRoundRobin :: s -> (s' -> s) -> [s -> Maybe s'] -> s
runRoundRobin state demote processors =
  runIdentity $ runRoundRobinM state demote $ fmap Identity <$> processors

runRoundRobinM :: Monad m => s -> (s' -> s) -> [s -> m (Maybe s')] -> m s
runRoundRobinM state demote processors = go state processors []
 where
  go !s [] [] = pure s
  go !s [] !qs = go s (L.reverse qs) []
  go !s (p : ps) !qs =
    p s
      >>= \case
        Nothing -> go s ps qs
        Just s' -> go (demote s') ps (p : qs)

--------------------------------------------------------------------------------
-- Accessor functions
--------------------------------------------------------------------------------

selectedAssetQuantity :: IsUTxOSelection s u => GYAssetClass -> s u -> Natural
selectedAssetQuantity asset =
  fromInteger
    . flip valueAssetClass asset
    . UTxOSelection.selectedBalance

selectedCoinQuantity :: IsUTxOSelection s u => s u -> Natural
selectedCoinQuantity =
  fromInteger
    . flip valueAssetClass GYLovelace
    . UTxOSelection.selectedBalance

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

distance :: Natural -> Natural -> Natural
distance a b
  | a > b = a - b
  | a < b = b - a
  | otherwise = 0

mapMaybe :: (a -> Maybe b) -> NonEmpty a -> [b]
mapMaybe predicate (x :| xs) = go (x : xs)
 where
  go [] = []
  go (a : as) =
    case predicate a of
      Just b -> b : go as
      Nothing -> go as
