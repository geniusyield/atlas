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
import Cardano.Numeric.Util (padCoalesce, partitionNatural)
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
import Data.Either.Extra (
  maybeToEither,
 )
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
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
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

type ValueSizeAssessor = GYValue -> ValueSizeAssessment

-- TODO: Having this 'v' lingering around is nuisance.

-- | Specifies all constraints required for coin selection.
data SelectionConstraints v = SelectionConstraints
  { tokenBundleSizeAssessor ::
      ValueSizeAssessor
  -- ^ Assesses the size of a value relative to the upper limit of
  -- what can be included in a transaction output.
  -- TODO: Make it just a Bool?
  -- TODO: Get rid of "bundle"...
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
          transform (`naturalMonus` dummyCoin) (const id) extraCoinSource
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

{- | Indicates 'True' if and only if a token bundle exceeds the maximum size
  that can be included in a transaction output.
-}
tokenBundleSizeExceedsLimit :: ValueSizeAssessor -> GYValue -> Bool
tokenBundleSizeExceedsLimit assess v =
  case assess v of
    ValueSizeWithinLimit ->
      False
    ValueSizeExceedsLimit ->
      True

{- | Constructs change bundles for a set of selected inputs and outputs.

Returns 'Nothing' if the specified inputs do not provide enough ada to
satisfy the minimum delta and minimum ada quantities of the change bundles
generated.

This function will generate runtime errors if:

   1.  The total balance of all outputs is not less than or equal to the
       total balance of all inputs.

   2.  The total ada balance of all outputs is zero.

Pre-condition (1) should be satisfied by any result produced by the
'runSelection' function.

Pre-condition (2) should be satisfied by assigning a minimum ada quantity
to every output token bundle.
-}
makeChange ::
  -- | Criteria for making change.
  MakeChangeCriteria (GYValue -> Natural) ValueSizeAssessor ->
  -- | Generated change bundles.
  Either UnableToConstructChangeError [GYValue]
makeChange criteria
  | not (totalOutputValue `valueLessOrEqual` totalInputValue) =
      totalInputValueInsufficient
  | valueAda totalOutputValue == 0 =
      totalOutputCoinValueIsZero
  | otherwise =
      first mkUnableToConstructChangeError $ do
        adaAvailable <-
          maybeToEither
            (requiredCost `naturalMonus` excessCoin)
            (excessCoin `naturalReductive` requiredCost)
        assignCoinsToChangeMaps
          adaAvailable
          minCoinFor
          changeMapOutputCoinPairs
 where
  MakeChangeCriteria
    { minCoinFor
    , bundleSizeAssessor
    , requiredCost
    , extraCoinSource
    , extraCoinSink
    , inputBundles
    , outputBundles
    , assetsToMint
    , assetsToBurn
    , maximumOutputAdaQuantity
    , maximumOutputTokenQuantity
    } = criteria

  -- The following subtraction is safe, as we have already checked
  -- that the total input value is greater than the total output
  -- value:
  excess :: GYValue
  excess = totalInputValue `valueMonus` totalOutputValue

  (fromInteger -> excessCoin, valueToList -> excessAssets) = valueSplitAda excess

  -- Change maps for all assets, where each change map is paired with a
  -- corresponding coin from the original outputs.
  --
  -- When combining change maps from user-specified assets and non-user-
  -- specified assets, we arrange that any empty maps in either list are
  -- combined together if possible, so as to give 'assignCoinsToChangeMaps'
  -- the greatest chance of success.
  --
  -- This list is sorted into ascending order of asset count, where empty
  -- change maps are all located at the start of the list.
  --
  changeMapOutputCoinPairs :: NonEmpty (GYValue, Natural) -- First component is actually tokens without ada.
  changeMapOutputCoinPairs =
    outputCoins
      -- First, combine the original output coins with the change maps for
      -- user-specified assets. We must pair these together right at the
      -- start in order to retain proportionality with the original outputs.
      & NE.zip changeForUserSpecifiedAssets
      -- Next, sort the list into ascending order of asset count, which moves
      -- any empty maps to the start of the list:
      & NE.sortWith (AssetCount . fst)
      -- Next, combine the existing list with the change maps for non-user
      -- specified assets, which are already sorted into ascending order of
      -- asset count:
      & NE.zipWith
        (\m1 (m2, c) -> (m1 <> m2, c))
        changeForNonUserSpecifiedAssets
      -- Finally, if there are any maps that are oversized (in any way), then
      -- split these maps up along with their corresponding output coins:
      & splitOversizedMaps
   where
    splitOversizedMaps ::
      NonEmpty (GYValue, Natural) -> NonEmpty (GYValue, Natural)
    splitOversizedMaps =
      -- For the sake of convenience when splitting up change maps and
      -- output coins (which are treated as weights), treat each change
      -- map and its corresponding output coin as a token bundle.
      fmap unbundle . split . fmap bundle
     where
      bundle (m, c) = m <> valueFromLovelace (fromIntegral c)
      unbundle val = let (c, m) = valueSplitAda val in (m, fromIntegral c)
      split b =
        b
          & flip
            splitBundlesWithExcessiveAssetCounts
            (tokenBundleSizeExceedsLimit assessBundleSizeWithMaxCoin)
          & flip
            splitBundlesWithExcessiveTokenQuantities
            maximumOutputTokenQuantity

      -- When assessing the size of a change map to determine if it is
      -- excessively large, we don't yet know how large the associated
      -- ada quantity will be, since ada quantities are assigned at a
      -- later stage (in 'assignCoinsToChangeMaps').
      --
      -- Therefore, we err on the side of caution, and assess the size
      -- of a change map combined with the maximum possible ada quantity.
      --
      -- This means that when presented with a very large change map, we
      -- have a small chance of splitting the map even if that map would
      -- be within the limit when combined with its final ada quantity.
      --
      -- However, oversplitting a change map is preferable to creating
      -- a bundle that is marginally over the limit, which would cause
      -- the resultant transaction to be rejected.
      --
      assessBundleSizeWithMaxCoin :: ValueSizeAssessor
      assessBundleSizeWithMaxCoin =
        bundleSizeAssessor
          . valueInsert GYLovelace (fromIntegral maximumOutputAdaQuantity)

  -- Change for user-specified assets: assets that were present in the
  -- original set of user-specified outputs ('outputsToCover').
  changeForUserSpecifiedAssets :: NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
  changeForUserSpecifiedAssets =
    F.foldr
      ( NE.zipWith (<>)
          . makeChangeForUserSpecifiedAsset outputMaps
      )
      (mempty <$ outputMaps)
      (fmap (Data.Bifunctor.second fromInteger) excessAssets)

  -- Change for non-user-specified assets: assets that were not present
  -- in the original set of user-specified outputs ('outputsToCover').
  changeForNonUserSpecifiedAssets :: NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
  changeForNonUserSpecifiedAssets =
    makeChangeForNonUserSpecifiedAssets
      outputMaps
      nonUserSpecifiedAssetQuantities
      & addMintValuesToChangeMaps
        (removeUserSpecifiedAssetIds assetsToMint)
      & removeBurnValuesFromChangeMaps
        (removeUserSpecifiedAssetIds assetsToBurn)
   where
    removeUserSpecifiedAssetIds :: GYValue -> GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
    removeUserSpecifiedAssetIds = flip valueWithoutAssets userSpecifiedAssetIds

  totalInputValueInsufficient =
    error
      "makeChange: not (totalOutputValue <= totalInputValue)"
  totalOutputCoinValueIsZero =
    error
      "makeChange: not (totalOutputCoinValue > 0)"

  mkUnableToConstructChangeError :: Natural -> UnableToConstructChangeError
  mkUnableToConstructChangeError shortfall =
    UnableToConstructChangeError
      { requiredCost
      , shortfall
      }

  outputMaps :: NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
  outputMaps = valueNonAda <$> outputBundles

  outputCoins :: NonEmpty Natural
  outputCoins = fromInteger . valueAda <$> outputBundles

  totalInputValue :: GYValue
  totalInputValue =
    F.fold inputBundles
      <> valueFromLovelace (fromIntegral extraCoinSource)
      -- Mints represent extra inputs from "the void"
      <> assetsToMint

  totalOutputValue :: GYValue
  totalOutputValue =
    F.fold outputBundles
      <> valueFromLovelace (fromIntegral extraCoinSink)
      -- Burns represent extra outputs to "the void"
      <> assetsToBurn

  -- Identifiers of all user-specified assets: assets that were included in
  -- the original set of outputs.
  userSpecifiedAssetIds :: Set GYAssetClass
  userSpecifiedAssetIds = valueAssets (F.fold outputBundles)

  -- Identifiers and quantities of all non-user-specified assets: assets that
  -- were not included in the original set of outputs, but that were
  -- nevertheless selected during the selection process.
  --
  -- Each asset is paired with the complete list of quantities of that asset
  -- present in the selected inputs.
  nonUserSpecifiedAssetQuantities :: Map GYAssetClass (NonEmpty Natural)
  nonUserSpecifiedAssetQuantities =
    collateNonUserSpecifiedAssetQuantities
      (valueNonAda <$> inputBundles)
      userSpecifiedAssetIds

{- | Generates a map of all non-user-specified assets and their quantities.

Each key in the resulting map corresponds to an asset that was NOT included
in the original set of user-specified outputs, but that was nevertheless
selected during the selection process.

The value associated with each key corresponds to the complete list of all
discrete non-zero quantities of that asset present in the selected inputs.
-}
collateNonUserSpecifiedAssetQuantities ::
  -- | Token maps of all selected inputs.
  NonEmpty GYValue -> -- Denotes without lovelace.

  -- | Set of all assets in user-specified outputs.
  Set GYAssetClass ->
  Map GYAssetClass (NonEmpty Natural)
collateNonUserSpecifiedAssetQuantities inputMaps userSpecifiedAssetIds =
  F.foldr discardUserSpecifiedAssets mempty inputMaps
 where
  discardUserSpecifiedAssets ::
    GYValue -> -- Denotes without lovelace.
    Map GYAssetClass (NonEmpty Natural) ->
    Map GYAssetClass (NonEmpty Natural)
  discardUserSpecifiedAssets tokens m =
    foldr (\(k, fromIntegral -> v) -> Map.insertWith (<>) k (v :| [])) m filtered
   where
    filtered =
      filter
        ((`Set.notMember` userSpecifiedAssetIds) . fst)
        (valueToList tokens)

{- | Assigns coin quantities to a list of pre-computed asset change maps.

Each pre-computed asset change map must be paired with the original coin
value of its corresponding output.

This function:

   - expects the list of pre-computed asset change maps to be sorted in an
     order that ensures all empty token maps are at the start of the list.

   - attempts to assign a minimum ada quantity to every change map, but
     iteratively drops empty change maps from the start of the list if the
     amount of ada is insufficient to cover them all.

   - continues dropping empty change maps from the start of the list until
     it is possible to assign a minimum ada value to all remaining entries.

   - returns a list that is identical in length to the input list if (and
     only if) it was possible to assign a minimum ada quantity to all change
     maps.

   - returns a list that is shorter than the input list if it was only
     possible to assign a minimum ada quantity to a suffix of the given
     list.

   - fails if (and only if) there was not enough ada available to assign the
     minimum ada quantity to all non-empty change maps.
-}
assignCoinsToChangeMaps ::
  HasCallStack =>
  -- | The total quantity of ada available, including any extra source of ada.
  Natural ->
  -- | A function to calculate the minimum required ada quantity for any
  -- token map.
  (GYValue -> Natural) -> -- Denotes without lovelace

  -- | A list of pre-computed asset change maps paired with original output
  -- coins, sorted into an order that ensures all empty token maps are at the
  -- start of the list.
  NonEmpty (GYValue, Natural) -> -- Denotes without lovelace.

  -- | Resulting change bundles, or the shortfall quantity if there was not
  -- enough ada available to assign a minimum ada quantity to all non-empty
  -- token maps.
  Either Natural [GYValue]
assignCoinsToChangeMaps adaAvailable minCoinFor pairsAtStart
  | not changeMapsCorrectlyOrdered =
      changeMapsNotCorrectlyOrderedError
  | otherwise =
      loop adaRequiredAtStart pairsAtStart
 where
  loop !adaRequired !pairsNonEmpty = case pairsNonEmpty of
    pair :| pairs
      | adaAvailable >= adaRequired ->
          -- We have enough ada available to pay for the minimum required
          -- amount of every asset map that remains in our list:
          let
            assetMapsRemaining = fst <$> (pair :| pairs)
            bundlesForAssetsWithMinimumCoins =
              assignMinimumCoin minCoinFor <$> assetMapsRemaining
            -- Calculate the amount of ada that remains after assigning the
            -- minimum amount to each map. This should be safe, as we have
            -- already determined that we have enough ada available:
            adaRemaining = adaAvailable `naturalMonus` adaRequired
            -- Partition any remaining ada according to the weighted
            -- distribution of output coins that remain in our list:
            outputCoinsRemaining = snd <$> (pair :| pairs)
            bundlesForOutputCoins =
              valueFromLovelace . fromIntegral
                <$> makeChangeForCoin outputCoinsRemaining adaRemaining
           in
            -- Finally, combine the minimal coin asset bundles with the
            -- bundles obtained by partitioning the remaining ada amount:
            Right $
              NE.toList $
                NE.zipWith
                  (<>)
                  bundlesForAssetsWithMinimumCoins
                  bundlesForOutputCoins
    (m, _) :| (p : ps)
      | isEmptyValue m && adaAvailable < adaRequired ->
          -- We don't have enough ada available to pay for the minimum
          -- required amount of every asset map, but we do have an empty
          -- asset map that is safe to drop. This will reduce the amount of
          -- ada required by a small amount:
          let adaRequired' = adaRequired `naturalMonus` minCoinFor m
           in loop adaRequired' (p :| ps)
    (m, _) :| []
      | isEmptyValue m && adaAvailable < adaRequired ->
          -- We didn't have any non-ada assets at all in our change, and we
          -- also don't have enough ada available to pay even for a single
          -- change output. We just burn the available ada amount (which
          -- will be small), returning no change.
          Right []
    _ ->
      -- We don't have enough ada available, and there are no empty token
      -- maps available to drop. We have to give up at this point.
      Left (adaRequired `naturalMonus` adaAvailable)

  adaRequiredAtStart = getSum $ F.fold $ Sum . minCoinFor . fst <$> pairsAtStart

  changeMaps = fst <$> pairsAtStart

  -- Indicates whether or not the given change maps are correctly ordered,
  -- so that all empty maps are located at the start of the list.
  changeMapsCorrectlyOrdered =
    (==)
      (NE.takeWhile isEmptyValue changeMaps)
      (NE.filter isEmptyValue changeMaps)

  changeMapsNotCorrectlyOrderedError =
    error $
      unwords
        [ "assignCoinsToChangeMaps: pre-computed asset change maps must be"
        , "arranged in an order where all empty maps are at the start of"
        , "the list."
        ]

-- | Assigns the minimum required ada quantity to a token map.
assignMinimumCoin :: (GYValue -> Natural) -> GYValue -> GYValue -- This first two 'GYValue' in this signature denote tokens without lovelace.
assignMinimumCoin minCoinFor m = valueFromLovelace (fromIntegral $ minCoinFor m) <> m

{- | Constructs change outputs for a user-specified asset: an asset that was
  present in the original set of outputs.

If the given asset does not appear in the given distribution, this function
returns a list of empty token maps. Otherwise, the given token quantity is
partitioned into a list of quantities that are proportional to the weights
within the given input distribution, modulo rounding.

The length of the output list is always the same as the the length of the
input list, and the sum of its quantities is either zero, or exactly equal
to the token quantity in the second argument.
-}
makeChangeForUserSpecifiedAsset ::
  -- | A list of weights for the distribution. Conveniently captures both
  -- the weights, and the number of elements amongst which the quantity
  -- should be distributed.
  NonEmpty GYValue -> -- Denotes without lovelace.

  -- | A surplus token quantity to distribute.
  (GYAssetClass, Natural) ->
  NonEmpty GYValue
makeChangeForUserSpecifiedAsset targets (asset, excess) =
  valueSingleton asset
    <$> maybe (fmap fromIntegral zeros) (fmap fromIntegral) (partitionNatural excess weights)
 where
  weights :: NonEmpty Natural
  weights = fromIntegral . flip valueAssetClass asset <$> targets

  zeros :: NonEmpty Natural
  zeros = 0 <$ targets

{- | Constructs change outputs for a non-user-specified asset: an asset that
  was not present in the original set of outputs.

This function constructs a list of change outputs by preserving the input
distribution as much as possible. Note that only the length of the first
argument is used.

The length of the output list is always the same as the length of the input
list, and the sum of its quantities is always exactly equal to the sum of
all token quantities given in the second argument.

The resultant list is sorted into ascending order when maps are compared
with the `leq` function.
-}
makeChangeForNonUserSpecifiedAsset ::
  -- | Determines the number of change maps to create.
  NonEmpty a ->
  -- | An asset quantity to distribute.
  (GYAssetClass, NonEmpty Natural) ->
  -- | The resultant change maps.
  NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
makeChangeForNonUserSpecifiedAsset n (asset, fmap (Sum . fromIntegral) -> quantities) =
  valueSingleton asset . getSum <$> padCoalesce quantities n

{- | Constructs change outputs for all non-user-specified assets: assets that
  were not present in the original set of outputs.

The resultant list is sorted into ascending order when maps are compared
with the `leq` function.
-}
makeChangeForNonUserSpecifiedAssets ::
  -- | Determines the number of change maps to create.
  NonEmpty a ->
  -- | A map of asset quantities to distribute.
  Map GYAssetClass (NonEmpty Natural) ->
  -- | The resultant change maps.
  NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
makeChangeForNonUserSpecifiedAssets n nonUserSpecifiedAssetQuantities =
  F.foldr
    (NE.zipWith (<>) . makeChangeForNonUserSpecifiedAsset n)
    (mempty <$ n)
    (Map.toList nonUserSpecifiedAssetQuantities)

{- | Constructs a list of ada change outputs based on the given distribution.

If the sum of weights in given distribution is equal to zero, this function
throws a runtime error.

The length of the output list is always the same as the length of the input
list, and the sum of its quantities is always exactly equal to the 'Coin'
value given as the second argument.
-}
makeChangeForCoin ::
  HasCallStack =>
  -- | A list of weights for the distribution. Conveniently captures both
  -- the weights, and the number of elements amongst which the surplus
  -- ada quantity should be distributed.
  NonEmpty Natural ->
  -- | A surplus ada quantity to be distributed.
  Natural ->
  NonEmpty Natural
makeChangeForCoin ws c = fromMaybe zeroWeightSumError $ partitionNatural c ws
 where
  zeroWeightSumError =
    error
      "makeChangeForCoin: weights must have a non-zero sum."

--------------------------------------------------------------------------------
-- Minting and burning
--------------------------------------------------------------------------------

-- Once we know how much change to give, grouping the change into bundles is a
-- somewhat complicated topic.
--
-- We want to create change outputs with, as far as possible, values that are
-- likely to be useful to the user in future, where values that more closely
-- approximate the user-specified outputs are considered more "useful".
--
-- A key property is that the number of change outputs should reflect the
-- number of outputs specified by the user. For example, if the user sends
-- value to five distinct outputs, we should create five distinct change
-- outputs.
--
-- However, we also want to mint and burn tokens. In general, minting tokens
-- requires us to add value to the change outputs and burning tokens requires
-- us to remove value from the change outputs.
--
-- It's also important to note that the change bundle calculation requires
-- that the change for user-specified and non-user-specified assets have the
-- following properties:
--
--    1. The lists share the same length;
--    2. The lists are in ascending partial order.
--
-- For example, given the following non-user-specified asset quantities:
--
--    [ ("A", [4, 1, 3, 2])
--    , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
--    ]
--
-- If the user requests 5 outputs in their transaction,
-- 'makeChangeForNonUserSpecifiedAssets' will generate:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ]
--
-- That is to say, it generates change bundles that satisfy the following
-- properties:
--
--    1.  The number of change bundles matches the number of outputs
--        the user originally requested;
--    2.  The change bundles are split in such a way to maximize the
--        number of large change bundles.
--
-- The change function maintains the property that the change bundles are in
-- ascending partial order, such that each change bundle is a subset of the
-- next. This property is required by 'changeMapOutputCoinPairs', so it's
-- important it's maintained.
--
-- The following two functions work by modifying the change bundles for
-- non-user-specified assets.
--
-- We add minted tokens to the largest change bundle:
--
--    [ [          ("B",  7) ]
--    [ [("A", 1), ("B",  8) ]
--    [ [("A", 2), ("B",  9) ]
--    [ [("A", 3), ("B",  9) ]
--    [ [("A", 4), ("B", 12) ] <-- add minted tokens here
--
-- We remove burned tokens from the smallest change bundles, until all burned
-- tokens are removed:
--
--    [ [          ("B", 7)  ] <-- start removing burned tokens from here
--    [ [("A", 1), ("B", 8)  ] <-- if we must burn more, remove from here
--    [ [("A", 2), ("B", 9)  ] <-- if we must burn more, remove from here
--    [ [("A", 3), ("B", 9)  ] <-- and so on, until we've removed everything.
--    [ [("A", 4), ("B", 12) ]
--
-- The solution for minting maintains the properties we desire, namely:
--
--    1.  The number of change bundles matches the number of
--        "outputs to cover" (we are not changing the number of bundles).
--    2.  The change bundles are in ascending partial order (by adding to the
--        largest bundle we trivially maintain ordering).
--    3.  The change bundles are split in such a way to maximize the
--        number of large change bundles.
--
-- The solution for burning maintains the same properties:
--
--    1.  The number of change bundles is not changed, in the case we burn a
--        change bundle completely, we just leave it as an empty entry
--        (effectively "pad with zeros").
--    2.  By removing from the smallest bundle, we maintain the ascending
--        partial order of the change bundles.
--    3.  By removing from the smallest bundles, we remove the "least useful"
--        bundles, maximizing the overall usefulness of our bundles.

{- | Adds a minted asset quantity to a list of change maps.

This function always adds the given quantity to the final change map in the
given list.

Example:

Suppose we have the following list of change maps:

   [ [          ("B",  7) ]
   [ [("A", 1), ("B",  8) ]
   [ [("A", 2), ("B",  9) ]
   [ [("A", 3), ("B",  9) ]
   [ [("A", 4), ("B", 12) ]

If we add 4 tokens of asset "A", we obtain the following result:

   [ [          ("B",  7) ]
   [ [("A", 1), ("B",  8) ]
   [ [("A", 2), ("B",  9) ]
   [ [("A", 3), ("B",  9) ]
   [ [("A", 8), ("B", 12) ] -- Increased by 4

Provided that the specified change maps are in ascending partial order, this
function guarantees that the resulting change maps will also be in ascending
partial order.

The length of the given list is preserved in the output list.
-}
addMintValueToChangeMaps ::
  (GYAssetClass, Natural) ->
  NonEmpty GYValue -> -- Actually denotes tokens without ada though this is not captured by our GYValue type.
  NonEmpty GYValue -- Actually denotes tokens without ada though this is not captured by our GYValue type.
addMintValueToChangeMaps (assetId, fromIntegral -> assetQty) =
  -- The largest element is the last element in an ascending order list
  modifyLast $ \m -> valueAdjust (assetQty +) assetId m
 where
  modifyLast f xs = case NE.reverse xs of
    (y :| ys) -> NE.reverse (f y :| ys)

{- | Adds minted values for multiple assets to a list of change maps.

Plural of @addMintValueToChangeMaps@.
-}
addMintValuesToChangeMaps ::
  -- | Map of minted values
  GYValue ->
  -- | Change maps
  NonEmpty GYValue ->
  -- | Change maps with minted values
  NonEmpty GYValue -- All 'GYValue' here are for without lovelace.
addMintValuesToChangeMaps =
  flip (F.foldr addMintValueToChangeMaps) . map (Data.Bifunctor.second fromInteger) . valueToList

{- | Removes a burned asset quantity from a list of change maps.

For a given asset 'a' and reduction target 't', this function traverses the
given list from left to right, reducing the quantity of asset 'a' in each
change map until the reduction target 't' has been met, or until the list
is exhausted.

For each change map 'm' under consideration:

   - if the quantity 'q' of asset 'a' in map 'm' is less than or equal to
     the remaining required reduction 'r', it will be replaced with a zero
     (effectively eliminating asset 'a' from the map).

   - if the quantity 'q' of asset 'a' in map 'm' is greater than the
     remaining required reduction 'r', it will be replaced with the
     absolute difference between 'q' and 'r'.

If the total quantity of the given asset in the given change maps is greater
than the specified reduction target, the total reduction will be equal to
the specified reduction target. Otherwise, the given asset will be
completely eliminated from all change maps.

Example:

Suppose we have the following list of change maps:

   [ [          ("B",  7) ]
   [ [("A", 1), ("B",  8) ]
   [ [("A", 2), ("B",  9) ]
   [ [("A", 3), ("B",  9) ]
   [ [("A", 4), ("B", 12) ]

If our target is to reduce the quantity of asset "A" by 4, then we should
obtain the following result:

   [ [          ("B",  7) ] -- Unable to reduce (already 0)
   [ [          ("B",  8) ] -- Reduced by 1 (and eliminated from map)
   [ [          ("B",  9) ] -- Reduced by 2 (and eliminated from map)
   [ [("A", 2), ("B",  9) ] -- Reduced by 1
   [ [("A", 4), ("B", 12) ]

Provided that the specified change maps are in ascending partial order, this
function guarantees that the resulting change maps will also be in ascending
partial order.

The length of the given list is preserved in the output list.
-}
removeBurnValueFromChangeMaps ::
  -- | Asset quantity reduction target
  (GYAssetClass, Natural) ->
  -- | Change maps with quantities of the given asset to be reduced
  NonEmpty GYValue -> -- without lovelace

  -- | Change maps with reduced quantities of the given asset
  NonEmpty GYValue -- without lovelace
removeBurnValueFromChangeMaps (assetId, assetQty) maps =
  maps
    & fmap (fromIntegral . (`valueAssetClass` assetId))
    & reduceTokenQuantities assetQty
    & NE.zipWith (\val n -> valueInsert assetId (fromIntegral n) val) maps

{- | Reduces the total value of the given list of token quantities by the given
  reduction target.

This function traverses the given list of quantities from left to right,
reducing each quantity in turn until the total reduction is equal to the
given reduction target, or until the list is exhausted.

For each quantity 'q' under consideration:

   - if 'q' is less than or equal to the remaining required reduction 'r',
     it will be replaced with a zero.

   - if 'q' is greater than the remaining required reduction 'r', it will
     be replaced with the absolute difference between 'q' and 'r'.

If the total value in the list is less than the reduction target, the
result will be a list of zeros.

Provided the given list is in ascending order, the resulting list is also
guaranteed to be in ascending order.

The length of the given list is preserved in the output.
-}
reduceTokenQuantities ::
  -- | Reduction target
  Natural ->
  -- | List of quantities to reduce
  NonEmpty Natural ->
  -- | The list of reduced quantities
  NonEmpty Natural
reduceTokenQuantities reductionTarget quantities =
  NE.fromList $ burn reductionTarget (NE.toList quantities) []
 where
  burn _ [] ys = reverse ys
  burn b (x : xs) ys
    | x >= b = reverse ys <> (x' : xs)
    | otherwise = burn b' xs (x' : ys)
   where
    b' = b `naturalMonus` x
    x' = x `naturalMonus` b

{- | Removes burned values for multiple assets from a list of change maps.

Plural of @removeBurnValueFromChangeMaps@.
-}
removeBurnValuesFromChangeMaps ::
  -- | Map of burned values
  GYValue -> -- all 'GYValue' here are for without lovelace.

  -- | Change maps
  NonEmpty GYValue ->
  -- | Change maps with burned values removed
  NonEmpty GYValue
removeBurnValuesFromChangeMaps =
  flip (F.foldr removeBurnValueFromChangeMaps) . fmap (Data.Bifunctor.second fromIntegral) . valueToList

--------------------------------------------------------------------------------
-- Splitting bundles
--------------------------------------------------------------------------------

{- | Splits a bundle into smaller bundles if its asset count is excessive when
  measured with the given 'isExcessive' indicator function.

Returns a list of smaller bundles for which 'isExcessive' returns 'False'.
-}
splitBundleIfAssetCountExcessive ::
  -- | The token bundle suspected to have an excessive number of assets.
  GYValue ->
  -- | A function that returns 'True' if (and only if) the asset count of
  -- the given bundle is excessive.
  (GYValue -> Bool) ->
  NonEmpty GYValue
splitBundleIfAssetCountExcessive b isExcessive
  | isExcessive b =
      splitInHalf b >>= flip splitBundleIfAssetCountExcessive isExcessive
  | otherwise =
      pure b
 where
  splitInHalf = flip equipartitionAssets (() :| [()])

equipartitionAssets = undefined
equipartitionQuantitiesWithUpperBound = undefined

{- | Splits bundles with excessive asset counts into smaller bundles.

Only token bundles where the 'isExcessive' indicator function returns 'True'
will be split.

Returns a list of smaller bundles for which 'isExcessive' returns 'False'.

If none of the bundles in the given list has an excessive asset count,
this function will return the original list.
-}
splitBundlesWithExcessiveAssetCounts ::
  -- | Token bundles.
  NonEmpty GYValue ->
  -- | A function that returns 'True' if (and only if) the asset count of
  -- the given bundle is excessive.
  (GYValue -> Bool) ->
  NonEmpty GYValue
splitBundlesWithExcessiveAssetCounts bs isExcessive =
  (`splitBundleIfAssetCountExcessive` isExcessive) =<< bs

{- | Splits bundles with excessive token quantities into smaller bundles.

Only token bundles containing quantities that exceed the maximum token
quantity will be split.

If none of the bundles in the given list contain a quantity that exceeds
the maximum token quantity, this function will return the original list.
-}
splitBundlesWithExcessiveTokenQuantities ::
  -- | Token bundles.
  NonEmpty GYValue ->
  -- | Maximum allowable token quantity.
  Natural ->
  -- | The partitioned bundles.
  NonEmpty GYValue
splitBundlesWithExcessiveTokenQuantities bs maxQuantity =
  (`equipartitionQuantitiesWithUpperBound` maxQuantity) =<< bs

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
-- Utility types
--------------------------------------------------------------------------------

{- | A total ordering on value based on the number of assets in it.

If two values have the same number of assets, then we fall back to ordinary
ordering as a tie-breaker.
-}
instance Ord (AssetCount GYValue) where
  compare = comparing projection
   where
    projection (AssetCount v) = (valueTotalAssets v, v)

newtype AssetCount a = AssetCount
  {unAssetCount :: a}
  deriving (Eq, Show)

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

naturalMonus :: Natural -> Natural -> Natural
naturalMonus a b
  | a >= b = a - b
  | otherwise = 0

naturalReductive :: Natural -> Natural -> Maybe Natural
naturalReductive a b
  | a >= b = Just (a - b)
  | otherwise = Nothing