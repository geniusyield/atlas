{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

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
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Foldable qualified as F
import Data.Generics.Internal.VL.Lens (
  view,
 )
import Data.List.NonEmpty (
  NonEmpty (..),
 )
import Data.Map qualified as Map
import Data.Semigroup (mtimesDefault)
import Data.Set qualified as S
import Data.Text.Class (
  ToText (toText),
  fromText,
 )
import GHC.IsList (fromList)
import GeniusYield.Imports
import GeniusYield.Transaction.CoinSelection.UTxOSelection (UTxOSelection)
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
      GYTxOut v ->
      Natural
  -- ^ Computes the minimum ada quantity required for a given output.
  , computeMinimumCost ::
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
  , {-
    , maximumOutputAdaQuantity
        :: Coin
        -- ^ Specifies the largest ada quantity that can appear in the token
        -- bundle of an output.
    , maximumOutputTokenQuantity
        :: TokenQuantity
        -- ^ Specifies the largest non-ada quantity that can appear in the
        -- token bundle of an output.
      -- FIXME: Delete this comment.
    -}
    nullAddress ::
      GYAddress
      -- TODO: This is supposed to be an empty bytestring, not a complete gyaddress. Study if it creates an issue and whether it can be omitted.
  }
  deriving Generic

-- | Specifies all parameters that are specific to a given selection.
data SelectionParams = SelectionParams
  { outputsToCover ::
      ![(GYAddress, GYValue)]
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
  SelectionParams -> GYValue
computeUTxOBalanceAvailable =
  UTxOSelection.availableBalance . view #utxoAvailable

-- | Computes the balance of UTxO entries required to be selected.
computeUTxOBalanceRequired ::
  SelectionParams -> GYValue
computeUTxOBalanceRequired = fst . computeDeficitInOut

computeBalanceInOut ::
  SelectionParams -> (GYValue, GYValue)
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
  SelectionParams -> (GYValue, GYValue)
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
  SelectionParams -> UTxOBalanceSufficiency
computeUTxOBalanceSufficiency = sufficiency . computeUTxOBalanceSufficiencyInfo

{- | Computes information about the UTxO balance sufficiency.

See 'UTxOBalanceSufficiencyInfo'.
-}
computeUTxOBalanceSufficiencyInfo ::
  SelectionParams -> UTxOBalanceSufficiencyInfo
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
  SelectionParams -> Bool
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
data SelectionResult = SelectionResult
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
      ![(GYAddress, GYValue)]
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
  SelectionResult -> SelectionDelta GYValue
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
  SelectionResult -> SelectionDelta Natural
selectionDeltaCoin = fmap (fromIntegral . (`valueAssetClass` GYLovelace)) . selectionDeltaAllAssets

-- TODO: Was this above function ever used?

-- | Indicates whether or not a selection result has a valid surplus.
selectionHasValidSurplus ::
  SelectionConstraints v -> SelectionResult -> Bool
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
selectionSurplusCoin :: SelectionResult -> Natural
selectionSurplusCoin result =
  case selectionDeltaCoin result of
    SelectionSurplus surplus -> surplus
    SelectionDeficit _ -> 0

-- | Converts a selection into a skeleton.
selectionSkeleton ::
  SelectionResult -> SelectionSkeleton
selectionSkeleton s =
  SelectionSkeleton
    { skeletonInputCount = F.length (view #inputsSelected s)
    , skeletonOutputs = F.toList (view #outputsCovered s)
    , skeletonChange = valueAssets <$> view #changeGenerated s
    }

-- | Computes the minimum required cost of a selection.
selectionMinimumCost ::
  SelectionConstraints v -> SelectionResult -> Natural
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
  SelectionConstraints v -> SelectionResult -> Natural
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

type PerformSelection m v =
  SelectionConstraints v ->
  SelectionParams ->
  m (Either SelectionBalanceError SelectionResult)