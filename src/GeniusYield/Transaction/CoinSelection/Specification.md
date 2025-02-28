# Technical design specification

This document provides technical design specification of employed coin selection algorithm. We give detailed explanation of involved key functions along with their signatures and interested readers are requested to look at well-documented code for further deep-dive into the technicalities. 

Atlas supports multiple coin selection strategies but there is a common thread across all of them. For all strategies, we proceed via "logarithmic fee over-approximation". In UTxO based architecture of Cardano, the transaction fees depend upon the contents of the transaction body but the contents itself include fee and hence we find ourselves in a chicken and egg situation. To mitigate this, we proceed via our "logarithmic fee over-approximation" approach. Here we first assume transaction fee to be 1 ADA. Based on this, individual strategies would select inputs (on top of selected inputs from available UTxO set) to satisfy specified outputs along with their minimum ADA requirements and also for minimum ADA requirements of any generated change outputs along with this specified fee.

We then build the complete transaction (filling for things such as execution units, script integrity hash and so on) and update the fee field of transaction body to the one that corresponds more closely to minimal actual fee requirements of this transaction body, let's say it is 0.17 ADA. Since we had allocated 1 ADA for fee, the remaining 0.83 ADA would get added to an existing change output (if already present) or a new change output would get generated to accommodate it. Note that since there is a possibility of generation of a change output, our fee estimation earlier (of 0.17 ADA in this example) actually estimates minimal required fee assuming an additional change output (of max possible ADA amount, just to be safe).

For our running example, let's say, we indeed required additional change output, but say, 0.83 ADA is not enough to satisfy minimum ADA requirement of this UTxO. In which case, we'll start again but this time by twice the initial approximation, i.e., 2 ADA. Again, fee could eventually come out to be 0.17 ADA but leftover ADA of 1.83 should be enough to satisfy minimum ADA requirement.

If transaction fee itself turned out to be higher than our approximation, we'll start again by twice the previous approximation. This approach should allow us to converge to actual minimal fee in logarithmic steps and hence the name, "logarithmic fee over-approximation".

## `GYLegacy` strategy

`GYLegacy` is an old legacy strategy which linearly goes over available UTxO set, picking UTxOs from it until (if not already in "inputs") the total value in them satisfy value in "outputs" and the specified fee.

Note that "output" here not only represent outputs that user specified in their transaction skeleton but also the tokens that are supposed to be burned in this transaction and any additional ADA which would be given by user such as say ADA needed for DRep registration. We call such additional ADA deposits as "ADA sink".

Similarly, "inputs" not just include user specified inputs but also tokens that would be newly minted by this transaction and additional ADA that would be redeemed to the user such as when user deregisters their stake address. We call such additional ADA as "ADA source".

## `SelectionStrategyOptimal` & `SelectionStrategyMinimal` strategy

These are more complex strategies and require thorough explanation.

Strategy here determines __how much__ of each asset the selection
algorithm will attempt to select from the available UTxO set, relative to
the minimum amount necessary to make the selection balance.

'SelectionStrategyOptimal', will cause the selection algorithm to attempt to select around
__twice__ the minimum possible amount of each asset from the available
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

The difference between the two strategies would be further clear by the following abstraction.

Here `targetQuantity` is "multiplier" times `minimumQuantity` where "multiplier" is 1 if selection strategy is `SelectionStrategyMinimal` and is 2 otherwise.

```hs
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
  -- | Allows us to give new state which we determine if it's an "improvement" (explained next).
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

-- | Modulus between two numbers.
distance :: Natural -> Natural -> Natural
distance a b
  | a > b = a - b
  | a < b = b - a
  | otherwise = 0
```

For the purpose of further explanation of intricacies of the strategies, let's define some types and small utilities.

```hs
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

-- | Semantically a 'GYValue' without the 'GYLovelace' asset class.
type GYValueWithoutLovelace = GYValue

-- | Specifies all constraints required for coin selection.
data SelectionConstraints = SelectionConstraints
  { valueSizeAssessor ::
      ValueSizeAssessor
  -- ^ Assesses the size of a value relative to the upper limit of
  -- what can be included in a transaction output.
  , computeMinimumAdaQuantity ::
      GYAddress ->
      GYValue ->
      Natural
  -- ^ Computes the minimum ada quantity required for a given output.
  , computeMinimumCost ::
      Natural
  -- ^ Minimum cost of a given selection skeleton, this is fixed to value as given by user in fee over approximation approach.
  , changeAddress ::
      GYAddress
  , maximumOutputAdaQuantity ::
      Natural
  -- ^ Specifies the largest ada quantity that can appear in the token
  -- bundle of an output.
  , maximumOutputTokenQuantity ::
      Natural
  -- ^ Specifies the largest non-ada quantity that can appear in the
  -- token bundle of an output.
  , nullAddress ::
      GYAddress
  }
  deriving Generic

-- | Specifies all parameters that are specific to a given selection.
data SelectionParams f = SelectionParams
  { outputsToCover ::
      !(f (GYAddress, GYValue))
  -- ^ The complete set of outputs to be covered.
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
```

Next we have following types and utilities to tell if provided UTxO set is enough to cover for "outputs".

```hs
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

valueAssetsWithoutLovelace :: GYValue -> Set GYAssetClass
valueAssetsWithoutLovelace = Set.delete GYLovelace . valueAssets

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
  }
  deriving (Generic, Eq, Show)

type PerformSelection m f =
  SelectionConstraints ->
  SelectionParams f ->
  m (Either SelectionBalanceError (SelectionResult f))

{- | Performs a coin selection and generates change values in one step.

Provided that 'isUTxOBalanceSufficient' returns 'True' for the given
selection criteria, this function guarantees to return a 'SelectionResult'
which has a "valid" surplus (see 'selectionHasValidSurplus' in code). Vaguely,
a surplus is valid if it consists of only ADA and is above the estimated fee but not
too high from estimated fee (should be less or equal to twice the estimation)
-}
performSelection ::
  forall m.
  (HasCallStack, MonadRandom m) =>
  PerformSelection m []
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
Note that I have deliberately removed definitions of 'selectionSurplus' and 'selectionHasValidSurplus' from this specification just to make it concise but they are already explained for earlier.
-}
performSelectionEmpty ::
  forall m.
  Functor m =>
  PerformSelection m NonEmpty ->
  PerformSelection m []
```

Before we describe selection process, assuming we have made a selection, let us first see how change is generated for.

### Specification on change output generation

Once we know how much change to give, grouping the change into values is a
somewhat complicated topic.
We want to create change outputs with, as far as possible, values that are
likely to be useful to the user in future, where values that more closely
approximate the user-specified outputs are considered more "useful".
A key property is that the number of change outputs should reflect the
number of outputs specified by the user. For example, if the user sends
value to five distinct outputs, we should create five distinct change
outputs.
However, we also want to mint and burn tokens. In general, minting tokens
requires us to add value to the change outputs and burning tokens requires
us to remove value from the change outputs.
It's also important to note that the change value calculation requires
that the change for user-specified and non-user-specified assets have the
following properties:
   1. The lists share the same length;
   2. The lists are in ascending partial order.
For example, given the following non-user-specified asset quantities:
```
   [ ("A", [4, 1, 3, 2])
   , ("B", [9, 1, 8, 2, 7, 3, 6, 4, 5])
   ]
```
If the user requests 5 outputs in their transaction,
`makeChangeForNonUserSpecifiedAssets` will generate:
```
   [ [          ("B",  7) ] ]
   [ [("A", 1), ("B",  8) ] ]
   [ [("A", 2), ("B",  9) ] ]
   [ [("A", 3), ("B",  9) ] ]
   [ [("A", 4), ("B", 12) ] ]
```
That is to say, it generates change values that satisfy the following
properties:
   1.  The number of change values matches the number of outputs
       the user originally requested;
   2.  The change values are split in such a way to maximize the
       number of large change values.
The change function maintains the property that the change values are in
ascending partial order, such that each change value is a subset of the
next. This property is required by `changeMapOutputCoinPairs`, so it's
important it's maintained.
The `addMintValueToChangeMaps` & `removeBurnValueFromChangeMaps` functions work by modifying the change values for
non-user-specified assets.
We add minted tokens to the largest change value:
```
   [ [          ("B",  7) ] ]
   [ [("A", 1), ("B",  8) ] ]
   [ [("A", 2), ("B",  9) ] ]
   [ [("A", 3), ("B",  9) ] ]
   [ [("A", 4), ("B", 12) ] ] <-- add minted tokens here
```
We remove burned tokens from the smallest change values, until all burned
tokens are removed:
```
   [ [          ("B", 7)  ] ] <-- start removing burned tokens from here
   [ [("A", 1), ("B", 8)  ] ] <-- if we must burn more, remove from here
   [ [("A", 2), ("B", 9)  ] ] <-- if we must burn more, remove from here
   [ [("A", 3), ("B", 9)  ] ] <-- and so on, until we've removed everything.
   [ [("A", 4), ("B", 12) ] ]
```
The solution for minting maintains the properties we desire, namely:
   1.  The number of change values matches the number of
       "outputs to cover" (we are not changing the number of values).
   2.  The change values are in ascending partial order (by adding to the
       largest value we trivially maintain ordering).
   3.  The change values are split in such a way to maximize the
       number of large change values.
The solution for burning maintains the same properties:
   1.  The number of change values is not changed, in the case we burn a
       change value completely, we just leave it as an empty entry
       (effectively "pad with zeros").
   2.  By removing from the smallest value, we maintain the ascending
       partial order of the change values.
   3.  By removing from the smallest values, we remove the "least useful"
       values, maximizing the overall usefulness of our values.

#### `makeChange`

Constructs change values for a set of selected inputs and outputs.

Returns `Nothing` if the specified inputs do not provide enough ada to
satisfy the minimum delta (`requiredCost`) and minimum ada quantities of the change values
generated.

This function will generate runtime errors if:

   1.  The total balance of all outputs (original outputs, assets to burn, and ada sink)
       is not less than or equal to the total balance of all inputs (inputs, ada source & mints).

   2.  The total ada balance of all outputs is zero.

Pre-condition (1) should be satisfied by any result produced by the
`runSelection` function.

Pre-condition (2) should be satisfied by assigning a minimum ada quantity
to every output value.

This function calls `assignCoinsToChangeMaps adaAvailable minCoinFor changeMapOutputCoinPairs`
where `adaAvailable` is the total ada balance of all inputs minus the total ada balance of all outputs and `requiredCost`,
`minCoinFor` is a function that computes the minimum ada quantity for a particular output,
and `changeMapOutputCoinPairs` is a list of change maps paired with the ada quantity of the original outputs (see it's definition in code).
```hs
-- | Criteria for the 'makeChange' function.
data MakeChangeCriteria minCoinFor valueSizeAssessor = MakeChangeCriteria
  { minCoinFor :: minCoinFor
  -- ^ A function that computes the minimum required ada quantity for a
  -- particular output.
  , valueSizeAssessor :: valueSizeAssessor
  -- ^ A function to assess the size of a value.
  , requiredCost :: Natural
  -- ^ The minimal (and optimal) delta between the total ada balance
  -- of all input values and the total ada balance of all output and
  -- change values, where:
  --
  --    delta = getCoin (fold inputValues)
  --          - getCoin (fold outputValues)
  --          - getCoin (fold changeValues)
  --
  -- This typically captures fees plus key deposits.
  , extraCoinSource :: Natural
  -- ^ An extra source of ada.
  , extraCoinSink :: Natural
  -- ^ An extra sink for ada.
  , inputValues :: NonEmpty GYValue
  -- ^ Token values of selected inputs.
  , outputValues :: NonEmpty GYValue
  -- ^ Token values of original outputs.
  , assetsToMint :: GYValue
  -- ^ Assets to mint: these provide input value to a transaction.
  , assetsToBurn :: GYValue
  -- ^ Assets to burn: these consume output value from a transaction.
  , maximumOutputAdaQuantity ::
      Natural
  -- ^ Specifies the largest ada quantity that can appear in the
  -- value of an output.
  , maximumOutputTokenQuantity ::
      Natural
  -- ^ Specifies the largest non-ada quantity that can appear in the
  -- value of an output.
  }
  deriving (Eq, Generic, Show)

makeChange ::
  -- | Criteria for making change.
  MakeChangeCriteria (GYValue -> Natural) ValueSizeAssessor ->
  -- | Generated change bundles.
  Either UnableToConstructChangeError [GYValue]
```

Definition of `changeMapOutputCoinPairs` is as follows:

```hs
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
  changeMapOutputCoinPairs :: NonEmpty (GYValueWithoutLovelace, Natural)
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
            splitValuesWithExcessiveAssetCounts
            (valueSizeExceedsLimit assessValueSizeWithMaxCoin)
          & flip
            splitValuesWithExcessiveTokenQuantities
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
      assessValueSizeWithMaxCoin :: ValueSizeAssessor
      assessValueSizeWithMaxCoin =
        valueSizeAssessor
          . valueInsert GYLovelace (fromIntegral maximumOutputAdaQuantity)

  -- Change for user-specified assets: assets that were present in the
  -- original set of user-specified outputs ('outputsToCover').
  changeForUserSpecifiedAssets :: NonEmpty GYValueWithoutLovelace
  changeForUserSpecifiedAssets =
    F.foldr
      ( NE.zipWith (<>)
          . makeChangeForUserSpecifiedAsset outputMaps
      )
      (mempty <$ outputMaps)
      (fmap (Data.Bifunctor.second fromInteger) excessAssets)

  -- Change for non-user-specified assets: assets that were not present
  -- in the original set of user-specified outputs ('outputsToCover').
  changeForNonUserSpecifiedAssets :: NonEmpty GYValueWithoutLovelace
  changeForNonUserSpecifiedAssets =
    makeChangeForNonUserSpecifiedAssets
      outputMaps
      nonUserSpecifiedAssetQuantities
      & addMintValuesToChangeMaps
        (removeUserSpecifiedAssetIds assetsToMint)
      & removeBurnValuesFromChangeMaps
        (removeUserSpecifiedAssetIds assetsToBurn)
   where
    removeUserSpecifiedAssetIds :: GYValueWithoutLovelace -> GYValueWithoutLovelace
    removeUserSpecifiedAssetIds = flip valueWithoutAssets userSpecifiedAssetIds
```

#### `assignCoinsToChangeMaps`

Assigns coin quantities to a list of pre-computed asset change maps.

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

  - after the function has assigned the minimum ada quantity to all
    non-empty change maps, it partitions any remaining ada according to the
    weighted distribution of original output coins in our list.

```hs
assignCoinsToChangeMaps ::
  HasCallStack =>
  -- | The total quantity of ada available, including any extra source of ada.
  Natural ->
  -- | A function to calculate the minimum required ada quantity for any
  -- token map.
  (GYValueWithoutLovelace -> Natural) ->
  -- | A list of pre-computed asset change maps paired with original output
  -- coins, sorted into an order that ensures all empty token maps are at the
  -- start of the list.
  NonEmpty (GYValueWithoutLovelace, Natural) ->
  -- | Resulting change bundles, or the shortfall quantity if there was not
  -- enough ada available to assign a minimum ada quantity to all non-empty
  -- token maps.
  Either Natural [GYValue]
```

#### `assignMinimumCoin`

Assigns the minimum required ada quantity to a token map.

```hs
assignMinimumCoin :: (GYValueWithoutLovelace -> Natural) -> GYValueWithoutLovelace -> GYValue
```

#### `makeChangeForUserSpecifiedAsset`
Constructs change outputs for a user-specified asset: an asset that was
  present in the original set of outputs.

If the given asset does not appear in the given distribution, this function
returns a list of empty token maps. Otherwise, the given token quantity is
partitioned into a list of quantities that are proportional to the weights
within the given input distribution, modulo rounding.

The length of the output list is always the same as the the length of the
input list, and the sum of its quantities is either zero, or exactly equal
to the token quantity in the second argument.
```hs
makeChangeForUserSpecifiedAsset ::
  -- | A list of weights for the distribution. Conveniently captures both
  -- the weights, and the number of elements amongst which the quantity
  -- should be distributed.
  NonEmpty GYValueWithoutLovelace ->
  -- | A surplus token quantity to distribute.
  (GYAssetClass, Natural) ->
  NonEmpty GYValueWithoutLovelace
```

#### `makeChangeForNonUserSpecifiedAsset`
Constructs change outputs for a non-user-specified asset: an asset that
  was not present in the original set of outputs.

This function constructs a list of change outputs by preserving the input
distribution as much as possible. Note that only the length of the first
argument is used.

The length of the output list is always the same as the length of the input
list, and the sum of its quantities is always exactly equal to the sum of
all token quantities given in the second argument.

The resultant list is sorted into ascending order when maps are compared
with the `leq` function.
```hs
makeChangeForNonUserSpecifiedAsset ::
  -- | Determines the number of change maps to create.
  NonEmpty a ->
  -- | A map of asset quantities to distribute.
  Map GYAssetClass (NonEmpty Natural) ->
  -- | The resultant change maps.
  NonEmpty GYValueWithoutLovelace
```

#### `makeChangeForNonUserSpecifiedAssets`
Constructs change outputs for all non-user-specified assets: assets that
  were not present in the original set of outputs.

The resultant list is sorted into ascending order when maps are compared
with the `leq` function.
```hs
makeChangeForNonUserSpecifiedAssets ::
  -- | Determines the number of change maps to create.
  NonEmpty a ->
  -- | A map of asset quantities to distribute.
  Map GYAssetClass (NonEmpty Natural) ->
  -- | The resultant change maps.
  NonEmpty GYValueWithoutLovelace
```

#### `makeChangeForCoin`
Constructs a list of ada change outputs based on the given distribution.

If the sum of weights in given distribution is equal to zero, this function
throws a runtime error.

The length of the output list is always the same as the length of the input
list, and the sum of its quantities is always exactly equal to the 'Coin'
value given as the second argument.
```hs
makeChangeForCoin ::
  HasCallStack =>
  -- | A list of weights for the distribution. Conveniently captures both
  -- the weights, and the number of elements amongst which the surplus
  -- ada quantity should be distributed.
  NonEmpty Natural ->
  -- | A surplus ada quantity to be distributed.
  Natural ->
  NonEmpty Natural
```

#### Functions related to minting & burning

##### `addMintValueToChangeMaps`

Adds a minted asset quantity to a list of change maps.

This function always adds the given quantity to the final change map in the
given list.

Example:

Suppose we have the following list of change maps:

```
   [ [          ("B",  7) ] ]
   [ [("A", 1), ("B",  8) ] ]
   [ [("A", 2), ("B",  9) ] ]
   [ [("A", 3), ("B",  9) ] ]
   [ [("A", 4), ("B", 12) ] ]
```
If we add 4 tokens of asset "A", we obtain the following result:
```
   [ [          ("B",  7) ] ]
   [ [("A", 1), ("B",  8) ] ]
   [ [("A", 2), ("B",  9) ] ]
   [ [("A", 3), ("B",  9) ] ]
   [ [("A", 8), ("B", 12) ] ] -- Increased by 4
```

Provided that the specified change maps are in ascending partial order, this
function guarantees that the resulting change maps will also be in ascending
partial order.

The length of the given list is preserved in the output list.
```hs
addMintValueToChangeMaps ::
  (GYAssetClass, Natural) ->
  NonEmpty GYValueWithoutLovelace ->
  NonEmpty GYValueWithoutLovelace
```

##### `addMintValuesToChangeMaps`

Adds minted values for multiple assets to a list of change maps.

Plural of `addMintValueToChangeMaps`.

```hs
addMintValuesToChangeMaps ::
  -- | Map of minted values
  GYValueWithoutLovelace ->
  -- | Change maps
  NonEmpty GYValueWithoutLovelace ->
  -- | Change maps with minted values
  NonEmpty GYValueWithoutLovelace
```

##### `removeBurnValueFromChangeMaps`

Removes a burned asset quantity from a list of change maps.

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
```
   [ [          ("B",  7) ] ]
   [ [("A", 1), ("B",  8) ] ]
   [ [("A", 2), ("B",  9) ] ]
   [ [("A", 3), ("B",  9) ] ]
   [ [("A", 4), ("B", 12) ] ]
```
If our target is to reduce the quantity of asset "A" by 4, then we should
obtain the following result:
```
   [ [          ("B",  7) ] ] -- Unable to reduce (already 0)
   [ [          ("B",  8) ] ] -- Reduced by 1 (and eliminated from map)
   [ [          ("B",  9) ] ] -- Reduced by 2 (and eliminated from map)
   [ [("A", 2), ("B",  9) ] ] -- Reduced by 1
   [ [("A", 4), ("B", 12) ] ]
```
Provided that the specified change maps are in ascending partial order, this
function guarantees that the resulting change maps will also be in ascending
partial order.

The length of the given list is preserved in the output list.

##### `removeBurnValuesFromChangeMaps`

Removes burned values for multiple assets from a list of change maps.

Plural of `removeBurnValueFromChangeMaps`.

```hs
removeBurnValuesFromChangeMaps ::
  -- | Map of burned values
  GYValueWithoutLovelace ->
  -- | Change maps
  NonEmpty GYValueWithoutLovelace ->
  -- | Change maps with burned values removed
  NonEmpty GYValueWithoutLovelace
```

##### `reduceTokenQuantities`
Reduces the total value of the given list of token quantities by the given
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

```hs
reduceTokenQuantities ::
  -- | Reduction target
  Natural ->
  -- | List of quantities to reduce
  NonEmpty Natural ->
  -- | The list of reduced quantities
  NonEmpty Natural
```

### Elaboration on input selection

The following code snippet does justice to elaboration on input selection. But to provide outline, we first compute required value (denoting the difference between "outputs" and selected "inputs") to be obtained from available UTxO set (think of it as a tuple of already selected outputs and other available outputs).

Next we run round robin,

```hs
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
```

against the initial UTxO available state and different selectors. Selectors here try to select for UTxO on priority basis which have either only the desired non-ADA token (desired tokens are those obtained from required value) along with ADA, or just one extra non-ADA token or any UTxO having our desired token.

Once we have found our selection, we try to generate change outputs via our `makeChange` function. If it succeeds and gives us change outputs more than the number of user mentioned outputs, we are done. Else we try to have a selection with more ADA and see if we are able to generate more change outputs by recursing on this function. If recursion fails, we proceed with the number of change outputs we had obtained.
However, in case function fails to give any change outputs, we try to improve our selection with more ADA and if it that is deemed possible, we recurse on this function with our improved selection else we return error case.

```hs
performSelectionNonEmpty ::
  forall m.
  (HasCallStack, MonadRandom m) =>
  PerformSelection m NonEmpty
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
    { valueSizeAssessor
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
      (fmap valueAssetsWithoutLovelace)
      ( makeChange
          MakeChangeCriteria
            { minCoinFor = noMinimumCoin
            , valueSizeAssessor = valueSizeAssessor
            , requiredCost = noCost
            , extraCoinSource
            , extraCoinSink
            , inputValues
            , outputValues
            , assetsToMint
            , assetsToBurn
            , maximumOutputAdaQuantity
            , maximumOutputTokenQuantity
            }
      )
   where
    inputValues = snd <$> UTxOSelection.selectedList s
    outputValues = snd <$> outputsToCover

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
          , valueSizeAssessor = valueSizeAssessor
          , requiredCost
          , extraCoinSource
          , extraCoinSink
          , inputValues = snd <$> inputsSelected
          , outputValues = snd <$> outputsToCover
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
```