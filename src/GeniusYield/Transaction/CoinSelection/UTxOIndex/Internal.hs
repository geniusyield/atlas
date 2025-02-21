{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

{- |
Copyright: © 2018-2021 IOHK, 2025 GYELD GMBH
License: Apache-2.0

Modified by: GeniusYield

Originally from: [@Cardano.CoinSelection.UTxOIndex.Internal@](https://github.com/cardano-foundation/cardano-wallet/blob/master/lib/coin-selection/lib/Cardano/CoinSelection/UTxOIndex/Internal.hs).

Provides internal functions for the 'UTxOIndex' type, which indexes a UTxO
set by asset identifier.

The index makes it possible to efficiently compute the subset of a UTxO set
containing a particular asset, or to select just a single UTxO containing a
particular asset, without having to search linearly through the entire UTxO
set.

See the documentation for 'UTxOIndex' for more details.
-}
module GeniusYield.Transaction.CoinSelection.UTxOIndex.Internal (
  ----------------------------------------------------------------------------
  -- Public Interface
  ----------------------------------------------------------------------------

  -- * Type

  -- Important:
  --
  -- The default data constructor for 'UTxOIndex' is not exported, by
  -- design, as the internal data structure has an invariant that must
  -- be preserved across all operations.
  --
  -- See the 'checkInvariant' function for more details.
  --
  UTxOIndex,

  -- * Construction
  empty,
  singleton,
  fromSequence,
  fromMap,

  -- * Deconstruction
  toList,
  toMap,

  -- * Folding
  fold,

  -- * Modification
  insert,
  insertMany,
  delete,
  deleteMany,

  -- * Filtering and partitioning
  filter,
  partition,

  -- * Queries
  assets,
  balance,
  lookup,
  member,
  null,
  size,

  -- * Set operations
  difference,
  disjoint,

  -- * Selection
  SelectionFilter (..),
  selectRandom,
  selectRandomWithPriority,
  ----------------------------------------------------------------------------
  -- Internal Interface
  ----------------------------------------------------------------------------

  -- * Value categorization
  ValueCategory (..),
  categorizeValue,

  -- * Utilities
  selectRandomSetMember,

  -- * Invariant
  InvariantStatus (..),
  checkInvariant,
) where

import Prelude hiding (
  filter,
  lookup,
  null,
 )

import Control.Monad.Extra (
  firstJustM,
 )
import Control.Monad.Random.Class (
  MonadRandom (..),
 )
import Data.Bifunctor (
  bimap,
 )
import Data.Foldable qualified as F
import Data.Function (
  (&),
 )
import Data.List qualified as L
import Data.List.NonEmpty (
  NonEmpty,
 )
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (
  Map,
 )
import Data.Map.Strict qualified as Map
import Data.Maybe (
  isJust,
 )
import Data.MonoidMap (
  MonoidMap,
 )
import Data.MonoidMap qualified as MonoidMap
import Data.Set (
  Set,
 )
import Data.Set qualified as Set
import GHC.Generics (
  Generic,
 )
import GeniusYield.Types

--------------------------------------------------------------------------------
-- Public Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

{- | A UTxO set that is indexed by asset identifier.

The index provides a mapping from assets to subsets of the UTxO set.

A UTxO appears in the set for a particular asset if and only if its
associated value has a non-zero quantity of that asset.

The index makes it possible to efficiently compute the subset of a UTxO set
containing a particular asset, or to select just a single UTxO containing a
particular asset, without having to search linearly through the entire UTxO
set.

The index also keeps track of the current UTxO balance of all assets, making
it possible to efficiently look up the total quantity of a particular asset
without having to sum across the entire UTxO set.

The UTxO index data structure has an invariant that can be checked with
the 'checkInvariant' function.
-}
data UTxOIndex u = UTxOIndex
  { indexAll ::
      !(MonoidMap GYAssetClass (Set u))
  , -- An index of all entries that contain the given asset.
    indexSingletons ::
      !(MonoidMap GYAssetClass (Set u))
  , -- An index of all entries that contain the given asset and no other
    -- assets.
    indexPairs ::
      !(MonoidMap GYAssetClass (Set u))
  , -- An index of all entries that contain the given asset and exactly
    -- one other asset.
    balance ::
      !GYValue
  , -- The total balance of all entries.
    universe ::
      !(Map u GYValue)
      -- The complete set of all entries.
  }
  deriving (Eq, Generic, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | An index with no entries.
empty :: UTxOIndex u
empty =
  UTxOIndex
    { indexAll = MonoidMap.empty
    , indexSingletons = MonoidMap.empty
    , indexPairs = MonoidMap.empty
    , balance = mempty
    , universe = Map.empty
    }

-- | Creates a singleton index from the specified UTxO identifier and value.
singleton :: Ord u => u -> GYValue -> UTxOIndex u
singleton u v = insertUnsafe u v empty

{- | Constructs an index from a sequence of entries.

Note that this operation is potentially expensive as it must construct an
index from scratch, and therefore should only be used sparingly.

If the given sequence contains more than one mapping for the same UTxO
identifier, the mapping that appears latest in the sequence will take
precedence, and all others will be ignored.
-}
fromSequence :: (Foldable f, Ord u) => f (u, GYValue) -> UTxOIndex u
fromSequence = flip insertMany empty

{- | Constructs an index from a map.

Note that this operation is potentially expensive as it must construct an
index from scratch, and therefore should only be used sparingly.

Satisfies the following property:

@
'fromMap' ≡ 'fromSequence' . 'Map.toList'
@
-}
fromMap :: Ord u => Map u GYValue -> UTxOIndex u
fromMap = Map.foldlWithKey' (\i u b -> insertUnsafe u b i) empty

--------------------------------------------------------------------------------
-- Deconstruction
--------------------------------------------------------------------------------

{- | Converts an index to a list of its constituent entries.

Consider using 'fold' if your goal is to consume all entries in the output.
-}
toList :: UTxOIndex u -> [(u, GYValue)]
toList = fold (\ubs u b -> (u, b) : ubs) []

{- | Converts an index into a map.

Consider using 'fold' if your goal is to consume all entries in the output.
-}
toMap :: UTxOIndex u -> Map u GYValue
toMap = universe

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

-- | Folds strictly over the constituent entries of an index.
fold :: (a -> u -> GYValue -> a) -> a -> UTxOIndex u -> a
fold f a = Map.foldlWithKey' f a . universe

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

{- | Inserts an entry that maps the given UTxO identifier to the given value.

If the index has an existing value for the specified UTxO identifier, the
value referred to by that identifier will be replaced with the specified
value.
-}
insert :: Ord u => u -> GYValue -> UTxOIndex u -> UTxOIndex u
insert u b = insertUnsafe u b . delete u

{- | Inserts multiple entries into an index.

See 'insert'.
-}
insertMany ::
  (Foldable f, Ord u) =>
  f (u, GYValue) ->
  UTxOIndex u ->
  UTxOIndex u
insertMany = flip $ F.foldl' $ \i (u, b) -> insert u b i

{- | Deletes the entry corresponding to the given UTxO identifier.

If the index has no existing entry for the specified identifier, the result
of applying this function will be equivalent to the identity function.
-}
delete :: forall u. Ord u => u -> UTxOIndex u -> UTxOIndex u
delete u i =
  maybe i updateIndex $ Map.lookup u $ universe i
 where
  updateIndex :: GYValue -> UTxOIndex u
  updateIndex v =
    i
      { balance = balance i `valueMonus` v -- Actually, `valueMinus` would have also been fine as we have already checked that this entry was added before.
      , universe = Map.delete u (universe i)
      , indexAll = deleteAssets (indexAll i)
      , indexSingletons =
          indexSingletons i
            & case valueCategory of
              ValueWithOneAsset {} -> deleteAssets
              _otherwise -> id
      , indexPairs =
          indexPairs i
            & case valueCategory of
              ValueWithTwoAssets {} -> deleteAssets
              _otherwise -> id
      }
   where
    valueAssets' :: Set GYAssetClass
    valueAssets' = valueAssets v

    valueCategory :: ValueCategory GYAssetClass
    valueCategory = categorizeValue v

    deleteAssets :: MonoidMap GYAssetClass (Set u) -> MonoidMap GYAssetClass (Set u)
    deleteAssets = flip (F.foldl' deleteAsset) valueAssets'
     where
      deleteAsset m a = MonoidMap.adjust (Set.delete u) a m

{- | Deletes multiple entries from an index.

See 'delete'.
-}
deleteMany :: (Foldable f, Ord u) => f u -> UTxOIndex u -> UTxOIndex u
deleteMany = flip $ F.foldl' $ \i u -> delete u i

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

-- | Filters an index.
filter :: Ord u => (u -> Bool) -> UTxOIndex u -> UTxOIndex u
filter f = fromSequence . L.filter (f . fst) . toList

-- | Partitions an index.
partition :: Ord u => (u -> Bool) -> UTxOIndex u -> (UTxOIndex u, UTxOIndex u)
partition f = bimap fromSequence fromSequence . L.partition (f . fst) . toList

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Returns the complete set of all assets contained in an index.
assets :: UTxOIndex u -> Set GYAssetClass
assets = MonoidMap.nonNullKeys . indexAll

{- | Returns the value corresponding to the given UTxO identifier.

If the index has no such identifier, this function returns 'Nothing'.
-}
lookup :: Ord u => u -> UTxOIndex u -> Maybe GYValue
lookup u = Map.lookup u . universe

{- | Returns 'True' if (and only if) the index has an entry for the given UTxO
  identifier.
-}
member :: Ord u => u -> UTxOIndex u -> Bool
member u = isJust . lookup u

-- | Returns 'True' if (and only if) the index is empty.
null :: UTxOIndex u -> Bool
null = (== 0) . size

-- | Returns the total number of UTxO entries held within the index.
size :: UTxOIndex u -> Int
size = Map.size . universe

--------------------------------------------------------------------------------
-- Set operations
--------------------------------------------------------------------------------

{- | Creates a new index by subtracting the second index from the first.

This operation is fast if the intersection of the first and second indices
is small relative to the size of the first index.
-}
difference :: Ord u => UTxOIndex u -> UTxOIndex u -> UTxOIndex u
difference a b
  | disjoint a b = a
  | otherwise = deleteMany keysToDelete a
 where
  keysToDelete =
    Set.intersection
      (Map.keysSet (universe a))
      (Map.keysSet (universe b))

-- | Indicates whether a pair of UTxO indices are disjoint.
disjoint :: Ord u => UTxOIndex u -> UTxOIndex u -> Bool
disjoint i1 i2 = universe i1 `Map.disjoint` universe i2

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

-- | Specifies a filter for selecting UTxO entries.
data SelectionFilter asset
  = -- | Matches UTxOs that contain only the given asset and no other assets.
    SelectSingleton asset
  | -- | Matches UTxOs that contain the given asset and exactly one other
    -- asset.
    SelectPairWith asset
  | -- | Matches UTxOs that contain the given asset and any number of other
    -- assets.
    SelectAnyWith asset
  | -- | Matches all UTxOs regardless of what assets they contain.
    SelectAny
  deriving (Eq, Foldable, Functor, Show, Traversable)

{- | Selects an entry at random from the index according to the given filter.

Returns the selected entry and an updated index with the entry removed.

Returns 'Nothing' if there were no matching entries.
-}
selectRandom ::
  forall m u.
  (MonadRandom m, Ord u) =>
  UTxOIndex u ->
  SelectionFilter GYAssetClass ->
  m (Maybe ((u, GYValue), UTxOIndex u))
selectRandom i selectionFilter =
  (lookupAndRemoveEntry =<<) <$> selectRandomSetMember selectionSet
 where
  lookupAndRemoveEntry :: u -> Maybe ((u, GYValue), UTxOIndex u)
  lookupAndRemoveEntry u =
    (\v -> ((u, v), delete u i)) <$> Map.lookup u (universe i)

  selectionSet :: Set u
  selectionSet = case selectionFilter of
    SelectSingleton a ->
      a `lookupWith` indexSingletons
    SelectPairWith a ->
      a `lookupWith` indexPairs
    SelectAnyWith a ->
      a `lookupWith` indexAll
    SelectAny ->
      Map.keysSet (universe i)
   where
    a `lookupWith` index = MonoidMap.get a $ index i

{- | Selects an entry at random from the index according to the given filters.

This function traverses the specified list of filters in descending order of
priority, from left to right.

When considering a particular filter:

   - if the function is able to select a UTxO entry that matches, it
     terminates with that entry and an updated index with the entry removed.

   - if the function is not able to select a UTxO entry that matches, it
     traverses to the next filter available.

This function returns 'Nothing' if (and only if) it traverses the entire
list of filters without successfully selecting a UTxO entry.
-}
selectRandomWithPriority ::
  (MonadRandom m, Ord u) =>
  UTxOIndex u ->
  -- | A list of selection filters to be traversed in descending order of
  -- priority, from left to right.
  NonEmpty (SelectionFilter GYAssetClass) ->
  m (Maybe ((u, GYValue), UTxOIndex u))
selectRandomWithPriority i =
  firstJustM (selectRandom i) . NE.toList

--------------------------------------------------------------------------------
-- Internal Interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Represents different categories of value.
data ValueCategory asset
  = ValueWithNoAssets
  | ValueWithOneAsset asset
  | ValueWithTwoAssets (asset, asset)
  | ValueWithMultipleAssets (Set asset)
  deriving (Eq, Show)

-- | Categorizes a value by how many assets it contains.
categorizeValue :: GYValue -> ValueCategory GYAssetClass
categorizeValue b = case F.toList valueAssets' of
  [] -> ValueWithNoAssets
  [a] -> ValueWithOneAsset a
  [a1, a2] -> ValueWithTwoAssets (a1, a2)
  _ -> ValueWithMultipleAssets valueAssets'
 where
  valueAssets' = valueAssets b

-- Inserts an entry, but without checking the following pre-condition:
--
-- Pre-condition: there is no existing entry for the specified UTxO identifier.
--
-- See 'insert' for a safe version of this function.
--
insertUnsafe ::
  forall u.
  Ord u =>
  u ->
  GYValue ->
  UTxOIndex u ->
  UTxOIndex u
insertUnsafe u v i =
  i
    { balance = balance i <> v
    , universe = Map.insert u v (universe i)
    , indexAll = insertAssets (indexAll i)
    , indexSingletons =
        indexSingletons i
          & case valueCategory of
            ValueWithOneAsset {} -> insertAssets
            _otherwise -> id
    , indexPairs =
        indexPairs i
          & case valueCategory of
            ValueWithTwoAssets {} -> insertAssets
            _otherwise -> id
    }
 where
  valueAssets' :: Set GYAssetClass
  valueAssets' = valueAssets v

  valueCategory :: ValueCategory GYAssetClass
  valueCategory = categorizeValue v

  insertAssets :: MonoidMap GYAssetClass (Set u) -> MonoidMap GYAssetClass (Set u)
  insertAssets = flip (F.foldl' insertAsset) valueAssets'
   where
    insertAsset m a = MonoidMap.adjust (Set.insert u) a m

{- | Selects an element at random from the given set.

Returns 'Nothing' if (and only if) the given set is empty.
-}
selectRandomSetMember ::
  MonadRandom m =>
  Set a ->
  m (Maybe a)
selectRandomSetMember s
  | Set.null s =
      pure Nothing
  | otherwise =
      Just . flip Set.elemAt s <$> getRandomR (0, Set.size s - 1)

--------------------------------------------------------------------------------
-- Invariant
--------------------------------------------------------------------------------

-- | The result of checking the invariant with the 'checkInvariant' function.
data InvariantStatus
  = -- | Indicates a successful check of the invariant.
    InvariantHolds
  | -- | Indicates that the cached 'balance' value is incorrect.
    InvariantBalanceError BalanceError
  | -- | Indicates that the 'index' is missing one or more entries.
    InvariantIndexIncomplete
  | -- | Indicates that the 'index' has one or more unnecessary entries.
    InvariantIndexNonMinimal
  | -- | Indicates that the index sets are not consistent.
    InvariantIndexInconsistent
  | -- | Indicates that the 'index' and the cached 'balance' value disagree
    --   about which assets are included.
    InvariantAssetsInconsistent
  deriving (Eq, Show)

-- | Checks whether or not the invariant holds.
checkInvariant :: Ord u => UTxOIndex u -> InvariantStatus
checkInvariant i
  | BalanceIncorrect balanceError <- checkBalance i =
      InvariantBalanceError balanceError
  | not (indexIsComplete i) =
      InvariantIndexIncomplete
  | not (indexIsMinimal i) =
      InvariantIndexNonMinimal
  | not (indexIsConsistent i) =
      InvariantIndexInconsistent
  | not (assetsConsistent i) =
      InvariantAssetsInconsistent
  | otherwise =
      InvariantHolds

-- | Indicates whether on not the stored 'balance' value is correct.
data BalanceStatus
  = BalanceCorrect
  | BalanceIncorrect BalanceError
  deriving (Eq, Show)

-- | Indicates that the stored 'balance' value is not correct.
data BalanceError = BalanceError
  { balanceComputed ::
      GYValue
  , balanceStored ::
      GYValue
  }
  deriving (Eq, Show)

{- | Checks that calculating the balance from scratch gives a result that
  is equal to the stored 'balance' value.
-}
checkBalance :: UTxOIndex u -> BalanceStatus
checkBalance i
  | balanceComputed == balanceStored =
      BalanceCorrect
  | otherwise =
      BalanceIncorrect $ BalanceError {balanceComputed, balanceStored}
 where
  balanceComputed = F.fold (universe i)
  balanceStored = balance i

-- | Checks that every entry in the 'universe' map is properly indexed.
indexIsComplete :: forall u. Ord u => UTxOIndex u -> Bool
indexIsComplete i =
  F.all hasEntry $ Map.toList $ universe i
 where
  hasEntry :: (u, GYValue) -> Bool
  hasEntry (u, b) = case categorizeValue b of
    ValueWithNoAssets ->
      True
    ValueWithOneAsset a ->
      and
        [ hasEntryForAsset a indexAll
        , hasEntryForAsset a indexSingletons
        ]
    ValueWithTwoAssets (a1, a2) ->
      and
        [ hasEntryForAsset a1 indexAll
        , hasEntryForAsset a2 indexAll
        , hasEntryForAsset a1 indexPairs
        , hasEntryForAsset a2 indexPairs
        ]
    ValueWithMultipleAssets as ->
      F.all (`hasEntryForAsset` indexAll) as
   where
    hasEntryForAsset ::
      Ord asset =>
      asset ->
      (UTxOIndex u -> MonoidMap asset (Set u)) ->
      Bool
    hasEntryForAsset asset assetsMap =
      Set.member u $ MonoidMap.get asset $ assetsMap i

{- | Checks that every indexed entry is required by some entry in the 'universe'
  map.
-}
indexIsMinimal :: forall u. Ord u => UTxOIndex u -> Bool
indexIsMinimal i =
  F.and
    [ indexAll i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasAsset a) u)
    , indexSingletons i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasOneAsset a) u)
    , indexPairs i
        & MonoidMap.toList
        & F.all (\(a, u) -> F.all (entryHasTwoAssetsWith a) u)
    ]
 where
  entryHasAsset :: GYAssetClass -> u -> Bool
  entryHasAsset a = entryMatches (`valueAssetPresent` a)

  entryHasOneAsset :: GYAssetClass -> u -> Bool
  entryHasOneAsset a = entryMatches $ \b ->
    and
      [ b `valueAssetPresent` a
      , valueTotalAssets b == 1
      ]

  entryHasTwoAssetsWith :: GYAssetClass -> u -> Bool
  entryHasTwoAssetsWith a = entryMatches $ \b ->
    and
      [ b `valueAssetPresent` a
      , valueTotalAssets b == 2
      ]

  entryMatches :: (GYValue -> Bool) -> u -> Bool
  entryMatches test u = maybe False test $ Map.lookup u $ universe i

-- | Checks that index set relationships are correct.
indexIsConsistent :: Ord u => UTxOIndex u -> Bool
indexIsConsistent i =
  F.and
    [ indexSingletons i
        `MonoidMap.disjoint` indexPairs i
    , indexSingletons i
        `MonoidMap.isSubmapOf` indexAll i
    , indexPairs i
        `MonoidMap.isSubmapOf` indexAll i
    ]

{- | Checks that the asset sets are consistent.

In particular, the set of assets in the cached 'balance' must be:

   - equal to the set of assets in 'indexAll'
   - a superset of the set of assets in 'indexSingletons'.
   - a superset of the set of assets in 'indexPairs'.
-}
assetsConsistent :: UTxOIndex u -> Bool
assetsConsistent i =
  and
    [ MonoidMap.nonNullKeys (indexAll i)
        == balanceAssets
    , MonoidMap.nonNullKeys (indexSingletons i)
        `Set.isSubsetOf` balanceAssets
    , MonoidMap.nonNullKeys (indexPairs i)
        `Set.isSubsetOf` balanceAssets
    ]
 where
  balanceAssets = valueAssets (balance i)
