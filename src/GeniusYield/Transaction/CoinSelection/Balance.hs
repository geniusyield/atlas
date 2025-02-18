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
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (
  ExceptT (ExceptT),
  except,
 )
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text.Class (
  ToText (toText),
  fromText,
 )
import GHC.IsList (fromList)
import GeniusYield.Imports
import GeniusYield.Transaction.CoinSelection.UTxOSelection (UTxOSelection)
import GeniusYield.Transaction.Common
import GeniusYield.Types
import GeniusYield.Utils

data ValueSizeAssessment
  = -- | Indicates that the size of a value does not exceed the maximum
    -- size that can be included in a transaction output.
    ValueSizeWithinLimit
  | -- | Indicates that the size of a value exceeds the maximum size
    -- that can be included in a transaction output.
    ValueSizeExceedsLimit
  deriving (Eq, Generic, Show)

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
      GYBalancedTx v ->
      Natural
  -- ^ Computes the minimum cost of a given selection skeleton.
  -- TODO: This can be cleverly used to not make use of logarithmic overestimation function. To study if it's only for fees or also for min deposits, if min deposit then which one?
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

-- | Specifies all parameters that are specific to a given selection.
data SelectionParams = SelectionParams
  { outputsToCover ::
      ![(GYAddress, GYValue)]
  -- ^ The complete set of outputs to be covered.
  -- TODO: Shall I be changing it's representation to say GYTxOut v? Or just GYValue?
  , utxoAvailable ::
      !(UTxOSelection GYTxOutRef)
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