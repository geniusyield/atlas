{- |
Module      : GeniusYield.TxBuilder.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.TxBuilder.Common (
  GYTxSkeleton (..),
  GYTxSkeletonRefIns (..),
  emptyGYTxSkeleton,
  gyTxSkeletonRefInsToList,
  gyTxSkeletonRefInsSet,
  GYTxBuildResult (..),
  buildTxCore,
  collateralLovelace,
  collateralValue,
  maximumRequiredCollateralLovelace,
  maximumRequiredCollateralValue,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger (unboundRational)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Conway.Tx qualified as Ledger
import Cardano.Ledger.Plutus qualified as Ledger
import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Random (MonadRandom)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.Transaction.Common (
  minimumUTxO,
  utxoFromTxInDetailed,
 )
import GeniusYield.TxBuilder.Errors
import GeniusYield.TxBuilder.Query.Class
import GeniusYield.Types
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)

-------------------------------------------------------------------------------
-- Transaction skeleton
-------------------------------------------------------------------------------

{- | Transaction skeleton

/Note:/ let's add fields as we need them.

The parameter @v@ indicates the minimum version of scripts allowed
as inputs.
-}
data GYTxSkeleton (v :: PlutusVersion) = GYTxSkeleton
  { gytxIns :: ![GYTxIn v]
  , gytxOuts :: ![GYTxOut v]
  , gytxRefIns :: !(GYTxSkeletonRefIns v)
  , gytxMint :: !(Map (GYMintScript v) (Map GYTokenName Integer, GYRedeemer))
  , gytxWdrls :: ![GYTxWdrl v]
  , gytxSigs :: !(Set GYPubKeyHash)
  , gytxCerts :: ![GYTxCert v]
  , gytxInvalidBefore :: !(Maybe GYSlot)
  , gytxInvalidAfter :: !(Maybe GYSlot)
  , gytxMetadata :: !(Maybe GYTxMetadata)
  }
  deriving (Show)

data GYTxSkeletonRefIns :: PlutusVersion -> Type where
  GYTxSkeletonRefIns :: (VersionIsGreaterOrEqual v 'PlutusV2) => !(Set GYTxOutRef) -> GYTxSkeletonRefIns v
  GYTxSkeletonNoRefIns :: GYTxSkeletonRefIns v

deriving instance Show (GYTxSkeletonRefIns v)
deriving instance Eq (GYTxSkeletonRefIns v)

gyTxSkeletonRefInsToList :: GYTxSkeletonRefIns v -> [GYTxOutRef]
gyTxSkeletonRefInsToList = Set.toList . gyTxSkeletonRefInsSet

gyTxSkeletonRefInsSet :: GYTxSkeletonRefIns v -> Set GYTxOutRef
gyTxSkeletonRefInsSet (GYTxSkeletonRefIns xs) = xs
gyTxSkeletonRefInsSet GYTxSkeletonNoRefIns = Set.empty

instance Semigroup (GYTxSkeletonRefIns v) where
  GYTxSkeletonRefIns a <> GYTxSkeletonRefIns b = GYTxSkeletonRefIns (Set.union a b)
  GYTxSkeletonRefIns a <> GYTxSkeletonNoRefIns = GYTxSkeletonRefIns a
  GYTxSkeletonNoRefIns <> GYTxSkeletonRefIns b = GYTxSkeletonRefIns b
  GYTxSkeletonNoRefIns <> GYTxSkeletonNoRefIns = GYTxSkeletonNoRefIns

emptyGYTxSkeleton :: GYTxSkeleton v
emptyGYTxSkeleton =
  GYTxSkeleton
    { gytxIns = []
    , gytxOuts = []
    , gytxRefIns = GYTxSkeletonNoRefIns
    , gytxMint = Map.empty
    , gytxWdrls = []
    , gytxSigs = Set.empty
    , gytxCerts = []
    , gytxInvalidBefore = Nothing
    , gytxInvalidAfter = Nothing
    , gytxMetadata = Nothing
    }

instance Semigroup (GYTxSkeleton v) where
  x <> y =
    GYTxSkeleton
      { gytxIns = combineIns (gytxIns x) (gytxIns y)
      , gytxOuts = gytxOuts x ++ gytxOuts y
      , gytxRefIns = gytxRefIns x <> gytxRefIns y
      , gytxMint = combineMint (gytxMint x) (gytxMint y)
      , gytxWdrls = combineWdrls (gytxWdrls x) (gytxWdrls y)
      , gytxSigs = Set.union (gytxSigs x) (gytxSigs y)
      , gytxCerts = gytxCerts x <> gytxCerts y
      , gytxInvalidBefore = combineInvalidBefore (gytxInvalidBefore x) (gytxInvalidBefore y)
      , gytxInvalidAfter = combineInvalidAfter (gytxInvalidAfter x) (gytxInvalidAfter y)
      , gytxMetadata = gytxMetadata x <> gytxMetadata y
      }
    where
      -- we keep only one input per utxo to spend
      combineIns u v = nubBy ((==) `on` gyTxInTxOutRef) (u ++ v)
      -- we cannot combine redeemers, so we just pick first.
      combineMint = Map.unionWith (\(amt, r) (amt', _r) -> (Map.unionWith (+) amt amt', r))
      -- we keep only one withdrawal per stake address
      combineWdrls u v = nubBy ((==) `on` gyTxWdrlStakeAddress) (u ++ v)

      combineInvalidBefore :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
      combineInvalidBefore m Nothing = m
      combineInvalidBefore Nothing n = n
      combineInvalidBefore (Just s) (Just t) = Just (max s t)

      combineInvalidAfter :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
      combineInvalidAfter m Nothing = m
      combineInvalidAfter Nothing n = n
      combineInvalidAfter (Just s) (Just t) = Just (min s t)

instance Monoid (GYTxSkeleton v) where
  mempty = emptyGYTxSkeleton

-------------------------------------------------------------------------------
-- Transaction building
-------------------------------------------------------------------------------

{- | Result of building 'GYTxBody's with the option of recovery from error.

Consider the act of building five 'GYTxSkeleton's into 'GYTxBody's. If three out of the five succeed, but the next
one fails due to insufficient funds - this type facilitates recovering the three rather than failing outright and discarding
the results.
-}
data GYTxBuildResult
  = -- | All given 'GYTxSkeleton's were successfully built.
    GYTxBuildSuccess !(NonEmpty GYTxBody)
  | -- | Some of the given 'GYTxSkeleton's were successfully built, but the rest failed due to _insufficient funds_.
    GYTxBuildPartialSuccess !GYBalancingError !(NonEmpty GYTxBody)
  | -- | None of the given 'GYTxSkeleton's could be built due to _insufficient funds_.
    GYTxBuildFailure !GYBalancingError
  | -- | Input did not contain any 'GYTxSkeleton's.
    GYTxBuildNoInputs

{- | The core implementation of 'GYTxQueryMonad' for building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
buildTxCore ::
  forall m v.
  (GYTxQueryMonad m, MonadRandom m) =>
  Api.SystemStart ->
  Api.EraHistory ->
  ApiProtocolParameters ->
  Set Api.S.PoolId ->
  GYCoinSelectionStrategy ->
  (GYTxBody -> GYUTxOs -> GYUTxOs) ->
  [GYAddress] ->
  GYAddress ->
  -- | Is `Nothing` if there was no 5 ada collateral returned by browser wallet.
  Maybe GYUTxO ->
  [GYTxSkeleton v] ->
  m (Either GYBuildTxError GYTxBuildResult)
buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change reservedCollateral skeletons = do
  ownUtxos <- utxosAtAddresses addrs

  let buildEnvWith ownUtxos' refIns collateralUtxo =
        GYBuildTxEnv
          { gyBTxEnvSystemStart = ss
          , gyBTxEnvEraHistory = eh
          , gyBTxEnvProtocolParams = pp
          , gyBTxEnvPools = ps
          , gyBTxEnvOwnUtxos = utxosRemoveTxOutRefs refIns $ utxosRemoveRefScripts $ maybe ownUtxos' ((`utxosRemoveTxOutRef` ownUtxos') . utxoRef) reservedCollateral
          , gyBTxEnvChangeAddr = change
          , gyBTxEnvCollateral = collateralUtxo
          }

      helper :: GYUTxOs -> GYTxSkeleton v -> m (Either GYBuildTxError GYTxBody)
      helper ownUtxos' GYTxSkeleton {..} = do
        let gytxMint' :: Maybe (GYValue, [(GYMintScript v, GYRedeemer)])
            gytxMint'
              | null gytxMint = Nothing
              | otherwise =
                  Just
                    ( valueFromList [(GYToken (mintingPolicyIdFromWitness mp) tn, n) | (mp, (tokens, _)) <- itoList gytxMint, (tn, n) <- itoList tokens]
                    , [(mp, redeemer) | (mp, (_, redeemer)) <- itoList gytxMint]
                    )

        let refIns =
              gyTxSkeletonRefInsToList gytxRefIns
                <> [r | GYTxIn {gyTxInWitness = GYTxInWitnessScript (GYInReference r _) _ _} <- gytxIns]
                <> [r | GYMintReference r _ <- Map.keys gytxMint]
        allRefUtxos <-
          utxosAtTxOutRefs $
            (gyTxInTxOutRef <$> gytxIns)
              <> refIns
        refInsUtxos <- forM refIns $ \refIn -> do
          case utxosLookup refIn allRefUtxos of
            Nothing -> throwError . GYQueryUTxOException $ GYNoUtxoAtRef refIn
            Just u -> pure u
        -- Convert the 'GYTxIn's to 'GYTxInDetailed's from fetched chain information about them.
        gyTxInsDetailed <- forM gytxIns $ \gyTxIn -> do
          let ref = gyTxInTxOutRef gyTxIn
          case utxosLookup ref allRefUtxos of
            Nothing -> throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref
            Just GYUTxO {utxoAddress, utxoValue, utxoRefScript, utxoOutDatum} ->
              if checkDatumMatch utxoOutDatum $ gyTxInWitness gyTxIn
                then
                  pure $
                    GYTxInDetailed gyTxIn utxoAddress utxoValue utxoOutDatum utxoRefScript
                else throwError $ GYDatumMismatch utxoOutDatum gyTxIn
              where
                checkDatumMatch _ GYTxInWitnessKey = True
                checkDatumMatch _ GYTxInWitnessSimpleScript {} = True
                checkDatumMatch ud (GYTxInWitnessScript _ wd _) = case ud of
                  GYOutDatumNone -> False
                  GYOutDatumHash h -> h == hashDatum wd
                  GYOutDatumInline uid -> uid == wd

        -- This operation is `O(n)` where `n` denotes the number of UTxOs in `ownUtxos'`.
        let totalRefScriptSize = foldl' (\acc GYUTxO {..} -> acc + maybe 0 scriptSize utxoRefScript) 0 $ refInsUtxos <> map utxoFromTxInDetailed gyTxInsDetailed
            maximumRequiredCollateralValue' = maximumRequiredCollateralValue pp totalRefScriptSize
            mCollateralUtxo =
              reservedCollateral
                <|> find
                  ( \u ->
                      let v = utxoValue u
                          -- Following depends on that we allow unsafe, i.e., negative coins count below. In future, we can take magnitude instead.
                          vWithoutMaxCollPledge = v `valueMinus` maximumRequiredCollateralValue'
                          worstCaseCollOutput = mkGYTxOutNoDatum change vWithoutMaxCollPledge
                       in -- @vWithoutMaxCollPledge@ should satisfy minimum ada requirement.

                          v `valueGreaterOrEqual` maximumRequiredCollateralValue'
                            && minimumUTxO pp worstCaseCollOutput <= fromInteger (valueAssetClass vWithoutMaxCollPledge GYLovelace)
                  ) -- Keeping it simple.
                  (utxosToList ownUtxos')

        case mCollateralUtxo of
          Nothing -> return (Left GYBuildTxNoSuitableCollateral)
          Just collateralUtxo ->
            -- Build the transaction.
            buildUnsignedTxBody
              (buildEnvWith ownUtxos' (Set.fromList refIns) collateralUtxo)
              cstrat
              gyTxInsDetailed
              gytxOuts
              (utxosFromList refInsUtxos)
              gytxMint'
              gytxWdrls
              gytxCerts
              gytxInvalidBefore
              gytxInvalidAfter
              gytxSigs
              gytxMetadata

      go :: GYUTxOs -> GYTxBuildResult -> [GYTxSkeleton v] -> m (Either GYBuildTxError GYTxBuildResult)
      go _ acc [] = pure $ Right $ reverseResult acc
      go ownUtxos' acc (skeleton : rest) = do
        res <- helper ownUtxos' skeleton
        case res of
          {- Not enough funds for this transaction
          We assume it's not worth continuing with the next transactions (which is often the case) -}
          Left (GYBuildTxBalancingError be) -> pure $ Right $ reverseResult $ updateBuildRes (Left be) acc
          -- Any other exception is fatal. TODO: To think more on whether collateral error can be handled here.
          Left err -> pure $ Left err
          Right body -> do
            -- Update the available utxos set by user supplied function.
            let ownUTxos'' = ownUtxoUpdateF body ownUtxos'
            -- Continue with an updated accumulator (set of built results).
            go ownUTxos'' (updateBuildRes (Right body) acc) rest
  go ownUtxos GYTxBuildNoInputs skeletons
  where
    {- This function updates 'GYTxBuildResult' based on a build outcome

    In case of insufficient funds failure ('Left' argument):
      We return either 'GYTxBuildFailure' or 'GYTxBuildPartialSuccess'
      Depending on whether or not any previous transactions were built succesfully.

    In case of successful build:
      We save the newly built tx body into the existing ones (if any)

    It's impossible for the second argument to ever be 'GYTxBuildFailure' or 'GYTxBuildPartialSuccess', as
    the outer function 'go' (see above) always exits as soon as the accumulator updates to one of these.
    -}
    updateBuildRes (Left v) GYTxBuildNoInputs = GYTxBuildFailure v
    updateBuildRes (Left v) (GYTxBuildSuccess ne) = GYTxBuildPartialSuccess v ne
    updateBuildRes (Right x) GYTxBuildNoInputs = GYTxBuildSuccess (x :| [])
    updateBuildRes (Right x) (GYTxBuildSuccess ne) = GYTxBuildSuccess (NE.cons x ne)
    updateBuildRes _ _ = error "buildTxCore.flippedFoldM.updateBuildRes: absurd"

    -- TODO: Move to @Data.Sequence.NonEmpty@?
    -- \| To reverse the final non-empty list built.
    reverseResult :: GYTxBuildResult -> GYTxBuildResult
    reverseResult (GYTxBuildSuccess ne) = GYTxBuildSuccess $ NE.reverse ne
    reverseResult (GYTxBuildPartialSuccess v ne) = GYTxBuildPartialSuccess v $ NE.reverse ne
    reverseResult anyOther = anyOther

collateralLovelace :: Integer
collateralLovelace = 5_000_000

collateralValue :: GYValue
collateralValue = valueFromLovelace collateralLovelace

{-# INLINEABLE maximumRequiredCollateralLovelace #-}

-- | What is the maximum possible collateral requirement as per current protocol parameters?
maximumRequiredCollateralLovelace :: ApiProtocolParameters -> Int -> Integer
maximumRequiredCollateralLovelace pp refScriptSize = ceiling $ fromIntegral (maximumFee pp refScriptSize) * ((pp ^. Ledger.ppCollateralPercentageL) % 100)

{-# INLINEABLE maximumFee #-}

-- | Compute the maximum fee possible for any transaction.
maximumFee :: ApiProtocolParameters -> Int -> Integer
maximumFee pp refScriptSize =
  let txFee :: Integer
      txFee = fromIntegral $ pp ^. Ledger.ppMinFeeBL + (pp ^. Ledger.ppMinFeeAL) * fromIntegral (pp ^. Ledger.ppMaxTxSizeL)
      executionFee :: Rational
      executionFee =
        case (pp ^. Ledger.ppPricesL, pp ^. Ledger.ppMaxTxExUnitsL) of
          (Ledger.Prices {..}, Ledger.ExUnits {..}) ->
            Ledger.unboundRational prSteps * fromIntegral exUnitsSteps + Ledger.unboundRational prMem * fromIntegral exUnitsMem
      refScriptFee = Ledger.tierRefScriptFee 1.2 25_600 (unboundRational $ pp ^. Ledger.ppMinFeeRefScriptCostPerByteL) refScriptSize
   in txFee + ceiling executionFee + Ledger.unCoin refScriptFee

{-# INLINEABLE maximumRequiredCollateralValue #-}

-- | See `maximumRequiredCollateralLovelace`.
maximumRequiredCollateralValue :: ApiProtocolParameters -> Int -> GYValue
maximumRequiredCollateralValue pp refScriptSize = valueFromLovelace $ maximumRequiredCollateralLovelace pp refScriptSize
