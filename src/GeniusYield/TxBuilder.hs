{-|
Module      : GeniusYield.TxBuilder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder
    ( module X
    , buildTxBody
    , buildTxBodyWithStrategy
    , queryBalance
    , getAdaOnlyUTxO
    , adaOnlyUTxOPure
    , getCollateral'
    , getCollateral
    , getTxBalance
    ) where

import qualified Cardano.Api                     as Api
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set

import           GeniusYield.TxBuilder.Class     as X
import           GeniusYield.TxBuilder.Common    as X
import           GeniusYield.TxBuilder.Errors    as X
import           GeniusYield.TxBuilder.IO        as X

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.Types

-- | Query the balance at given address.
queryBalance :: GYTxQueryMonad m => GYAddress -> m GYValue
queryBalance addr = foldMapUTxOs utxoValue <$> utxosAtAddress addr Nothing

-- | Query the txoutrefs at given address with ADA-only values.
--
-- Useful for finding a txoutref to be used as collateral.
getAdaOnlyUTxO :: GYTxQueryMonad m => GYAddress -> m [(GYTxOutRef, Natural)]
getAdaOnlyUTxO addr = adaOnlyUTxOPure <$> utxosAtAddress addr Nothing

-- | Get a UTxO suitable for use as collateral.
--
getCollateral' :: GYTxQueryMonad m
               => GYAddress                       -- ^ The address where to look.
               -> Natural                         -- ^ The minimal amount of lovelace required as collateral.
               -> m (Maybe (GYTxOutRef, Natural)) -- ^ Returns the smallest ada-only UTxO and the contained amount of lovelace at the specified address with the specified minimal value. If no such UTxO exists, 'Nothing' is returned.
getCollateral' addr minCollateral = do
    xs <- filter (\(_, n) -> n >= minCollateral) <$> getAdaOnlyUTxO addr
    return $ case xs of
        [] -> Nothing
        ys -> Just $ minimumBy (compare `on` snd) ys

-- | Get an UTxO suitable for use as collateral.
--
getCollateral :: GYTxQueryMonad m
              => GYAddress               -- ^ The address where to look.
              -> Natural                 -- ^ The minimal amount of lovelace required as collateral.
              -> m (GYTxOutRef, Natural) -- ^ Returns the smallest ada-only UTxO and the contained amount of lovelace at the specified address with the specified minimal value. If no such UTxO exists, an exception is thrown.
getCollateral addr minCollateral = do
    mc <- getCollateral' addr minCollateral
    case mc of
        Nothing -> throwError $ GYNoSuitableCollateralException minCollateral addr
        Just x  -> return x

adaOnlyUTxOPure :: GYUTxOs -> [(GYTxOutRef, Natural)]
adaOnlyUTxOPure = Map.toList . mapMaybeUTxOs (valueIsPositiveAda . utxoValue)
  where
    valueIsPositiveAda :: GYValue -> Maybe Natural
    valueIsPositiveAda v = case valueSplitAda v of
        (n, v') | n > 0, isEmptyValue v' -> Just (fromInteger n)
        _                                -> Nothing

-- | Calculate how much balance is the given transaction
-- is moving to given pubkeyhash address(es).
getTxBalance :: GYTxQueryMonad m => GYPubKeyHash -> GYTx -> m GYValue
getTxBalance pkh tx = do
    let Api.TxBody content = Api.getTxBody $ txToApi tx
        ins      = txOutRefFromApi . fst <$> Api.txIns content
        outValue = mconcat [ valueFromApiTxOutValue v
                           | Api.TxOut a v _ _ <- Api.txOuts content
                           , isRelevantAddress $ addressFromApi' a
                           ]
    utxos <- utxosAtTxOutRefs ins
    let inValue = foldMapUTxOs f utxos
    return $ outValue `valueMinus` inValue
  where
    isRelevantAddress :: GYAddress -> Bool
    isRelevantAddress addr = Just pkh == addressToPubKeyHash addr

    f :: GYUTxO -> GYValue
    f utxo
        | isRelevantAddress $ utxoAddress utxo = utxoValue utxo
        | otherwise                            = mempty

-------------------------------------------------------------------------------
-- Transaction builder
-------------------------------------------------------------------------------

-- TODO: Maybe add utils like signAndSubmit, submitSkeleton etc.

buildTxBody :: forall v m. (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
            => GYTxSkeleton v
            -> m GYTxBody
buildTxBody = buildTxBodyWithStrategy GYRandomImproveMultiAsset

buildTxBodyWithStrategy :: forall v m. (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
                        => GYCoinSelectionStrategy
                        -> GYTxSkeleton v
                        -> m GYTxBody
buildTxBodyWithStrategy cstrat = fmap runIdentity . buildTxBodyF @Identity cstrat . Identity

{- | The most basic version of 'GYTxMonadIO' interpreter over a generic 'Traversable'.

== NOTE ==
This is not meant to be used with structures containing _multiple_ 'GYTxSkeleton's. As the balancer
will end up using the same utxos across the different txs.

Consider using 'buildTxBodyParallel' or 'buildTxBodyChaining' instead.
-}
buildTxBodyF :: forall f v m. (Traversable f, GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
             => GYCoinSelectionStrategy
             -> f (GYTxSkeleton v)
             -> m (f GYTxBody)
buildTxBodyF cstrat m = do
    x <- buildTxBodyCore (const id) cstrat [m]
    case x of
      GYTxBuildSuccess ne          -> pure $ NE.head ne
      GYTxBuildPartialSuccess be _ -> throwError . GYBuildTxException $ GYBuildTxBalancingError be
      GYTxBuildFailure be          -> throwError . GYBuildTxException $ GYBuildTxBalancingError be
      -- We know there is precisely one input.
      GYTxBuildNoInputs            -> error "runGYTxMonadIOF: absurd"

{- | A multi transaction building 'GYTxMonadIO' interpreter.

This does not perform chaining, i.e does not use utxos created by one of the given transactions in the next one.
However, it does ensure that the balancer does not end up using the same own utxos when building multiple
transactions at once.

This supports failure recovery by utilizing 'GYTxBuildResult'.
-}
buildTxBodyParallelF :: (Traversable f, GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
                     => GYCoinSelectionStrategy
                     -> [f (GYTxSkeleton v)]
                     -> m (GYTxBuildResult f)
buildTxBodyParallelF cstrat m = do
    buildTxBodyCore updateOwnUtxosParallel cstrat m

{- | A chaining transaction building 'GYTxMonadIO' interpreter.

This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.

This supports failure recovery by utilizing 'GYTxBuildResult'.

**EXPERIMENTAL**
-}
buildTxBodyChainingF :: (Traversable f, GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
                     => GYCoinSelectionStrategy
                     -> [f (GYTxSkeleton v)]
                     -> m (GYTxBuildResult f)
buildTxBodyChainingF cstrat m = do
    addrs <- ownAddresses
    buildTxBodyCore (updateOwnUtxosChaining $ Set.fromList addrs) cstrat m

{- | The core implementation of buildTxBody: Building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

See 'buildTxBodyF' for an example which _does not update_ used up own utxos for multi 'GYTxSkeleton' builds.

See 'buildTxBodyParallel' for an example which  _removes_ used up own utxos for next 'GYTxSkeleton's (if any).

See 'buildTxBodyChaining' for an example which _removes_ used up own utxos, **and** _adds_ newly created utxos addressed to
own wallet, for next 'GYTxSkeleton's (if any).

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
buildTxBodyCore
    :: forall f m v. (Traversable f, GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m)
    => (GYTxBody -> GYUTxOs -> GYUTxOs)           -- ^ Function governing how to update UTxO set when building for multiple skeletons.
    -> GYCoinSelectionStrategy                    -- ^ Coin selection strategy.
    -> [f (GYTxSkeleton v)]                       -- ^ Skeleton(s).
    -> m (GYTxBuildResult f)
buildTxBodyCore ownUtxoUpdateF cstrat skeletons = do
    logSkeletons skeletons

    -- Obtain constant parameters to be used across several 'GYTxBody' generations.
    ss    <- systemStart
    eh    <- eraHistory
    apiPp <- protocolParams
    ps    <- stakePools

    pp <- case Api.toLedgerPParams Api.ShelleyBasedEraBabbage apiPp of
        Left e   -> throwError . GYBuildTxException $ GYBuildTxPPConversionError e
        Right pp -> pure pp

    collateral <- ownCollateral
    addrs <- ownAddresses
    change <- ownChangeAddress

    e <- buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change collateral skeletons
    case e of
        Left err  -> throwError $ GYBuildTxException err
        Right res -> pure res

    where
      logSkeletons :: [f (GYTxSkeleton v)] -> m ()
      logSkeletons = mapM_ (mapM_ (logMsg "buildTxBody" GYDebug . show))

-- | Update own utxo set by removing any utxos used up in the given tx.
updateOwnUtxosParallel :: GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosParallel txBody = utxosRemoveTxOutRefs (Set.fromList txIns)
  where
    txIns = txBodyTxIns txBody

{- | Update own utxo set by removing any utxos used up in the given tx,
**and** adding newly created utxos addressed to own wallet. -}
updateOwnUtxosChaining :: Set GYAddress -> GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosChaining ownAddrs txBody utxos = utxosRemoveTxOutRefs (Set.fromList txIns) utxos <> txOutsOwn
  where
    txIns = txBodyTxIns txBody
    txOuts = txBodyUTxOs txBody
    txOutsOwn = filterUTxOs (\GYUTxO {utxoAddress} -> utxoAddress `Set.member` ownAddrs) txOuts
