{-|
Module      : GeniusYield.TxBuilder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder
    ( module X
    , queryBalance
    , getAdaOnlyUTxO
    , adaOnlyUTxOPure
    , getCollateral'
    , getCollateral
    , getTxBalance
    ) where

import qualified Cardano.Api                     as Api
import qualified Data.Map.Strict                 as Map

import           GeniusYield.TxBuilder.Class     as X
import           GeniusYield.TxBuilder.Common    as X
import           GeniusYield.TxBuilder.Errors    as X
import           GeniusYield.TxBuilder.Node      as X
import           GeniusYield.TxBuilder.NodeQuery as X
import           GeniusYield.TxBuilder.Random    as X
import           GeniusYield.TxBuilder.Clb       as X

import           GeniusYield.Imports
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
