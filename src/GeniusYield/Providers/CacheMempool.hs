{- |
Module      : GeniusYield.Providers.CacheMempool
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.CacheMempool (
  augmentQueryUTxOWithMempool,
  augmentQueryUTxO,
  splitTxsInsOuts,
  splitTxsOutsModuloIns,
  outsWithDatumsToMap,
  outsWithDatumsMapToOuts,
) where

import Control.Applicative ((<|>))
import Control.AutoUpdate (
  UpdateSettings (..),
  defaultUpdateSettings,
  mkAutoUpdate,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GeniusYield.Imports (Set, foldl', (&), (<&>))
import GeniusYield.Types

-- | Augment the query with mempool transactions.
augmentQueryUTxOWithMempool ::
  -- | The query to augment.
  GYQueryUTxO ->
  -- | The function to fetch mempool transactions.
  IO [GYTx] ->
  -- | The interval to cache the mempool transactions so as to avoid querying mempool with high frequency.
  NominalDiffTime ->
  IO GYQueryUTxO
augmentQueryUTxOWithMempool queryUtxo fetchMempoolTxs cacheMempoolTxsInterval = do
  getTime <- mkAutoUpdate defaultUpdateSettings {updateAction = getCurrentTime}
  mempoolRefetchTime <- addUTCTime cacheMempoolTxsInterval <$> getTime
  initMempoolTxs <- fetchMempoolTxs
  dataStoreRef <- mkDataStoreVar $ GYDataStore mempoolRefetchTime cacheMempoolTxsInterval initMempoolTxs fetchMempoolTxs getTime
  pure $ augmentQueryUTxOWithMempool' queryUtxo dataStoreRef

-- | Augment the query with mempool transactions.
augmentQueryUTxOWithMempool' ::
  -- | The query to augment.
  GYQueryUTxO ->
  -- | 'GYDataStore' to fetch mempool transactions.
  GYDataStoreVar [GYTx] ->
  GYQueryUTxO
augmentQueryUTxOWithMempool' q fetchMempoolTxsDataStore = augmentQueryUTxO q fetchMempoolTxsDataStore splitMempoolTxsDataStoreOutsModuloIns

-- | Augment the query with mempool transactions.
augmentQueryUTxO ::
  -- | The query to augment.
  GYQueryUTxO ->
  -- | Placeholder for a particular data store.
  a ->
  -- | Function fetch outputs (modulo inputs) from the data store.
  (a -> IO (Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))) ->
  GYQueryUTxO
augmentQueryUTxO GYQueryUTxO {..} dataStore fetchOutsModuloIns = do
  GYQueryUTxO
    { gyQueryUtxosAtTxOutRefsWithDatums' = case gyQueryUtxosAtTxOutRefsWithDatums' of
        Nothing -> gyQueryUtxosAtTxOutRefsWithDatums'
        Just f -> Just $ \refs -> do
          mempoolOuts <- fetchOutsModuloIns dataStore
          foundOuts <- f refs
          let mempoolFoundOuts = Map.restrictKeys mempoolOuts (Set.fromList refs)
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtTxOutRefs' = \refs -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosAtTxOutRefs' refs
        let mempoolFoundOuts = Map.restrictKeys mempoolOuts (Set.fromList refs) & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredsWithDatums' = case gyQueryUtxosAtPaymentCredsWithDatums' of
        Nothing -> gyQueryUtxosAtPaymentCredsWithDatums'
        Just f -> Just $ \creds -> do
          mempoolOuts <- fetchOutsModuloIns dataStore
          foundOuts <- f creds
          let mempoolFoundOuts =
                Map.filter
                  ( \(GYUTxO {..}, _mdatum) ->
                      let
                        mpc = addressToPaymentCredential utxoAddress
                       in
                        case mpc of
                          Nothing -> False
                          Just pc -> pc `elem` creds
                  )
                  mempoolOuts
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredentials' = \creds -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosAtPaymentCredentials' creds
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    let
                      mpc = addressToPaymentCredential utxoAddress
                     in
                      case mpc of
                        Nothing -> False
                        Just pc -> pc `elem` creds
                )
                mempoolOuts
                & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredential' = \cred massetClass -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosAtPaymentCredential' cred massetClass
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    let
                      mpc = addressToPaymentCredential utxoAddress
                     in
                      case mpc of
                        Nothing -> False
                        Just pc ->
                          pc == cred
                            && ( case massetClass of
                                  Nothing -> True
                                  Just ac -> Set.member ac (valueAssets utxoValue)
                               )
                )
                mempoolOuts
                & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredWithDatums' = case gyQueryUtxosAtPaymentCredWithDatums' of
        Nothing -> gyQueryUtxosAtPaymentCredWithDatums'
        Just f -> Just $ \cred massetClass -> do
          mempoolOuts <- fetchOutsModuloIns dataStore
          foundOuts <- f cred massetClass
          let mempoolFoundOuts =
                Map.filter
                  ( \(GYUTxO {..}, _mdatum) ->
                      let
                        mpc = addressToPaymentCredential utxoAddress
                       in
                        case mpc of
                          Nothing -> False
                          Just pc ->
                            pc == cred
                              && ( case massetClass of
                                    Nothing -> True
                                    Just ac -> Set.member ac (valueAssets utxoValue)
                                 )
                  )
                  mempoolOuts
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtAddressesWithDatums' = case gyQueryUtxosAtAddressesWithDatums' of
        Nothing -> gyQueryUtxosAtAddressesWithDatums'
        Just f -> Just $ \addrs -> do
          mempoolOuts <- fetchOutsModuloIns dataStore
          foundOuts <- f addrs
          let mempoolFoundOuts =
                Map.filter
                  ( \(GYUTxO {..}, _mdatum) ->
                      utxoAddress `elem` addrs
                  )
                  mempoolOuts
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtAddresses' = \addrs -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosAtAddresses' addrs
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    utxoAddress `elem` addrs
                )
                mempoolOuts
                & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtAddressWithDatums' = case gyQueryUtxosAtAddressWithDatums' of
        Nothing -> gyQueryUtxosAtAddressWithDatums'
        Just f -> Just $ \addr massetClass -> do
          mempoolOuts <- fetchOutsModuloIns dataStore
          foundOuts <- f addr massetClass
          let mempoolFoundOuts =
                Map.filter
                  ( \(GYUTxO {..}, _mdatum) ->
                      utxoAddress == addr
                        && ( case massetClass of
                              Nothing -> True
                              Just ac -> Set.member ac (valueAssets utxoValue)
                           )
                  )
                  mempoolOuts
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtAddress' = \addr massetClass -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosAtAddress' addr massetClass
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    utxoAddress == addr
                      && ( case massetClass of
                            Nothing -> True
                            Just ac -> Set.member ac (valueAssets utxoValue)
                         )
                )
                mempoolOuts
                & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosWithAsset' = \ac -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- gyQueryUtxosWithAsset' ac
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    valueAssetClass utxoValue (nonAdaTokenToAssetClass ac) > 0
                )
                mempoolOuts
                & outsWithDatumsMapToOuts
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxoRefsAtAddress' = \addr -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOuts <- Set.fromList <$> gyQueryUtxoRefsAtAddress' addr
        let mempoolFoundOuts =
              Map.filter
                ( \(GYUTxO {..}, _mdatum) ->
                    utxoAddress == addr
                )
                mempoolOuts
                & Map.keysSet
        pure $ Set.toList $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxoAtTxOutRef' = \ref -> do
        mempoolOuts <- fetchOutsModuloIns dataStore
        foundOut <- gyQueryUtxoAtTxOutRef' ref
        let mempoolFoundOut = Map.lookup ref mempoolOuts <&> fst
        pure $ foundOut <|> mempoolFoundOut
    }

outsWithDatumsToMap :: [(GYUTxO, Maybe GYDatum)] -> Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum)
outsWithDatumsToMap = Map.fromList . map (\(utxo, mdatum) -> (utxoRef utxo, (utxo, mdatum)))

outsWithDatumsMapToOuts :: Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum) -> GYUTxOs
outsWithDatumsMapToOuts = utxosFromList . map fst . Map.elems

splitMempoolTxsDataStoreOutsModuloIns :: GYDataStoreVar [GYTx] -> IO (Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))
splitMempoolTxsDataStoreOutsModuloIns mempoolDataStore = do
  mempoolTxs <- fetchFromDataStore mempoolDataStore
  pure $ splitTxsOutsModuloIns mempoolTxs

splitTxsOutsModuloIns :: [GYTx] -> Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum)
splitTxsOutsModuloIns txs =
  let (ins, outsWithDatumsMap) = splitTxsInsOuts txs
   in outsWithDatumsMap `Map.withoutKeys` ins

-- | Split the transactions into inputs & outputs.
splitTxsInsOuts :: [GYTx] -> (Set GYTxOutRef, Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))
splitTxsInsOuts = foldl' f (mempty, mempty)
 where
  f (!ins, !outsWithDatums) tx =
    let txBody = getTxBody tx
        !ins' = Set.fromList $ txBodyTxIns txBody
        !outsWithDatums' = Map.fromList $ map (\(utxo, mdatum) -> (utxoRef utxo, (utxo, mdatum))) $ txBodyUTxOsWithDatums txBody
     in (Set.union ins ins', Map.union outsWithDatums' outsWithDatums)
