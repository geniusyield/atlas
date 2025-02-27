{- |
Module      : GeniusYield.Providers.CacheMempool
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.CacheMempool (
  augmentQueryUTxOWithMempool,
) where

import Control.AutoUpdate (
  UpdateSettings (..),
  defaultUpdateSettings,
  mkAutoUpdate,
 )
import Control.Concurrent.Class.MonadMVar.Strict (
  StrictMVar,
  modifyMVar,
  newMVar,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GeniusYield.Imports (Set, foldl', (&))
import GeniusYield.Types

-- TODO: Move datastore to it's own module.

-- | A data store to cache data and refetch it after a certain time.
data GYDataStore a = GYDataStore
  { gyDataStoreRefetchTime :: !UTCTime
  -- ^ The time after which the data should be refetched.
  , gyDataStoreCacheInterval :: !NominalDiffTime
  -- ^ The interval to cache the data so as to avoid querying with high frequency.
  , gyDataStoreData :: !a
  -- ^ Cached data.
  , gyDataStoreRefetchData :: !(IO a)
  -- ^ The action to refetch the data.
  , gyDataStoreGetTime :: !(IO UTCTime)
  -- ^ The action to get the current time.
  }

fetchFromDataStore :: StrictMVar IO (GYDataStore a) -> IO a
fetchFromDataStore var = do
  -- See note: [Caching and concurrently accessible MVars].
  modifyMVar var $ \(GYDataStore {..}) -> do
    now <- gyDataStoreGetTime
    if now < gyDataStoreRefetchTime
      then do
        -- Return unmodified.
        pure (GYDataStore {..}, gyDataStoreData)
      else do
        newDataVal <- gyDataStoreRefetchData
        newNow <- gyDataStoreGetTime
        let newRefetchTime = addUTCTime gyDataStoreCacheInterval newNow
        pure (GYDataStore newRefetchTime gyDataStoreCacheInterval newDataVal gyDataStoreRefetchData gyDataStoreGetTime, newDataVal)

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
  dataStoreRef <- newMVar $ GYDataStore mempoolRefetchTime cacheMempoolTxsInterval initMempoolTxs fetchMempoolTxs getTime
  pure $ augmentQueryUTxOWithMempool' queryUtxo dataStoreRef

-- | Augment the query with mempool transactions.
augmentQueryUTxOWithMempool' ::
  -- | The query to augment.
  GYQueryUTxO ->
  -- | 'GYDataStore' to fetch mempool transactions.
  StrictMVar IO (GYDataStore [GYTx]) ->
  GYQueryUTxO
augmentQueryUTxOWithMempool' GYQueryUTxO {..} fetchMempoolTxsDataStore = do
  GYQueryUTxO
    { gyQueryUtxosAtTxOutRefsWithDatums' = case gyQueryUtxosAtTxOutRefsWithDatums' of
        Nothing -> gyQueryUtxosAtTxOutRefsWithDatums'
        Just f -> Just $ \refs -> do
          mempoolOuts <- splitMempoolTxsDataStoreOuts fetchMempoolTxsDataStore
          foundOuts <- f refs
          let mempoolFoundOuts = Map.restrictKeys mempoolOuts (Set.fromList refs)
              foundOutsMap = outsWithDatumsToMap foundOuts
          pure $ Map.elems $ Map.union foundOutsMap mempoolFoundOuts
    , gyQueryUtxosAtTxOutRefs' = \refs -> do
        mempoolOuts <- splitMempoolTxsDataStoreOuts fetchMempoolTxsDataStore
        foundOuts <- gyQueryUtxosAtTxOutRefs' refs
        let mempoolFoundOuts = Map.restrictKeys mempoolOuts (Set.fromList refs) & Map.elems & map fst & utxosFromList
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredsWithDatums' = case gyQueryUtxosAtPaymentCredsWithDatums' of
        Nothing -> gyQueryUtxosAtPaymentCredsWithDatums'
        Just f -> Just $ \creds -> do
          mempoolOuts <- splitMempoolTxsDataStoreOuts fetchMempoolTxsDataStore
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
        mempoolOuts <- splitMempoolTxsDataStoreOuts fetchMempoolTxsDataStore
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
                & Map.elems
                & map fst
                & utxosFromList
        pure $ foundOuts <> mempoolFoundOuts
    , gyQueryUtxosAtPaymentCredential' = undefined
    , gyQueryUtxosAtPaymentCredWithDatums' = undefined
    , gyQueryUtxosAtAddressesWithDatums' = undefined
    , gyQueryUtxosAtAddresses' = undefined
    , gyQueryUtxosAtAddressWithDatums' = undefined
    , gyQueryUtxosAtAddress' = undefined
    , gyQueryUtxoRefsAtAddress' = undefined
    , gyQueryUtxoAtTxOutRef' = undefined
    }

outsWithDatumsToMap :: [(GYUTxO, Maybe GYDatum)] -> Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum)
outsWithDatumsToMap = Map.fromList . map (\(utxo, mdatum) -> (utxoRef utxo, (utxo, mdatum)))

splitMempoolTxsDataStoreOuts :: StrictMVar IO (GYDataStore [GYTx]) -> IO (Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))
splitMempoolTxsDataStoreOuts mempoolDataStore = do
  mempoolTxs <- fetchFromDataStore mempoolDataStore
  pure $ splitMempoolTxsOuts mempoolTxs

splitMempoolTxsOuts :: [GYTx] -> Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum)
splitMempoolTxsOuts txs = snd $ splitMempoolTxsInsOuts txs

-- | Split the found mempool transactions into inputs & outputs.
splitMempoolTxsInsOuts :: [GYTx] -> (Set GYTxOutRef, Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))
splitMempoolTxsInsOuts = foldl' f (mempty, mempty)
 where
  f (!ins, !outsWithDatums) tx =
    let txBody = getTxBody tx
        !ins' = Set.fromList $ txBodyTxIns txBody
        !outsWithDatums' = Map.fromList $ map (\(utxo, mdatum) -> (utxoRef utxo, (utxo, mdatum))) $ txBodyUTxOsWithDatums txBody
     in (Set.union ins ins', Map.union outsWithDatums' outsWithDatums)