{- |
Module      : GeniusYield.Types.DataStore
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.DataStore (
  GYDataStore (..),
  fetchFromDataStore,
  GYDataStoreVar,
  mkDataStoreVar,
) where

import Control.Concurrent.Class.MonadMVar.Strict (
  StrictMVar,
  modifyMVar,
  newMVar,
 )
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

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

type GYDataStoreVar a = StrictMVar IO (GYDataStore a)

fetchFromDataStore :: StrictMVar IO (GYDataStore a) -> IO a
fetchFromDataStore var = do
  -- See note: [Caching and concurrently accessible MVars].
  modifyMVar var $ \GYDataStore {..} -> do
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

mkDataStoreVar :: GYDataStore a -> IO (GYDataStoreVar a)
mkDataStoreVar = newMVar
