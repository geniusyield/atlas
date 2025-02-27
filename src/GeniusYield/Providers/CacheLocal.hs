{- |
Module      : GeniusYield.Providers.CacheLocal
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.CacheLocal (
  LocallySubmittedTxs,
  mkLocallySubmittedTxsVar,
  augmentTxSubmission,
  augmentQueryUTxOWithLocalSubmission,
) where

import Control.Concurrent.Class.MonadMVar.Strict (
  StrictMVar,
  modifyMVar,
  newMVar,
  readMVar,
 )
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import GeniusYield.Providers.CacheMempool (augmentQueryUTxO, splitTxsOutsModuloIns)
import GeniusYield.Types

type LocallySubmittedTxs = StrictMVar IO ([(GYTx, UTCTime)], NominalDiffTime)

mkLocallySubmittedTxsVar ::
  -- | The time interval until which previously submitted transactions would be kept. The list of submitted transactions is cleaned whenever a new transaction is submitted.
  NominalDiffTime ->
  IO LocallySubmittedTxs
mkLocallySubmittedTxsVar keepOldDelta = newMVar ([], keepOldDelta)

augmentTxSubmission :: GYSubmitTx -> LocallySubmittedTxs -> GYSubmitTx
augmentTxSubmission txF var tx = modifyMVar var $ \(submitted, keepOldDelta) -> do
  tid <- txF tx
  now <- getCurrentTime
  pure (((tx, now) : removeOldOnes now keepOldDelta submitted, keepOldDelta), tid)
 where
  removeOldOnes :: UTCTime -> NominalDiffTime -> [(GYTx, UTCTime)] -> [(GYTx, UTCTime)]
  removeOldOnes now delta =
    filter
      ( \(_, t) ->
          addUTCTime delta t >= now
      )

splitSubmittedTxsVarOutsModuloIns :: LocallySubmittedTxs -> IO (Map.Map GYTxOutRef (GYUTxO, Maybe GYDatum))
splitSubmittedTxsVarOutsModuloIns locallySubmittedTxsVar = do
  (map fst -> submitted, _) <- readMVar locallySubmittedTxsVar
  pure $ splitTxsOutsModuloIns submitted

augmentQueryUTxOWithLocalSubmission :: GYQueryUTxO -> LocallySubmittedTxs -> GYQueryUTxO
augmentQueryUTxOWithLocalSubmission q locallySubmittedTxsVar = augmentQueryUTxO q locallySubmittedTxsVar splitSubmittedTxsVarOutsModuloIns
