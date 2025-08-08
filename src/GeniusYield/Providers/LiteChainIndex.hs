-- IOG didn't use ExplicitNamespaces to deprecate only pattern synonyms
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : GeniusYield.Providers.LiteChainIndex
Description : Lite-chain index. In memory chain index. Used in tests
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.LiteChainIndex (
  LCIClient,
  withLCIClient,
  newLCIClient,
  closeLCIClient,
  lciWaitUntilSlot,
  lciLookupDatum,
  lciGetCurrentSlot,
  lciStats,
) where

import GeniusYield.Imports
import GeniusYield.Types

import Cardano.Api qualified as Api
import Cardano.Api.ChainSync.Client qualified as Api.Sync
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Data.Map.Strict qualified as Map

-- | A very simple chain index client, which only maintains a datum hashes.
data LCIClient
  = LCIClient
      (Async.Async ())
      (STM.TVar Api.SlotNo)
      (STM.TVar (Map (Api.Hash Api.ScriptData) Api.HashableScriptData))

withLCIClient ::
  Api.LocalNodeConnectInfo ->
  -- | resume points
  [Api.ChainPoint] ->
  (LCIClient -> IO r) ->
  IO r
withLCIClient info resumePoints kont = do
  slotVar <- STM.newTVarIO $ Api.SlotNo 0
  dataVar <- STM.newTVarIO Map.empty

  let cb = chainSyncCallback slotVar dataVar

  withChainSync info resumePoints cb $ \a ->
    kont $
      LCIClient
        a
        slotVar
        dataVar

{- | Create new 'LCIClient'.

Use 'withLCIClient' if possible.
-}
newLCIClient ::
  Api.LocalNodeConnectInfo ->
  -- | resume points
  [Api.ChainPoint] ->
  IO LCIClient
newLCIClient info resumePoints = do
  slotVar <- STM.newTVarIO $ Api.SlotNo 0
  dataVar <- STM.newTVarIO Map.empty

  let cb = chainSyncCallback slotVar dataVar

  a <- newChainSync info resumePoints cb
  return $ LCIClient a slotVar dataVar

chainSyncCallback :: STM.TVar Api.SlotNo -> STM.TVar (Map (Api.Hash Api.ScriptData) Api.HashableScriptData) -> ChainSyncCallback
chainSyncCallback slotVar dataVar (RollForward block@(Api.BlockInMode Api.ConwayEra (Api.getBlockHeader -> Api.BlockHeader slot _ _)) _tip) =
  STM.atomically $ do
    STM.writeTVar slotVar slot
    STM.modifyTVar' dataVar $ \m ->
      foldl' (\m' sd -> Map.insert (Api.hashScriptDataBytes sd) sd m') m (blockDatums block)
chainSyncCallback _ _ _ = return ()

-- | Close (destroy) 'LCIClient'.
closeLCIClient :: LCIClient -> IO ()
closeLCIClient (LCIClient a _ _) = Async.cancel a

-- | Wait until 'LCIClient' has processed a given slot.
lciWaitUntilSlot :: LCIClient -> GYSlot -> IO GYSlot
lciWaitUntilSlot (LCIClient _ slotVar _) (slotToApi -> slot) = STM.atomically $ do
  slot' <- STM.readTVar slotVar
  unless (slot' >= slot) STM.retry
  return (slotFromApi slot')

lookupApiDatum :: LCIClient -> Api.Hash Api.ScriptData -> IO (Maybe Api.HashableScriptData)
lookupApiDatum (LCIClient _ _ dataVar) h = do
  m <- STM.readTVarIO dataVar
  return $ Map.lookup h m

lciLookupDatum :: LCIClient -> GYLookupDatum
lciLookupDatum c dh = fmap datumFromApi' <$> lookupApiDatum c (datumHashToApi dh)

{- | This is not good 'GeniusYield.Types.Providers.gyGetCurrentSlot' provider as it might lag
plenty behind the current slot of local node.
-}
lciGetCurrentSlot :: LCIClient -> IO GYSlot
lciGetCurrentSlot (LCIClient _ slotVar _) = slotFromApi <$> STM.readTVarIO slotVar

-- | Return statistics of 'LCIClient': currently processed slot and number of hashes known.
lciStats :: LCIClient -> IO (GYSlot, Int)
lciStats (LCIClient _ slotVar dataVar) = STM.atomically $ do
  slot <- STM.readTVar slotVar
  m <- STM.readTVar dataVar
  return (slotFromApi slot, Map.size m)

-------------------------------------------------------------------------------
-- simplified Cardano.Protocol.Socket.Client
-------------------------------------------------------------------------------

data ChainSyncEvent
  = Resume !Api.ChainPoint
  | RollForward !Api.BlockInMode !Api.ChainTip
  | RollBackward !Api.ChainPoint !Api.ChainTip

type ChainSyncCallback = ChainSyncEvent -> IO ()

withChainSync ::
  Api.LocalNodeConnectInfo ->
  [Api.ChainPoint] ->
  ChainSyncCallback ->
  (Async.Async () -> IO r) ->
  IO r
withChainSync info resumePoints callback = Async.withAsync (Api.connectToLocalNode info localNodeClientProtocols)
 where
  localNodeClientProtocols :: Api.LocalNodeClientProtocolsInMode
  localNodeClientProtocols =
    Api.LocalNodeClientProtocols
      { localChainSyncClient = Api.LocalChainSyncClient $ chainSyncClient resumePoints callback
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

newChainSync ::
  Api.LocalNodeConnectInfo ->
  [Api.ChainPoint] ->
  ChainSyncCallback ->
  IO (Async.Async ())
newChainSync info resumePoints callback =
  Async.async (Api.connectToLocalNode info localNodeClientProtocols)
 where
  localNodeClientProtocols :: Api.LocalNodeClientProtocolsInMode
  localNodeClientProtocols =
    Api.LocalNodeClientProtocols
      { localChainSyncClient = Api.LocalChainSyncClient $ chainSyncClient resumePoints callback
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

chainSyncClient ::
  [Api.ChainPoint] ->
  ChainSyncCallback ->
  Api.ChainSyncClient Api.BlockInMode Api.ChainPoint Api.ChainTip IO ()
chainSyncClient [] cb = chainSyncClient [Api.ChainPointAtGenesis] cb
chainSyncClient resumePoints cb =
  Api.ChainSyncClient $ pure initialise
 where
  initialise =
    Api.Sync.SendMsgFindIntersect resumePoints $
      Api.Sync.ClientStIntersect
        { Api.Sync.recvMsgIntersectFound = \point _tip -> Api.ChainSyncClient $ do
            cb (Resume point)
            pure requestNext
        , Api.Sync.recvMsgIntersectNotFound = \_tip ->
            Api.ChainSyncClient $ pure requestNext
        }

  requestNext :: Api.Sync.ClientStIdle Api.BlockInMode Api.ChainPoint Api.ChainTip IO ()
  requestNext = Api.Sync.SendMsgRequestNext (pure ()) handleNext

  handleNext =
    Api.Sync.ClientStNext
      { Api.Sync.recvMsgRollForward = \block tip -> Api.ChainSyncClient $ do
          cb (RollForward block tip)
          pure requestNext
      , Api.Sync.recvMsgRollBackward = \point tip -> Api.ChainSyncClient $ do
          cb (RollBackward point tip)
          pure requestNext
      }

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

blockDatums :: Api.BlockInMode -> [Api.HashableScriptData]
blockDatums (Api.BlockInMode _ block) = goBlock block
 where
  goBlock :: Api.Block era -> [Api.HashableScriptData]
  goBlock (Api.Block _header txs) = concatMap goTx txs

  goTx :: Api.Tx era -> [Api.HashableScriptData]
  goTx (Api.Tx (Api.TxBody body) _witnesses) = goTxBody body

  goTxBody :: Api.TxBodyContent Api.ViewTx era -> [Api.HashableScriptData]
  goTxBody body = concatMap goTxOut (Api.txOuts body)

  goTxOut :: Api.TxOut Api.CtxTx era -> [Api.HashableScriptData]
  goTxOut (Api.TxOut _addr _value datum _) = goDatum datum

  goDatum :: Api.TxOutDatum Api.CtxTx era -> [Api.HashableScriptData]
  goDatum Api.TxOutDatumNone = []
  goDatum (Api.TxOutSupplementalDatum _ sd) = [sd]
  goDatum (Api.TxOutDatumHash _ _h) = []
  goDatum (Api.TxOutDatumInline _ sd) = [sd]
