{- |
Module      : GeniusYield.Providers.Node
Description : Providers using local @cardano-node@ connection.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Node (
  nodeSubmitTx,
  nodeSlotActions,
  nodeGetParameters,

  -- * Low-level
  nodeGetSlotOfCurrentBlock,
  nodeStakeAddressInfo,
  nodeStakePools,

  -- * Auxiliary
  networkIdToLocalNodeConnectInfo,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Slotting.Time (SystemStart)
import Control.Exception (throwIO)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Txt
import GeniusYield.CardanoApi.Query
import GeniusYield.Providers.Common (SubmitTxException (SubmitTxException), makeLastEraEndUnbounded)
import GeniusYield.Types
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

nodeSubmitTx :: Api.LocalNodeConnectInfo -> GYSubmitTx
nodeSubmitTx info tx = do
  -- We may submit transaction in older eras as well, it seems.
  res <- Api.submitTxToNodeLocal info $ Api.TxInMode Api.ShelleyBasedEraConway (txToApi tx)
  case res of
    SubmitSuccess -> return $ txIdFromApi $ Api.getTxId $ Api.getTxBody $ txToApi tx
    SubmitFail err -> throwIO $ SubmitTxException $ Txt.pack $ show err

-------------------------------------------------------------------------------
-- Current slot
-------------------------------------------------------------------------------

nodeGetSlotOfCurrentBlock :: Api.LocalNodeConnectInfo -> IO GYSlot
nodeGetSlotOfCurrentBlock info = do
  Api.ChainTip s _ _ <- Api.getLocalChainTip info
  return $ slotFromApi s

nodeSlotActions :: Api.LocalNodeConnectInfo -> GYSlotActions
nodeSlotActions info =
  GYSlotActions
    { gyGetSlotOfCurrentBlock' = getSlotOfCurrentBlock
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault getSlotOfCurrentBlock
    , gyWaitUntilSlot' = gyWaitUntilSlotDefault getSlotOfCurrentBlock
    }
 where
  getSlotOfCurrentBlock = nodeGetSlotOfCurrentBlock info

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

nodeGetParameters :: Api.LocalNodeConnectInfo -> IO GYGetParameters
nodeGetParameters info = makeGetParameters (nodeGetProtocolParameters info) (systemStart info) (eraHistory info) (nodeGetSlotOfCurrentBlock info)

nodeGetProtocolParameters :: Api.LocalNodeConnectInfo -> IO ApiProtocolParameters
nodeGetProtocolParameters info = queryConwayEra info Api.QueryProtocolParameters

nodeStakePools :: Api.LocalNodeConnectInfo -> IO (Set.Set Api.S.PoolId)
nodeStakePools info = queryConwayEra info Api.QueryStakePools

nodeStakeAddressInfo :: Api.LocalNodeConnectInfo -> GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
nodeStakeAddressInfo info saddr = resolveStakeAddressInfoFromApi saddr <$> queryConwayEra info (Api.QueryStakeAddresses (Set.singleton $ stakeCredentialToApi $ stakeAddressToCredential saddr) (Api.localNodeNetworkId info))

resolveStakeAddressInfoFromApi :: GYStakeAddress -> (Map.Map Api.StakeAddress Ledger.Coin, Map.Map Api.StakeAddress Api.S.PoolId) -> Maybe GYStakeAddressInfo
resolveStakeAddressInfoFromApi (stakeAddressToApi -> stakeAddr) (rewards, delegations) =
  if Map.member stakeAddr rewards
    then
      Just $
        GYStakeAddressInfo
          { gyStakeAddressInfoAvailableRewards = fromIntegral $ Map.findWithDefault 0 stakeAddr rewards
          , gyStakeAddressInfoDelegatedPool = stakePoolIdFromApi <$> Map.lookup stakeAddr delegations
          }
    else Nothing

systemStart :: Api.LocalNodeConnectInfo -> IO SystemStart
systemStart info = queryCardanoMode info Api.QuerySystemStart

eraHistory :: Api.LocalNodeConnectInfo -> IO Api.EraHistory
eraHistory info = makeLastEraEndUnbounded <$> queryCardanoMode info Api.QueryEraHistory

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Constructs the connection info to a local node.
networkIdToLocalNodeConnectInfo ::
  -- | The network identifier.
  GYNetworkId ->
  -- | Path to the local node socket.
  FilePath ->
  Api.LocalNodeConnectInfo
networkIdToLocalNodeConnectInfo nid nodeSocket =
  Api.LocalNodeConnectInfo
    { localConsensusModeParams = Api.CardanoModeParams $ networkIdToEpochSlots nid
    , localNodeNetworkId = networkIdToApi nid
    , localNodeSocketPath = Api.File nodeSocket
    }
