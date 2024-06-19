{-|
Module      : GeniusYield.Providers.Node
Description : Providers using local @cardano-node@ connection.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.Node
    ( nodeSubmitTx
    , nodeSlotActions
    , nodeGetParameters
    -- * Low-level
    , nodeGetSlotOfCurrentBlock
    , nodeUtxosAtAddress
    , nodeUtxoAtTxOutRef
    , nodeUtxosAtTxOutRefs
    , nodeStakeAddressInfo
    -- * Auxiliary
    , networkIdToLocalNodeConnectInfo
    ) where

import qualified Cardano.Api                                       as Api
import qualified Cardano.Api.Shelley                               as Api.S
import           Cardano.Slotting.Time                             (SystemStart)
import           Control.Exception                                 (throwIO)
import qualified Data.Map.Strict                                   as Map
import qualified Data.Set                                          as Set
import qualified Data.Text                                         as Txt
import           GeniusYield.CardanoApi.Query
import           GeniusYield.Providers.Common                      (SubmitTxException (SubmitTxException))
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

nodeSubmitTx :: Api.LocalNodeConnectInfo -> GYSubmitTx
nodeSubmitTx info tx = do
    -- We may submit transaction in older eras as well, it seems.
    res <- Api.submitTxToNodeLocal info $ Api.TxInMode Api.ShelleyBasedEraBabbage (txToApi tx)
    case res of
        SubmitSuccess  -> return $ txIdFromApi $ Api.getTxId $ Api.getTxBody $ txToApi tx
        SubmitFail err -> throwIO $ SubmitTxException $ Txt.pack $ show err

-------------------------------------------------------------------------------
-- Current slot
-------------------------------------------------------------------------------

nodeGetSlotOfCurrentBlock :: Api.LocalNodeConnectInfo -> IO GYSlot
nodeGetSlotOfCurrentBlock info = do
    Api.ChainTip s _ _ <- Api.getLocalChainTip info
    return $ slotFromApi s

nodeSlotActions :: Api.LocalNodeConnectInfo -> GYSlotActions
nodeSlotActions info = GYSlotActions
    { gyGetSlotOfCurrentBlock' = getSlotOfCurrentBlock
    , gyWaitForNextBlock'      = gyWaitForNextBlockDefault getSlotOfCurrentBlock
    , gyWaitUntilSlot'         = gyWaitUntilSlotDefault getSlotOfCurrentBlock
    }
  where
    getSlotOfCurrentBlock = nodeGetSlotOfCurrentBlock info

-------------------------------------------------------------------------------
-- UTxO query
-------------------------------------------------------------------------------

nodeUtxosAtAddress :: GYEra -> Api.LocalNodeConnectInfo -> GYAddress -> IO GYUTxOs
nodeUtxosAtAddress era info addr = queryUTxO era info $ Api.QueryUTxOByAddress $ Set.singleton $ addressToApi addr

nodeUtxoAtTxOutRef :: GYEra -> Api.LocalNodeConnectInfo -> GYTxOutRef -> IO (Maybe GYUTxO)
nodeUtxoAtTxOutRef era info ins = do
    utxos <- queryUTxO era info $ Api.QueryUTxOByTxIn $ Set.singleton $ txOutRefToApi ins
    case utxosToList utxos of
        [x] | utxoRef x == ins -> return (Just x)
        _                      -> return Nothing -- we return Nothing also in "should never happen" cases.

nodeUtxosAtTxOutRefs :: GYEra -> Api.LocalNodeConnectInfo -> [GYTxOutRef] -> IO GYUTxOs
nodeUtxosAtTxOutRefs era info ins = queryUTxO era info $ Api.QueryUTxOByTxIn $ Set.fromList $ txOutRefToApi <$> ins

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

nodeGetParameters :: GYEra -> Api.LocalNodeConnectInfo -> GYGetParameters
nodeGetParameters era info = GYGetParameters
    { gyGetProtocolParameters' = nodeGetProtocolParameters era info
    , gyGetStakePools'         = stakePools era info
    , gyGetSystemStart'        = systemStart info
    , gyGetEraHistory'         = eraHistory info
    , gyGetSlotConfig'         = either
                                    (throwIO . GYConversionException . GYEraSummariesToSlotConfigError . Txt.pack)
                                    pure
                                    =<< (makeSlotConfig <$> systemStart info <*> eraHistory info)
    }

nodeGetProtocolParameters :: GYEra -> Api.LocalNodeConnectInfo -> IO Api.S.ProtocolParameters
nodeGetProtocolParameters GYAlonzo  info = Api.fromLedgerPParams Api.ShelleyBasedEraAlonzo <$> queryAlonzoEra info Api.QueryProtocolParameters
nodeGetProtocolParameters GYBabbage info = Api.fromLedgerPParams Api.ShelleyBasedEraBabbage <$> queryBabbageEra info Api.QueryProtocolParameters
-- FIXME: add Conway

stakePools :: GYEra -> Api.LocalNodeConnectInfo -> IO (Set.Set Api.S.PoolId)
stakePools GYAlonzo  info = queryAlonzoEra  info Api.QueryStakePools
stakePools GYBabbage info = queryBabbageEra info Api.QueryStakePools

nodeStakeAddressInfo :: Api.LocalNodeConnectInfo -> GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
nodeStakeAddressInfo info saddr = resolveStakeAddressInfoFromApi saddr <$> queryBabbageEra info (Api.QueryStakeAddresses (Set.singleton $ stakeCredentialToApi $ stakeAddressToCredential saddr) (Api.localNodeNetworkId info))

resolveStakeAddressInfoFromApi :: GYStakeAddress -> (Map.Map Api.StakeAddress Api.Lovelace, Map.Map Api.StakeAddress Api.S.PoolId) -> Maybe GYStakeAddressInfo
resolveStakeAddressInfoFromApi (stakeAddressToApi -> stakeAddr) (rewards, delegations) =
    if Map.member stakeAddr rewards
    then Just $ GYStakeAddressInfo
        { gyStakeAddressInfoAvailableRewards = fromIntegral $ Map.findWithDefault 0 stakeAddr rewards
        , gyStakeAddressInfoDelegatedPool = stakePoolIdFromApi <$> Map.lookup stakeAddr delegations
        }
    else Nothing

systemStart :: Api.LocalNodeConnectInfo -> IO SystemStart
systemStart info = queryCardanoMode info Api.QuerySystemStart

eraHistory :: Api.LocalNodeConnectInfo -> IO Api.EraHistory
eraHistory info = queryCardanoMode info Api.QueryEraHistory

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Constructs the connection info to a local node.
--
networkIdToLocalNodeConnectInfo :: GYNetworkId                              -- ^ The network identifier.
                                -> FilePath                                 -- ^ Path to the local node socket.
                                -> Api.LocalNodeConnectInfo
networkIdToLocalNodeConnectInfo nid nodeSocket = Api.LocalNodeConnectInfo
    { localConsensusModeParams = Api.CardanoModeParams $ networkIdToEpochSlots nid
    , localNodeNetworkId       = networkIdToApi nid
    , localNodeSocketPath      = Api.File nodeSocket
    }
