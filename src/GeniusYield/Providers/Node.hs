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
    , nodeQueryUTxO
    , nodeGetParameters
    -- * Low-level
    , nodeGetCurrentSlot
    , nodeUtxosAtAddress
    , nodeSlotConfig
    -- * Auxiliary
    , networkIdToLocalNodeConnectInfo
    ) where

import qualified Cardano.Api                                       as Api
import qualified Cardano.Api.Shelley                               as Api.S
import           Cardano.Slotting.Time                             (SystemStart)
import           Control.Exception                                 (throwIO)
import qualified Data.Set                                          as Set
import qualified Data.Text                                         as Txt

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))

import           GeniusYield.CardanoApi.Query
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

nodeSubmitTx :: Api.LocalNodeConnectInfo Api.CardanoMode -> GYSubmitTx
nodeSubmitTx info tx = do
    -- We may submit transaction in older eras as well, it seems.
    res <- Api.submitTxToNodeLocal info $ Api.TxInMode (txToApi tx) Api.BabbageEraInCardanoMode
    case res of
        SubmitSuccess  -> return $ txIdFromApi $ Api.getTxId $ Api.getTxBody $ txToApi tx
        SubmitFail err -> throwIO $ userError $ show err

-------------------------------------------------------------------------------
-- Current slot
-------------------------------------------------------------------------------

nodeGetCurrentSlot :: Api.LocalNodeConnectInfo Api.CardanoMode -> IO GYSlot
nodeGetCurrentSlot info = do
    Api.ChainTip s _ _ <- Api.getLocalChainTip info
    return $ slotFromApi s

nodeSlotActions :: Api.LocalNodeConnectInfo Api.CardanoMode -> GYSlotActions
nodeSlotActions info = GYSlotActions
    { gyGetCurrentSlot'   = getCurrentSlot
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault getCurrentSlot
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault getCurrentSlot
    }
  where
    getCurrentSlot = nodeGetCurrentSlot info

-------------------------------------------------------------------------------
-- UTxO query
-------------------------------------------------------------------------------

nodeQueryUTxO :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> GYQueryUTxO
nodeQueryUTxO era info = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefs'           = nodeUtxosAtTxOutRefs era info
    , gyQueryUtxosAtTxOutRefsWithDatums' = Nothing  -- Will use the default implementation.
    , gyQueryUtxoAtTxOutRef'             = nodeUtxoAtTxOutRef era info
    , gyQueryUtxoRefsAtAddress'          = gyQueryUtxoRefsAtAddressDefault $ nodeUtxosAtAddress era info
    , gyQueryUtxosAtAddresses'           = gyQueryUtxoAtAddressesDefault $ nodeUtxosAtAddress era info
    , gyQueryUtxosAtAddressesWithDatums' = Nothing  -- Will use the default implementation.
    , gyQueryUtxosAtPaymentCredential'   = Nothing
    }

nodeUtxosAtAddress :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> GYAddress -> IO GYUTxOs
nodeUtxosAtAddress era info addr = queryUTxO era info $ Api.QueryUTxOByAddress $ Set.singleton $ addressToApi addr

nodeUtxoAtTxOutRef :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> GYTxOutRef -> IO (Maybe GYUTxO)
nodeUtxoAtTxOutRef era info ins = do
    utxos <- queryUTxO era info $ Api.QueryUTxOByTxIn $ Set.singleton $ txOutRefToApi ins
    case utxosToList utxos of
        [x] | utxoRef x == ins -> return (Just x)
        _                      -> return Nothing -- we return Nothing also in "should never happen" cases.

nodeUtxosAtTxOutRefs :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> [GYTxOutRef] -> IO GYUTxOs
nodeUtxosAtTxOutRefs era info ins = queryUTxO era info $ Api.QueryUTxOByTxIn $ Set.fromList $ txOutRefToApi <$> ins

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

nodeGetParameters :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> GYGetParameters
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

nodeGetProtocolParameters :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> IO Api.S.ProtocolParameters
nodeGetProtocolParameters GYAlonzo  info = queryAlonzoEra  info Api.QueryProtocolParameters
nodeGetProtocolParameters GYBabbage info = queryBabbageEra info Api.QueryProtocolParameters

stakePools :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> IO (Set.Set Api.S.PoolId)
stakePools GYAlonzo  info = queryAlonzoEra  info Api.QueryStakePools
stakePools GYBabbage info = queryBabbageEra info Api.QueryStakePools

systemStart :: Api.LocalNodeConnectInfo Api.CardanoMode -> IO SystemStart
systemStart info = queryCardanoMode info Api.QuerySystemStart

eraHistory :: Api.LocalNodeConnectInfo Api.CardanoMode -> IO (Api.EraHistory Api.CardanoMode)
eraHistory info = queryCardanoMode info $ Api.QueryEraHistory Api.CardanoModeIsMultiEra

-------------------------------------------------------------------------------
-- Slot configuration
-------------------------------------------------------------------------------

genesisParameters :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> IO Api.GenesisParameters
genesisParameters GYAlonzo  info = queryAlonzoEra  info Api.QueryGenesisParameters
genesisParameters GYBabbage info = queryBabbageEra info Api.QueryGenesisParameters

-- | Return 'GYSlotConfig', from which we can estimate what the currentSlot should be.
nodeSlotConfig :: GYEra -> Api.LocalNodeConnectInfo Api.CardanoMode -> IO GYSlotConfig
nodeSlotConfig era info = do
    gp <- genesisParameters era info

    let slotLength = Api.protocolParamSlotLength gp
    let slotZero   = Api.protocolParamSystemStart gp

    return $ simpleSlotConfig slotZero slotLength

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Constructs the connection info to a local node.
--
networkIdToLocalNodeConnectInfo :: GYNetworkId                              -- ^ The network identifier.
                                -> FilePath                                 -- ^ Path to the local node socket.
                                -> Api.LocalNodeConnectInfo Api.CardanoMode
networkIdToLocalNodeConnectInfo nid nodeSocket = Api.LocalNodeConnectInfo
    { localConsensusModeParams = Api.CardanoModeParams $ networkIdToEpochSlots nid
    , localNodeNetworkId       = networkIdToApi nid
    , localNodeSocketPath      = nodeSocket
    }
