{-|
Module      : GeniusYield.Providers.Node.Query
Description : QueryUTxO provider using local node. Inefficient, for testing purposes only.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

**INTERNAL MODULE**
-}

module GeniusYield.Providers.Node.Query (nodeQueryUTxO) where

import qualified Cardano.Api                                       as Api
import qualified Cardano.Api.Shelley                               as Api.S
import qualified Data.Set                                          as Set
import           GeniusYield.CardanoApi.Query
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- UTxO query
-------------------------------------------------------------------------------

nodeUtxosAtAddress :: GYEra -> Api.LocalNodeConnectInfo -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
nodeUtxosAtAddress era info addr mAssetClass = do
    utxos <- nodeUtxosAtAddresses era info [addr]
    pure $ case mAssetClass of
        Nothing -> utxos
        Just assetClass -> filterUTxOs (\GYUTxO {utxoValue} -> valueAssetClass utxoValue assetClass /= 0) utxos

nodeUtxosAtAddresses :: GYEra -> Api.LocalNodeConnectInfo -> [GYAddress] -> IO GYUTxOs
nodeUtxosAtAddresses era info addrs = do
    queryUTxO era info $ Api.QueryUTxOByAddress $ Set.fromList $ addressToApi <$> addrs

nodeUtxoAtTxOutRef :: GYEra -> Api.LocalNodeConnectInfo -> GYTxOutRef -> IO (Maybe GYUTxO)
nodeUtxoAtTxOutRef era info ref = do
    utxos <- nodeUtxosAtTxOutRefs era info [ref]
    case utxosToList utxos of
        [x] | utxoRef x == ref -> return (Just x)
        _                      -> return Nothing -- we return Nothing also in "should never happen" cases.

nodeUtxosAtTxOutRefs :: GYEra -> Api.LocalNodeConnectInfo -> [GYTxOutRef] -> IO GYUTxOs
nodeUtxosAtTxOutRefs era info refs = queryUTxO era info $ Api.QueryUTxOByTxIn $ Set.fromList $ txOutRefToApi <$> refs

-- NOTE: This is extremely inefficient and only viable for a small private testnet. It queries the whole UTxO set.
nodeUtxosAtPaymentCredential :: GYEra -> Api.LocalNodeConnectInfo -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
nodeUtxosAtPaymentCredential era info cred mAssetClass = do
    utxos <- nodeUtxosAtPaymentCredentials era info [cred]
    pure $ case mAssetClass of
        Nothing -> utxos
        Just assetClass -> filterUTxOs (\GYUTxO {utxoValue} -> valueAssetClass utxoValue assetClass /= 0) utxos

-- NOTE: This is extremely inefficient and only viable for a small private testnet. It queries the whole UTxO set.
nodeUtxosAtPaymentCredentials :: GYEra -> Api.LocalNodeConnectInfo -> [GYPaymentCredential] -> IO GYUTxOs
nodeUtxosAtPaymentCredentials era info creds = do
    allUtxos <- queryUTxO era info Api.QueryUTxOWhole
    pure $ filterUTxOs (\GYUTxO {utxoAddress} -> matchesCred $ addressToPaymentCredential utxoAddress) allUtxos
  where
    credSet = Set.fromList creds
    matchesCred Nothing     = False
    matchesCred (Just cred) = cred `Set.member` credSet

nodeQueryUTxO :: GYEra -> Api.S.LocalNodeConnectInfo -> GYQueryUTxO
nodeQueryUTxO era info = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefsWithDatums'    = Nothing
    , gyQueryUtxosAtTxOutRefs'              = nodeUtxosAtTxOutRefs era info
    , gyQueryUtxosAtPaymentCredsWithDatums' = Nothing
    , gyQueryUtxosAtPaymentCredentials'     = nodeUtxosAtPaymentCredentials era info
    , gyQueryUtxosAtPaymentCredential'      = nodeUtxosAtPaymentCredential era info
    , gyQueryUtxosAtPaymentCredWithDatums'  = Nothing
    , gyQueryUtxosAtAddressesWithDatums'    = Nothing
    , gyQueryUtxosAtAddresses'              = nodeUtxosAtAddresses era info
    , gyQueryUtxosAtAddressWithDatums'      = Nothing
    , gyQueryUtxosAtAddress'                = nodeUtxosAtAddress era info
    , gyQueryUtxoRefsAtAddress'             = gyQueryUtxoRefsAtAddressDefault $ nodeUtxosAtAddress era info
    , gyQueryUtxoAtTxOutRef'                = nodeUtxoAtTxOutRef era info
    }
