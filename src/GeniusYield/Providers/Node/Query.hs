{-|
Module      : GeniusYield.Providers.Node.Query
Description : QueryUTxO provider using local node. Inefficient, for testing purposes only.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

**INTERNAL MODULE**
-}

module GeniusYield.Providers.Node.Query (
    nodeQueryUTxO,
    nodeUtxosAtAddress,
    nodeUtxosAtAddresses,
    nodeUtxoAtTxOutRef,
    nodeUtxosAtTxOutRefs,
    nodeUtxosAtPaymentCredential,
    nodeUtxosAtPaymentCredentials
) where

import qualified Data.Set                     as Set

import qualified Cardano.Api                  as Api
import qualified Cardano.Api.Shelley          as Api.S

import           GeniusYield.CardanoApi.Query
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- UTxO query
-------------------------------------------------------------------------------

nodeUtxosAtAddress :: Api.LocalNodeConnectInfo -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
nodeUtxosAtAddress info addr mAssetClass = do
    utxos <- nodeUtxosAtAddresses info [addr]
    pure $ case mAssetClass of
        Nothing -> utxos
        Just assetClass -> filterUTxOs (\GYUTxO {utxoValue} -> valueAssetClass utxoValue assetClass /= 0) utxos

nodeUtxosAtAddresses :: Api.LocalNodeConnectInfo -> [GYAddress] -> IO GYUTxOs
nodeUtxosAtAddresses info addrs = do
    queryUTxO info $ Api.QueryUTxOByAddress $ Set.fromList $ addressToApi <$> addrs

nodeUtxoAtTxOutRef :: Api.LocalNodeConnectInfo -> GYTxOutRef -> IO (Maybe GYUTxO)
nodeUtxoAtTxOutRef info ref = do
    utxos <- nodeUtxosAtTxOutRefs info [ref]
    case utxosToList utxos of
        [x] | utxoRef x == ref -> return (Just x)
        _                      -> return Nothing -- we return Nothing also in "should never happen" cases.

nodeUtxosAtTxOutRefs :: Api.LocalNodeConnectInfo -> [GYTxOutRef] -> IO GYUTxOs
nodeUtxosAtTxOutRefs info refs = queryUTxO info $ Api.QueryUTxOByTxIn $ Set.fromList $ txOutRefToApi <$> refs

-- NOTE: This is extremely inefficient and only viable for a small private testnet. It queries the whole UTxO set.
nodeUtxosAtPaymentCredential :: Api.LocalNodeConnectInfo -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
nodeUtxosAtPaymentCredential info cred mAssetClass = do
    utxos <- nodeUtxosAtPaymentCredentials info [cred]
    pure $ case mAssetClass of
        Nothing -> utxos
        Just assetClass -> filterUTxOs (\GYUTxO {utxoValue} -> valueAssetClass utxoValue assetClass /= 0) utxos

-- NOTE: This is extremely inefficient and only viable for a small private testnet. It queries the whole UTxO set.
nodeUtxosAtPaymentCredentials :: Api.LocalNodeConnectInfo -> [GYPaymentCredential] -> IO GYUTxOs
nodeUtxosAtPaymentCredentials info creds = do
    allUtxos <- queryUTxO info Api.QueryUTxOWhole
    pure $ filterUTxOs (\GYUTxO {utxoAddress} -> matchesCred $ addressToPaymentCredential utxoAddress) allUtxos
  where
    credSet = Set.fromList creds
    matchesCred Nothing     = False
    matchesCred (Just cred) = cred `Set.member` credSet

nodeQueryUTxO :: Api.S.LocalNodeConnectInfo -> GYQueryUTxO
nodeQueryUTxO info = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefsWithDatums'    = Nothing
    , gyQueryUtxosAtTxOutRefs'              = nodeUtxosAtTxOutRefs info
    , gyQueryUtxosAtPaymentCredsWithDatums' = Nothing
    , gyQueryUtxosAtPaymentCredentials'     = nodeUtxosAtPaymentCredentials info
    , gyQueryUtxosAtPaymentCredential'      = nodeUtxosAtPaymentCredential info
    , gyQueryUtxosAtPaymentCredWithDatums'  = Nothing
    , gyQueryUtxosAtAddressesWithDatums'    = Nothing
    , gyQueryUtxosAtAddresses'              = nodeUtxosAtAddresses info
    , gyQueryUtxosAtAddressWithDatums'      = Nothing
    , gyQueryUtxosAtAddress'                = nodeUtxosAtAddress info
    , gyQueryUtxoRefsAtAddress'             = gyQueryUtxoRefsAtAddressDefault $ nodeUtxosAtAddress info
    , gyQueryUtxoAtTxOutRef'                = nodeUtxoAtTxOutRef info
    }
