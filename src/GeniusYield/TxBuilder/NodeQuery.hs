{-|
Module      : GeniusYield.TxBuilder.NodeQuery
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.NodeQuery (
    GYTxQueryMonadNode,
    runGYTxQueryMonadNode,
) where

import           Control.Monad.IO.Class       (MonadIO (..))

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import           GHC.Stack                    (withFrozenCallStack)


-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxQueryMonad' interpretation run against real node.
newtype GYTxQueryMonadNode a = GYTxQueryMonadNode { unGYTxQueryMonadNode :: GYTxNodeEnv -> IO a }
  deriving stock (Functor)

instance Applicative GYTxQueryMonadNode where
    pure x = GYTxQueryMonadNode $ \_ -> return x
    (<*>) = ap

instance Monad GYTxQueryMonadNode where
    m >>= k = GYTxQueryMonadNode $ \env -> do
        x <- unGYTxQueryMonadNode m env
        unGYTxQueryMonadNode (k x) env

instance MonadIO GYTxQueryMonadNode where
    liftIO = GYTxQueryMonadNode . const

data GYTxNodeEnv = GYTxNodeEnv !GYNetworkId !GYProviders

instance MonadError GYTxMonadException GYTxQueryMonadNode where
    throwError = liftIO . throwIO

    catchError action handler = GYTxQueryMonadNode $ \env -> catch
        (unGYTxQueryMonadNode action env)
        (\err -> unGYTxQueryMonadNode (handler err) env)

instance GYTxQueryMonad GYTxQueryMonadNode where
    networkId = GYTxQueryMonadNode $ \(GYTxNodeEnv nid _ ) ->
        return nid

    lookupDatum h = do
      logMsg mempty GYInfo $ printf "Querying Datum: %s" (show h)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyLookupDatum providers h

    utxosAtAddress addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxo At Address: %s" addr
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddress providers addr mAssetClass

    utxosAtAddressWithDatums addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at address: %s" addr
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddressWithDatums providers addr mAssetClass

    utxosAtPaymentCredential cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying UTxOs at payment credential: %s" cred
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtPaymentCredential providers cred mAssetClass

    utxosAtAddresses addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos At Addresses: \n %s" (show addrs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddresses providers addrs

    utxosAtAddressesWithDatums addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At Addresses: \n %s" (show addrs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddressesWithDatums providers addrs

    utxosAtPaymentCredentialWithDatums cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at credential: \n %s" (show cred)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtPaymentCredWithDatums providers cred mAssetClass

    utxosAtPaymentCredentials pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos at payment credentials: \n %s" (show pcs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtPaymentCredentials providers pcs

    utxosAtPaymentCredentialsWithDatums pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at payment credentials: \n %s" (show pcs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtPaymentCredsWithDatums providers pcs

    utxoRefsAtAddress addr = do
      logMsg mempty GYInfo $ printf "Querying UtxoRefs At Address: %s"  addr
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxoRefsAtAddress providers addr

    utxoAtTxOutRef oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRef: %s" oref
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxoAtTxOutRef providers oref

    utxosAtTxOutRefs oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRefs: %s" (show oref)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtTxOutRefs providers oref

    utxosAtTxOutRefsWithDatums orefs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At TxOutRefs: \n %s" (show orefs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtTxOutRefsWithDatums providers orefs

    stakeAddressInfo saddr = do
      logMsg mempty GYInfo $ printf "Querying Stake Address Info: %s" saddr
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyGetStakeAddressInfo providers saddr

    slotConfig = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyGetSlotConfig providers

    slotOfCurrentBlock = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyGetSlotOfCurrentBlock providers

    logMsg ns s msg = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        withFrozenCallStack $ gyLog providers ns s msg

runGYTxQueryMonadNode
    :: GYNetworkId
    -> GYProviders
    -> GYTxQueryMonadNode a
    -> IO a
runGYTxQueryMonadNode nid providers (GYTxQueryMonadNode action) = do
    action $ GYTxNodeEnv nid providers
