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

    utxosAtAddress addr = do
      logMsg mempty GYInfo $ printf "Querying utxo At Address: %s" addr
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddress providers addr

    utxosAtAddresses addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos At Addresses: \n %s" (show addrs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddresses providers addrs

    utxosAtAddressesWithDatums addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At Addresses: \n %s" (show addrs)
      GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddressesWithDatums providers addrs

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

    slotConfig = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyGetSlotConfig providers

    currentSlot = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyGetCurrentSlot providers

    logMsg ns s msg = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyLog providers ns s msg

runGYTxQueryMonadNode
    :: GYNetworkId
    -> GYProviders
    -> GYTxQueryMonadNode a
    -> IO a
runGYTxQueryMonadNode nid providers (GYTxQueryMonadNode action) = do
    action $ GYTxNodeEnv nid providers
