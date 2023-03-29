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

    lookupDatum h = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyLookupDatum providers h

    utxosAtAddress addr = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddress providers addr

    utxosAtAddresses addrs = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtAddresses providers addrs

    utxoRefsAtAddress addr = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxoRefsAtAddress providers addr

    utxoAtTxOutRef oref = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxoAtTxOutRef providers oref

    utxosAtTxOutRefs oref = GYTxQueryMonadNode $ \(GYTxNodeEnv _ providers) ->
        gyQueryUtxosAtTxOutRefs providers oref

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
