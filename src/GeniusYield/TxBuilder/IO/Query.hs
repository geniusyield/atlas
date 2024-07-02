{-|
Module      : GeniusYield.TxBuilder.IO.Query
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.IO.Query (
    GYTxQueryMonadIO,
    runGYTxQueryMonadIO,
) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           GHC.Stack                    (withFrozenCallStack)

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types


-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxQueryMonad' interpretation run under IO.
newtype GYTxQueryMonadIO a = GYTxQueryMonadIO { unGYTxQueryMonadIO :: GYTxQueryIOEnv -> IO a }
  deriving stock (Functor)

instance Applicative GYTxQueryMonadIO where
    pure x = GYTxQueryMonadIO $ \_ -> return x
    (<*>) = ap

instance Monad GYTxQueryMonadIO where
    m >>= k = GYTxQueryMonadIO $ \env -> do
        x <- unGYTxQueryMonadIO m env
        unGYTxQueryMonadIO (k x) env

instance MonadIO GYTxQueryMonadIO where
    liftIO = GYTxQueryMonadIO . const

data GYTxQueryIOEnv = GYTxQueryIOEnv !GYNetworkId !GYProviders

instance MonadError GYTxMonadException GYTxQueryMonadIO where
    throwError = liftIO . throwIO

    catchError action handler = GYTxQueryMonadIO $ \env -> catch
        (unGYTxQueryMonadIO action env)
        (\err -> unGYTxQueryMonadIO (handler err) env)

instance GYTxQueryMonad GYTxQueryMonadIO where
    networkId = GYTxQueryMonadIO $ \(GYTxQueryIOEnv nid _ ) ->
        return nid

    lookupDatum h = do
      logMsg mempty GYInfo $ printf "Querying Datum: %s" (show h)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyLookupDatum providers h

    utxosAtAddress addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxo At Address: %s" addr
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtAddress providers addr mAssetClass

    utxosAtAddressWithDatums addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at address: %s" addr
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtAddressWithDatums providers addr mAssetClass

    utxosAtPaymentCredential cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying UTxOs at payment credential: %s" cred
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtPaymentCredential providers cred mAssetClass

    utxosAtAddresses addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos At Addresses: \n %s" (show addrs)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtAddresses providers addrs

    utxosAtAddressesWithDatums addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At Addresses: \n %s" (show addrs)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtAddressesWithDatums providers addrs

    utxosAtPaymentCredentialWithDatums cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at credential: \n %s" (show cred)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtPaymentCredWithDatums providers cred mAssetClass

    utxosAtPaymentCredentials pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos at payment credentials: \n %s" (show pcs)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtPaymentCredentials providers pcs

    utxosAtPaymentCredentialsWithDatums pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at payment credentials: \n %s" (show pcs)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtPaymentCredsWithDatums providers pcs

    utxoRefsAtAddress addr = do
      logMsg mempty GYInfo $ printf "Querying UtxoRefs At Address: %s"  addr
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxoRefsAtAddress providers addr

    utxoAtTxOutRef oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRef: %s" oref
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxoAtTxOutRef providers oref

    utxosAtTxOutRefs oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRefs: %s" (show oref)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtTxOutRefs providers oref

    utxosAtTxOutRefsWithDatums orefs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At TxOutRefs: \n %s" (show orefs)
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyQueryUtxosAtTxOutRefsWithDatums providers orefs

    stakeAddressInfo saddr = do
      logMsg mempty GYInfo $ printf "Querying Stake Address Info: %s" saddr
      GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyGetStakeAddressInfo providers saddr

    slotConfig = GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyGetSlotConfig providers

    slotOfCurrentBlock = GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        gyGetSlotOfCurrentBlock providers

    logMsg ns s msg = GYTxQueryMonadIO $ \(GYTxQueryIOEnv _ providers) ->
        withFrozenCallStack $ gyLog providers ns s msg

runGYTxQueryMonadIO
    :: GYNetworkId
    -> GYProviders
    -> GYTxQueryMonadIO a
    -> IO a
runGYTxQueryMonadIO nid providers (GYTxQueryMonadIO action) = do
    action $ GYTxQueryIOEnv nid providers
