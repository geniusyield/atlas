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

import           Control.Monad.Reader
import           GHC.Stack                    (withFrozenCallStack)

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types


-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxQueryMonad' interpretation run under IO.
type role GYTxQueryMonadIO representational
newtype GYTxQueryMonadIO a = GYTxQueryMonadIO { runGYTxQueryMonadIO' :: GYTxQueryIOEnv -> IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader GYTxQueryIOEnv
           , MonadIO
           , MonadRandom
           )
  via ReaderT GYTxQueryIOEnv IO

data GYTxQueryIOEnv = GYTxQueryIOEnv { envNid :: !GYNetworkId, envProviders :: !GYProviders}

instance MonadError GYTxMonadException GYTxQueryMonadIO where
    throwError = liftIO . throwIO

    catchError action handler = do
      env <- ask
      liftIO $ catch
        (runGYTxQueryMonadIO' action env)
        (\err -> handler err `runGYTxQueryMonadIO'` env)

instance GYTxQueryMonad GYTxQueryMonadIO where
    networkId = asks envNid

    lookupDatum h = do
      logMsg mempty GYInfo $ printf "Querying Datum: %s" (show h)
      providers <- asks envProviders
      liftIO $ gyLookupDatum providers h

    utxosAtAddress addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxo At Address: %s" addr
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtAddress providers addr mAssetClass

    utxosAtAddressWithDatums addr mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at address: %s" addr
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtAddressWithDatums providers addr mAssetClass

    utxosAtPaymentCredential cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying UTxOs at payment credential: %s" cred
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtPaymentCredential providers cred mAssetClass

    utxosAtAddresses addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos At Addresses: \n %s" (show addrs)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtAddresses providers addrs

    utxosAtAddressesWithDatums addrs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At Addresses: \n %s" (show addrs)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtAddressesWithDatums providers addrs

    utxosAtPaymentCredentialWithDatums cred mAssetClass = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at credential: \n %s" (show cred)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtPaymentCredWithDatums providers cred mAssetClass

    utxosAtPaymentCredentials pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos at payment credentials: \n %s" (show pcs)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtPaymentCredentials providers pcs

    utxosAtPaymentCredentialsWithDatums pcs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) at payment credentials: \n %s" (show pcs)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtPaymentCredsWithDatums providers pcs

    utxoRefsAtAddress addr = do
      logMsg mempty GYInfo $ printf "Querying UtxoRefs At Address: %s"  addr
      providers <- asks envProviders
      liftIO $ gyQueryUtxoRefsAtAddress providers addr

    utxoAtTxOutRef oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRef: %s" oref
      providers <- asks envProviders
      liftIO $ gyQueryUtxoAtTxOutRef providers oref

    utxosAtTxOutRefs oref = do
      logMsg mempty GYInfo $ printf "Querying Utxos At TxOutRefs: %s" (show oref)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtTxOutRefs providers oref

    utxosAtTxOutRefsWithDatums orefs = do
      logMsg mempty GYInfo $ printf "Querying utxos (with datums) At TxOutRefs: \n %s" (show orefs)
      providers <- asks envProviders
      liftIO $ gyQueryUtxosAtTxOutRefsWithDatums providers orefs

    stakeAddressInfo saddr = do
      logMsg mempty GYInfo $ printf "Querying Stake Address Info: %s" saddr
      providers <- asks envProviders
      liftIO $ gyGetStakeAddressInfo providers saddr

    slotConfig = do
      providers <- asks envProviders
      liftIO $ gyGetSlotConfig providers

    slotOfCurrentBlock = do
      providers <- asks envProviders
      liftIO $ gyGetSlotOfCurrentBlock providers

    logMsg ns s msg = do
      providers <- asks envProviders
      liftIO $ withFrozenCallStack $ gyLog providers ns s msg

runGYTxQueryMonadIO
    :: GYNetworkId
    -> GYProviders
    -> GYTxQueryMonadIO a
    -> IO a
runGYTxQueryMonadIO nid providers = flip runGYTxQueryMonadIO' $ GYTxQueryIOEnv nid providers
