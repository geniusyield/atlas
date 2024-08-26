{-|
Module      : GeniusYield.TxBuilder.IO.Query
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

**INTERNAL MODULE**
-}
module GeniusYield.TxBuilder.IO.Query (
    GYTxQueryMonadIO,
    runGYTxQueryMonadIO,
    ioToQueryMonad
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
           , MonadRandom
           , MonadIO
           )
  via ReaderT GYTxQueryIOEnv IO

data GYTxQueryIOEnv = GYTxQueryIOEnv { envNid :: !GYNetworkId, envProviders :: !GYProviders}

-- | INTERNAL USAGE ONLY
--
-- Do not expose a 'MonadIO' instance. It allows the user to do arbitrary IO within the tx monad.
ioToQueryMonad :: IO a -> GYTxQueryMonadIO a
ioToQueryMonad ioAct = GYTxQueryMonadIO $ const ioAct

instance MonadError GYTxMonadException GYTxQueryMonadIO where
    throwError = ioToQueryMonad . throwIO

    catchError action handler = do
        env <- ask
        ioToQueryMonad $ catch
            (runGYTxQueryMonadIO' action env)
            (\err -> handler err `runGYTxQueryMonadIO'` env)

instance GYTxQueryMonad GYTxQueryMonadIO where
    networkId = asks envNid

    lookupDatum h = do
        logMsg mempty GYDebug $ printf "Querying Datum: %s" (show h)
        providers <- asks envProviders
        ioToQueryMonad $ gyLookupDatum providers h

    utxosAtAddress addr mAssetClass = do
        logMsg mempty GYDebug $ printf "Querying utxo At Address: %s" addr
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtAddress providers addr mAssetClass

    utxosAtAddressWithDatums addr mAssetClass = do
        logMsg mempty GYDebug $ printf "Querying utxos (with datums) at address: %s" addr
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtAddressWithDatums providers addr mAssetClass

    utxosAtPaymentCredential cred mAssetClass = do
        logMsg mempty GYDebug $ printf "Querying UTxOs at payment credential: %s" cred
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtPaymentCredential providers cred mAssetClass

    utxosAtAddresses addrs = do
        logMsg mempty GYDebug $ printf "Querying utxos At Addresses: \n %s" (show addrs)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtAddresses providers addrs

    utxosAtAddressesWithDatums addrs = do
        logMsg mempty GYDebug $ printf "Querying utxos (with datums) At Addresses: \n %s" (show addrs)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtAddressesWithDatums providers addrs

    utxosAtPaymentCredentialWithDatums cred mAssetClass = do
        logMsg mempty GYDebug $ printf "Querying utxos (with datums) at credential: \n %s" (show cred)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtPaymentCredWithDatums providers cred mAssetClass

    utxosAtPaymentCredentials pcs = do
        logMsg mempty GYDebug $ printf "Querying utxos at payment credentials: \n %s" (show pcs)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtPaymentCredentials providers pcs

    utxosAtPaymentCredentialsWithDatums pcs = do
        logMsg mempty GYDebug $ printf "Querying utxos (with datums) at payment credentials: \n %s" (show pcs)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtPaymentCredsWithDatums providers pcs

    utxoRefsAtAddress addr = do
        logMsg mempty GYDebug $ printf "Querying UtxoRefs At Address: %s"  addr
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxoRefsAtAddress providers addr

    utxoAtTxOutRef oref = do
        logMsg mempty GYDebug $ printf "Querying Utxos At TxOutRef: %s" oref
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxoAtTxOutRef providers oref

    utxosAtTxOutRefs oref = do
        logMsg mempty GYDebug $ printf "Querying Utxos At TxOutRefs: %s" (show oref)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtTxOutRefs providers oref

    utxosAtTxOutRefsWithDatums orefs = do
        logMsg mempty GYDebug $ printf "Querying utxos (with datums) At TxOutRefs: \n %s" (show orefs)
        providers <- asks envProviders
        ioToQueryMonad $ gyQueryUtxosAtTxOutRefsWithDatums providers orefs

    stakeAddressInfo saddr = do
        logMsg mempty GYDebug $ printf "Querying Stake Address Info: %s" saddr
        providers <- asks envProviders
        ioToQueryMonad $ gyGetStakeAddressInfo providers saddr

    slotConfig = do
        providers <- asks envProviders
        ioToQueryMonad $ gyGetSlotConfig providers

    slotOfCurrentBlock = do
        providers <- asks envProviders
        ioToQueryMonad $ gyGetSlotOfCurrentBlock providers

    logMsg ns s msg = do
        providers <- asks envProviders
        ioToQueryMonad $ withFrozenCallStack $ gyLog providers ns s msg

    waitUntilSlot slot = do
        providers <- asks envProviders
        ioToQueryMonad $ gyWaitUntilSlot providers slot

    waitForNextBlock = do
        providers <- asks envProviders
        ioToQueryMonad $ gyWaitForNextBlock providers

instance GYTxSpecialQueryMonad GYTxQueryMonadIO where
    systemStart    = asks envProviders >>= ioToQueryMonad . gyGetSystemStart
    eraHistory     = asks envProviders >>= ioToQueryMonad . gyGetEraHistory
    protocolParams = asks envProviders >>= ioToQueryMonad . gyGetProtocolParameters
    stakePools     = asks envProviders >>= ioToQueryMonad . gyGetStakePools

runGYTxQueryMonadIO
    :: GYNetworkId
    -> GYProviders
    -> GYTxQueryMonadIO a
    -> IO a
runGYTxQueryMonadIO nid providers = flip runGYTxQueryMonadIO' $ GYTxQueryIOEnv nid providers
