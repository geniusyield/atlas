{-|
Module      : GeniusYield.TxBuilder.IO.Builder
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

**INTERNAL MODULE**
-}
module GeniusYield.TxBuilder.IO.Builder (
    GYTxBuilderMonadIO,
    runGYTxBuilderMonadIO,
    ioToTxBuilderMonad,
    queryAsBuilderMonad,
) where


import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader, ReaderT (ReaderT),
                                                 asks)
import           Control.Monad.Trans.Maybe      (MaybeT (runMaybeT))
import qualified Data.Set                       as Set

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.TxBuilder.IO.Query
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxUserQueryMonad' interpretation run under IO.
type role GYTxBuilderMonadIO representational
newtype GYTxBuilderMonadIO a = GYTxBuilderMonadIO (GYTxBuilderIOEnv -> GYTxQueryMonadIO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader GYTxBuilderIOEnv
           , MonadRandom
           , MonadError GYTxMonadException
           , GYTxQueryMonad
           , GYTxSpecialQueryMonad
           )
  via ReaderT GYTxBuilderIOEnv GYTxQueryMonadIO
  deriving anyclass GYTxBuilderMonad

data GYTxBuilderIOEnv = GYTxBuilderIOEnv
    { envAddrs         :: ![GYAddress]
    , envChangeAddr    :: !GYAddress
    , envCollateral    :: !(Maybe GYUTxO)
    , envUsedSomeUTxOs :: !(Set GYTxOutRef)
    }

-- INTERNAL USAGE ONLY
-- Do not expose a 'MonadIO' instance. It allows the user to do arbitrary IO within the tx monad.
ioToTxBuilderMonad :: IO a -> GYTxBuilderMonadIO a
ioToTxBuilderMonad ioAct = GYTxBuilderMonadIO . const $ ioToQueryMonad ioAct

-- | Lift a 'GYTxQueryMonadIO' into a 'GYTxBuilderMonadIO'
queryAsBuilderMonad :: GYTxQueryMonadIO a -> GYTxBuilderMonadIO a
queryAsBuilderMonad = GYTxBuilderMonadIO . pure

instance GYTxUserQueryMonad GYTxBuilderMonadIO where

    ownAddresses = asks envAddrs

    ownChangeAddress = asks envChangeAddr

    ownCollateral = asks envCollateral

    availableUTxOs = do
        addrs         <- ownAddresses
        mCollateral   <- getCollateral
        usedSomeUTxOs <- getUsedSomeUTxOs
        utxos         <- utxosAtAddresses addrs
        return $ utxosRemoveTxOutRefs (maybe usedSomeUTxOs ((`Set.insert` usedSomeUTxOs) . utxoRef) mCollateral) utxos
      where
        getCollateral    = asks envCollateral
        getUsedSomeUTxOs = asks envUsedSomeUTxOs

    someUTxO lang = do
        addrs           <- ownAddresses
        utxosToConsider <- availableUTxOs
        case lang of
          PlutusV3 -> ifNotV1 utxosToConsider addrs
          PlutusV2 -> ifNotV1 utxosToConsider addrs
          PlutusV1 ->
            case find utxoTranslatableToV1 $ utxosToList utxosToConsider of
              Just u  -> return $ utxoRef u
              Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Better error message here?
      where
          ifNotV1 utxosToConsider addrs =
            case someTxOutRef utxosToConsider  of
                Just (oref, _) -> return oref
                Nothing        -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs

runGYTxBuilderMonadIO
    :: GYNetworkId                      -- ^ Network ID.
    -> GYProviders                      -- ^ Provider.
    -> [GYAddress]                      -- ^ Addresses belonging to wallet.
    -> GYAddress                        -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)         -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxBuilderMonadIO a
    -> IO a
runGYTxBuilderMonadIO envNid envProviders envAddrs envChangeAddr collateral (GYTxBuilderMonadIO action) = do
    collateral' <- obtainCollateral

    runGYTxQueryMonadIO envNid envProviders $ action GYTxBuilderIOEnv
        { envAddrs
        , envChangeAddr
        , envCollateral    = collateral'
        , envUsedSomeUTxOs = mempty
        }
    where
      obtainCollateral :: IO (Maybe GYUTxO)
      obtainCollateral = runMaybeT $ do
        (collateralRef, toCheck) <- hoistMaybe collateral
        collateralUtxo <- liftIO $ gyQueryUtxoAtTxOutRef envProviders collateralRef
            >>= maybe (throwIO . GYQueryUTxOException $ GYNoUtxoAtRef collateralRef) pure
        if not toCheck || (utxoValue collateralUtxo == collateralValue) then return collateralUtxo
        else hoistMaybe Nothing

