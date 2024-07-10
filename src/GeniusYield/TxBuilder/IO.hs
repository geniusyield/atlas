{-|
Module      : GeniusYield.TxBuilder.IO
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.IO (
    GYTxMonadIO,
    GYTxQueryMonadIO,
    GYTxBuilderMonadIO,
    runGYTxBuilderMonadIO,
    runGYTxQueryMonadIO,
    runGYTxMonadIO,
    queryAsBuilderMonad,
    liftQueryMonad,
    liftBuilderMonad
) where


import           Control.Monad.Reader            (ReaderT(ReaderT), MonadReader, asks)
import           Data.Maybe                      (maybeToList)

import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.TxBuilder.IO.Query
import           GeniusYield.TxBuilder.IO.Builder
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation run under IO.
type role GYTxMonadIO representational
newtype GYTxMonadIO a = GYTxMonadIO (GYTxIOEnv -> GYTxBuilderMonadIO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader GYTxIOEnv
           , MonadRandom
           , MonadError GYTxMonadException
           , GYTxQueryMonad
           , GYTxSpecialQueryMonad
           , GYTxUserQueryMonad
           )
  via ReaderT GYTxIOEnv GYTxBuilderMonadIO

data GYTxIOEnv = GYTxIOEnv
    { envNid           :: !GYNetworkId
    , envProviders     :: !GYProviders
    , envPaymentSKey   :: !GYPaymentSigningKey
    , envStakeSKey     :: !(Maybe GYStakeSigningKey)
    }

-- INTERNAL USAGE ONLY
-- Do not expose a 'MonadIO' instance. It allows the user to do arbitrary IO within the tx monad.
ioToTxMonad :: IO a -> GYTxMonadIO a
ioToTxMonad ioAct = GYTxMonadIO . const $ ioToTxBuilderMonad ioAct

-- | Lift a 'GYTxBuilderMonadIO' into 'GYTxMonadIO'.
liftBuilderMonad :: GYTxBuilderMonadIO a -> GYTxMonadIO a
liftBuilderMonad = GYTxMonadIO . pure

-- | Lift a 'GYTxQueryMonadIO' into 'GYTxMonadIO'.
liftQueryMonad :: GYTxQueryMonadIO a -> GYTxMonadIO a
liftQueryMonad = GYTxMonadIO . pure . queryAsBuilderMonad

instance GYTxMonad GYTxMonadIO where
    signTxBody txBody = do
      sKey <- asks envPaymentSKey
      pure $ signGYTxBody txBody [sKey]

    signTxBodyWithStake txBody = do
      paymentSKey <- asks envPaymentSKey
      stakeSKey <- asks envStakeSKey
      pure . signGYTxBody txBody $ GYSomeSigningKey paymentSKey : (GYSomeSigningKey <$> maybeToList stakeSKey)

    submitTx tx = do
        txSubmitter <- asks (gySubmitTx . envProviders)
        ioToTxMonad $ txSubmitter tx

    awaitTxConfirmed' params txId = do
        txAwaiter <- asks (gyAwaitTxConfirmed . envProviders)
        ioToTxMonad $ txAwaiter params txId

runGYTxMonadIO
    :: GYNetworkId                      -- ^ Network ID.
    -> GYProviders                      -- ^ Provider.
    -> GYPaymentSigningKey              -- ^ Payment signing key of the wallet
    -> Maybe GYStakeSigningKey          -- ^ Stake signing key of the wallet (optional)
    -> [GYAddress]                      -- ^ Addresses belonging to wallet.
    -> GYAddress                        -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)         -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO a
    -> IO a
runGYTxMonadIO envNid envProviders envPaymentSKey envStakeSKey envAddrs envChangeAddr collateral (GYTxMonadIO action) = do
    runGYTxBuilderMonadIO envNid envProviders envAddrs envChangeAddr collateral $ action GYTxIOEnv
            { envNid
            , envProviders
            , envPaymentSKey
            , envStakeSKey
            }
