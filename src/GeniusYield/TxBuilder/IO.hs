{- |
Module      : GeniusYield.TxBuilder.IO
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.TxBuilder.IO (
  GYTxGameMonadIO,
  GYTxMonadIO,
  GYTxQueryMonadIO,
  GYTxBuilderMonadIO,
  runGYTxBuilderMonadIO,
  runGYTxQueryMonadIO,
  runGYTxMonadIO,
  runGYTxGameMonadIO,
  queryAsBuilderMonad,
  liftQueryMonad,
  liftBuilderMonad,
) where

import Control.Monad.Reader (
  MonadReader,
  ReaderT (ReaderT),
  asks,
 )
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

import GeniusYield.TxBuilder.Class
import GeniusYield.TxBuilder.Errors
import GeniusYield.TxBuilder.IO.Builder
import GeniusYield.TxBuilder.IO.Query
import GeniusYield.TxBuilder.User
import GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation run under IO.
type role GYTxMonadIO representational

newtype GYTxMonadIO a = GYTxMonadIO (GYTxIOEnv -> GYTxBuilderMonadIO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader GYTxIOEnv
    , MonadRandom
    , MonadError GYTxMonadException
    , GYTxQueryMonad
    , GYTxSpecialQueryMonad
    , GYTxUserQueryMonad
    , GYTxBuilderMonad
    )
    via ReaderT GYTxIOEnv GYTxBuilderMonadIO

data GYTxIOEnv = GYTxIOEnv
  { envNid :: !GYNetworkId
  , envProviders :: !GYProviders
  , envPaymentSKey :: !GYSomePaymentSigningKey
  , envStakeSKey :: !(Maybe GYSomeStakeSigningKey)
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
  signTxBody = signTxBodyImpl $ asks envPaymentSKey

  signTxBodyWithStake = signTxBodyWithStakeImpl $ asks ((,) . envPaymentSKey) <*> asks envStakeSKey

  submitTx tx = do
    txSubmitter <- asks (gySubmitTx . envProviders)
    ioToTxMonad $ txSubmitter tx

  awaitTxConfirmed' params txId = do
    txAwaiter <- asks (gyAwaitTxConfirmed . envProviders)
    ioToTxMonad $ txAwaiter params txId

runGYTxMonadIO ::
  -- | Network ID.
  GYNetworkId ->
  -- | Provider.
  GYProviders ->
  -- | Payment signing key of the wallet
  GYSomePaymentSigningKey ->
  -- | Stake signing key of the wallet (optional)
  Maybe GYSomeStakeSigningKey ->
  -- | Addresses belonging to wallet.
  [GYAddress] ->
  -- | Change address.
  GYAddress ->
  -- | If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
  Maybe (GYTxOutRef, Bool) ->
  GYTxMonadIO a ->
  IO a
runGYTxMonadIO envNid envProviders envPaymentSKey envStakeSKey envAddrs envChangeAddr collateral (GYTxMonadIO action) = do
  runGYTxBuilderMonadIO envNid envProviders envAddrs envChangeAddr collateral $
    action
      GYTxIOEnv
        { envNid
        , envProviders
        , envPaymentSKey
        , envStakeSKey
        }

-- | 'GYTxMonad' interpretation run under IO.
type role GYTxGameMonadIO representational

newtype GYTxGameMonadIO a = GYTxGameMonadIO (GYTxGameIOEnv -> GYTxQueryMonadIO a)
  {- Note: The implementation of 'GYTxGameMonadIO' is pretty hacky. It should really be read as 'GYTxGameIOEnv -> GYTxQueryMonadIO a',
  because that's what is really happening. We use 'GYTxQueryMonadIO' just to auto derive the relevant instances. But in reality,
  we'll be using internal functions to lift IO functions into 'GYTxQueryMonadIO' and _pretend_ to be 'GYTxQueryMonadIO'.

  The usage is controlled, and we do _mean_ to do IO within 'GYTxGameMonadIO'. So it is advised to simply read the impl as suggested above.
  -}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader GYTxGameIOEnv
    , MonadRandom
    , MonadError GYTxMonadException
    , GYTxQueryMonad
    , GYTxSpecialQueryMonad
    )
    via ReaderT GYTxGameIOEnv GYTxQueryMonadIO

data GYTxGameIOEnv = GYTxGameIOEnv
  { envGameNid :: !GYNetworkId
  , envGameProviders :: !GYProviders
  }

{- | INTERNAL USAGE ONLY

Do not expose a 'MonadIO' instance. It allows the user to do arbitrary IO within the tx monad.
-}
ioToTxGameMonad :: IO a -> GYTxGameMonadIO a
ioToTxGameMonad ioAct = GYTxGameMonadIO . const $ ioToQueryMonad ioAct

instance GYTxGameMonad GYTxGameMonadIO where
  type TxMonadOf GYTxGameMonadIO = GYTxMonadIO

  createUser = do
    nid <- networkId
    paymentSKey <- ioToTxGameMonad generatePaymentSigningKey
    stakeSKey <- Just <$> ioToTxGameMonad generateStakeSigningKey
    let paymentVKey = paymentVerificationKey paymentSKey
        stakeVKey = stakeVerificationKey <$> stakeSKey
        pkh = paymentKeyHash paymentVKey
        skh = stakeKeyHash <$> stakeVKey
        newAddr =
          addressFromCredential
            nid
            (GYPaymentCredentialByKey pkh)
            (GYStakeCredentialByKey <$> skh)
    gyLogDebug' "createUser" . T.unpack $ "Created user with address: " <> addressToText newAddr
    pure $ User' {userPaymentSKey' = paymentSKey, userAddr = newAddr, userStakeSKey' = stakeSKey}

  asUser u@User {..} act = do
    nid <- asks envGameNid
    providers <- asks envGameProviders
    ioToTxGameMonad $
      runGYTxMonadIO
        nid
        providers
        (AGYPaymentSigningKey userPaymentSKey)
        (AGYStakeSigningKey <$> userStakeSKey)
        (NE.toList userAddresses)
        userChangeAddress
        (userCollateralDumb u)
        act

runGYTxGameMonadIO ::
  -- | Network ID.
  GYNetworkId ->
  -- | Provider.
  GYProviders ->
  GYTxGameMonadIO a ->
  IO a
runGYTxGameMonadIO envGameNid envGameProviders (GYTxGameMonadIO action) = do
  runGYTxQueryMonadIO envGameNid envGameProviders $
    action
      GYTxGameIOEnv
        { envGameNid
        , envGameProviders
        }
