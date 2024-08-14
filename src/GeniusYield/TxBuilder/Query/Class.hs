{-|
Module      : GeniusYield.TxBuilder.Query.Class
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Query.Class (GYTxQueryMonad (..), GYTxSpecialQueryMonad (..), GYTxUserQueryMonad (..)) where

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import           Control.Monad.Except                 (MonadError (..))
import           Control.Monad.Random                 (RandT, lift)
import           Control.Monad.Reader                 (ReaderT)
import qualified Control.Monad.State.Lazy             as Lazy
import qualified Control.Monad.State.Strict           as Strict
import qualified Control.Monad.Writer.CPS             as CPS
import qualified Control.Monad.Writer.Lazy            as Lazy
import qualified Control.Monad.Writer.Strict          as Strict
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (listToMaybe)
import           GHC.Stack                            (withFrozenCallStack)

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import           GeniusYield.Types.ProtocolParameters (GYProtocolParameters)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class of monads for querying chain data.
class MonadError GYTxMonadException m => GYTxQueryMonad m where
    {-# MINIMAL networkId, lookupDatum, (utxoAtTxOutRef | utxosAtTxOutRefs), utxosAtAddress, utxosAtPaymentCredential, stakeAddressInfo, slotConfig, slotOfCurrentBlock, logMsg #-}

    -- | Get the network id
    networkId :: m GYNetworkId

    -- | Lookup datum by its hash.
    lookupDatum :: GYDatumHash -> m (Maybe GYDatum)

    -- | Lookup 'GYUTxO' at 'GYTxOutRef'.
    --
    utxoAtTxOutRef :: GYTxOutRef -> m (Maybe GYUTxO)
    utxoAtTxOutRef ref = do
        utxos <- utxosAtTxOutRefs [ref]
        return $ case utxosToList utxos of
            []       -> Nothing
            utxo : _ -> Just utxo

    -- | Lookup UTxO at 'GYTxOutRef' with an attempt to resolve for datum.
    utxoAtTxOutRefWithDatum :: GYTxOutRef -> m (Maybe (GYUTxO, Maybe GYDatum))
    utxoAtTxOutRefWithDatum ref = listToMaybe <$> utxosAtTxOutRefsWithDatums [ref]

    -- | Lookup 'GYUTxOs' at multiple 'GYTxOutRef's at once
    utxosAtTxOutRefs :: [GYTxOutRef] -> m GYUTxOs
    utxosAtTxOutRefs orefs = utxosFromList <$> wither utxoAtTxOutRef orefs

    -- | Lookup UTxOs at zero or more 'GYTxOutRef' with their datums. This has a default implementation using `utxosAtTxOutRefs` and `lookupDatum` but should be overridden for efficiency if provider provides suitable option.
    utxosAtTxOutRefsWithDatums :: [GYTxOutRef] -> m [(GYUTxO, Maybe GYDatum)]
    utxosAtTxOutRefsWithDatums = gyQueryUtxosAtTxOutRefsWithDatumsDefault utxosAtTxOutRefs lookupDatum

    -- | Lookup 'GYUTxOs' at 'GYAddress'.
    utxosAtAddress :: GYAddress -> Maybe GYAssetClass -> m GYUTxOs

    -- | Lookup 'GYUTxO' at given 'GYAddress' with their datums. This has a default implementation using `utxosAtAddress` and `lookupDatum` but should be overridden for efficiency if provider provides suitable option.
    utxosAtAddressWithDatums :: GYAddress -> Maybe GYAssetClass -> m [(GYUTxO, Maybe GYDatum)]
    utxosAtAddressWithDatums = gyQueryUtxosAtAddressWithDatumsDefault utxosAtAddress lookupDatum

    -- | Lookup 'GYUTxOs' at zero or more 'GYAddress'.
    utxosAtAddresses :: [GYAddress] -> m GYUTxOs
    utxosAtAddresses = foldM f mempty
      where
        f :: GYUTxOs -> GYAddress -> m GYUTxOs
        f utxos addr = (<> utxos) <$> utxosAtAddress addr Nothing

    -- | Lookup UTxOs at zero or more 'GYAddress' with their datums. This has a default implementation using `utxosAtAddresses` and `lookupDatum` but should be overridden for efficiency if provider provides suitable option.
    utxosAtAddressesWithDatums :: [GYAddress] -> m [(GYUTxO, Maybe GYDatum)]
    utxosAtAddressesWithDatums = gyQueryUtxosAtAddressesWithDatumsDefault utxosAtAddresses lookupDatum

    -- | Lookup the `[GYTxOutRef]`s at a `GYAddress`
    utxoRefsAtAddress :: GYAddress -> m [GYTxOutRef]
    utxoRefsAtAddress = fmap (Map.keys . mapUTxOs id) . flip utxosAtAddress Nothing

    -- | Lookup 'GYUTxOs' at 'GYPaymentCredential'.
    utxosAtPaymentCredential :: GYPaymentCredential -> Maybe GYAssetClass -> m GYUTxOs

    -- | Lookup UTxOs at given 'GYPaymentCredential' with their datums. This has a default implementation using `utxosAtPaymentCredential` and `lookupDatum` but should be overridden for efficiency if provider provides suitable option.
    utxosAtPaymentCredentialWithDatums :: GYPaymentCredential -> Maybe GYAssetClass -> m [(GYUTxO, Maybe GYDatum)]
    utxosAtPaymentCredentialWithDatums = gyQueryUtxosAtPaymentCredWithDatumsDefault utxosAtPaymentCredential lookupDatum

    -- | Lookup 'GYUTxOs' at zero or more 'GYPaymentCredential'.
    utxosAtPaymentCredentials :: [GYPaymentCredential] -> m GYUTxOs
    utxosAtPaymentCredentials = foldM f mempty
      where
        f :: GYUTxOs -> GYPaymentCredential -> m GYUTxOs
        f utxos paymentCred = (<> utxos) <$> utxosAtPaymentCredential paymentCred Nothing

    -- | Lookup UTxOs at zero or more 'GYPaymentCredential' with their datums. This has a default implementation using `utxosAtPaymentCredentials` and `lookupDatum` but should be overridden for efficiency if provider provides suitable option.
    utxosAtPaymentCredentialsWithDatums :: [GYPaymentCredential] -> m [(GYUTxO, Maybe GYDatum)]
    utxosAtPaymentCredentialsWithDatums = gyQueryUtxosAtPaymentCredsWithDatumsDefault utxosAtPaymentCredentials lookupDatum

    -- | Obtain delegation information for a stake address. Note that in case stake address is not registered, this function should return `Nothing`.
    stakeAddressInfo :: GYStakeAddress -> m (Maybe GYStakeAddressInfo)

    {- | Obtain the slot config for the network.

    Implementations using era history to create slot config may raise 'GYEraSummariesToSlotConfigError'.
    -}
    slotConfig :: m GYSlotConfig

    -- | This is expected to give the slot of the latest block. We say "expected" as we cache the result for 5 seconds, that is to say, suppose slot was cached at time @T@, now if query for current block's slot comes within time duration @(T, T + 5)@, then we'll return the cached slot but if say, query happened at time @(T + 5, T + 21)@ where @21@ was taken as an arbitrary number above 5, then we'll query the chain tip and get the slot of the latest block seen by the provider and then store it in our cache, thus new cached value would be served for requests coming within time interval of @(T + 21, T + 26)@.
    --
    -- __NOTE:__ It's behaviour is slightly different, solely for our plutus simple model provider where it actually returns the value of the @currentSlot@ variable maintained inside plutus simple model library.
    slotOfCurrentBlock :: m GYSlot

    -- | Log a message with specified namespace and severity.
    logMsg :: HasCallStack => GYLogNamespace -> GYLogSeverity -> String -> m ()

-- | Class of monads for querying special chain data.
{- Note [Necessity of 'GYTxSpecialQueryMonad' and transaction building as a class method]

The only purpose of 'GYTxSpecialQueryMonad' is to provide necessary information for building
transactions. Since the inclusion of 'submitTx' under 'GYTxMonad', it is necessary to be able to
build transactions within 'GYTxMonad'. Our current tx building interface requires these pieces
of information. So this is a superclass to 'GYTxMonad'.

However, transaction building _could_ be included as a class method of 'GYTxMonad'. But it is difficult
to decide where to draw the line regarding the interface. Our transaction building mechanisms are aware of
coin selection strategy, parallel transactions, chaining transactions etc. Should all this really be included
under the class method in question?
-}
class GYTxQueryMonad m => GYTxSpecialQueryMonad m where
    systemStart :: m Api.SystemStart
    eraHistory :: m Api.EraHistory
    protocolParams :: m GYProtocolParameters
    stakePools :: m (Set Api.S.PoolId)

-- | Class of monads for querying as a user.
class GYTxQueryMonad m => GYTxUserQueryMonad m where
    -- | Get your own address(es).
    ownAddresses :: m [GYAddress]

    -- | Get own change address.
    ownChangeAddress :: m GYAddress

    -- | Get own collateral utxo.
    ownCollateral :: m (Maybe GYUTxO)

    -- | Get available own UTxOs that can be operated upon.
    availableUTxOs :: m GYUTxOs

    -- | Return some unspent transaction output translatable to the given language corresponding to the script in question.
    --
    -- /Law:/ Must return the different values.
    someUTxO :: PlutusVersion -> m GYTxOutRef

-------------------------------------------------------------------------------
-- Instances for useful transformers.
-------------------------------------------------------------------------------

instance GYTxQueryMonad m => GYTxQueryMonad (RandT g m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance GYTxUserQueryMonad m => GYTxUserQueryMonad (RandT g m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance GYTxSpecialQueryMonad m => GYTxSpecialQueryMonad (RandT g m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

instance GYTxQueryMonad m => GYTxQueryMonad (ReaderT env m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance GYTxUserQueryMonad m => GYTxUserQueryMonad (ReaderT env m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance GYTxSpecialQueryMonad m => GYTxSpecialQueryMonad (ReaderT env m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

-------------------------------------------------------------------------------
-- Instances for less useful transformers, provided for completeness.
-- Many of these transformers are fundamentally riddled with issues.
--    See: https://github.com/haskell-effectful/effectful/blob/master/transformers.md
-------------------------------------------------------------------------------


{- Note [MonadError on GYTxQueryMonad and ExceptT]

ExceptT instances are omitted since the MonadError requirement for GYTxQueryMonad
enforces only one exception type: thereby making multiple error handler transformers
useless.
See: https://ro-che.info/articles/2014-06-11-problem-with-mtl

Perhaps said superclass constraint should be rethought? Does it really
achieve what it is meant to achieve? The purpose of that contraint is to signal
that the query methods _should_ throw errors of type 'GYTxMonadException'.
After all, some queries are bound to fail. But this expectation does not really
restrict the type of exceptions. In fact, an IO based monad could still
be throwing exceptions of different types (and our implementation of 'GYTxMonadIO'
likely does).

Alternatively: Use a better effect system. E.g effectful but any ReaderT based effect
system will suffice (do NOT use free(er) monad like ones). This will trivialize this
entire problem.
-}

instance GYTxQueryMonad m => GYTxQueryMonad (Strict.StateT s m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance GYTxUserQueryMonad m => GYTxUserQueryMonad (Strict.StateT s m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance GYTxSpecialQueryMonad m => GYTxSpecialQueryMonad (Strict.StateT s m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

instance GYTxQueryMonad m => GYTxQueryMonad (Lazy.StateT s m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance GYTxUserQueryMonad m => GYTxUserQueryMonad (Lazy.StateT s m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance GYTxSpecialQueryMonad m => GYTxSpecialQueryMonad (Lazy.StateT s m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

instance (GYTxQueryMonad m, Monoid w) => GYTxQueryMonad (CPS.WriterT w m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance (GYTxUserQueryMonad m, Monoid w) => GYTxUserQueryMonad (CPS.WriterT w m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance (GYTxSpecialQueryMonad m, Monoid w) => GYTxSpecialQueryMonad (CPS.WriterT w m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

instance (GYTxQueryMonad m, Monoid w) => GYTxQueryMonad (Strict.WriterT w m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance (GYTxUserQueryMonad m, Monoid w) => GYTxUserQueryMonad (Strict.WriterT w m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance (GYTxSpecialQueryMonad m, Monoid w) => GYTxSpecialQueryMonad (Strict.WriterT w m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools

instance (GYTxQueryMonad m, Monoid w) => GYTxQueryMonad (Lazy.WriterT w m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = lift . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = lift . utxosAtAddress addr
    utxosAtAddressWithDatums addr = lift . utxosAtAddressWithDatums addr
    utxosAtAddresses = lift . utxosAtAddresses
    utxosAtAddressesWithDatums = lift . utxosAtAddressesWithDatums
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    utxosAtPaymentCredential pc = lift . utxosAtPaymentCredential pc
    utxosAtPaymentCredentialWithDatums pc = lift . utxosAtPaymentCredentialWithDatums pc
    utxosAtPaymentCredentials = lift . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = lift . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = lift . stakeAddressInfo
    slotConfig = lift slotConfig
    slotOfCurrentBlock = lift slotOfCurrentBlock
    logMsg ns s = withFrozenCallStack $ lift . logMsg ns s

instance (GYTxUserQueryMonad m, Monoid w) => GYTxUserQueryMonad (Lazy.WriterT w m) where
    ownAddresses = lift ownAddresses
    ownChangeAddress = lift ownChangeAddress
    ownCollateral = lift ownCollateral
    availableUTxOs = lift availableUTxOs
    someUTxO = lift . someUTxO

instance (GYTxSpecialQueryMonad m, Monoid w) => GYTxSpecialQueryMonad (Lazy.WriterT w m) where
    systemStart = lift systemStart
    eraHistory = lift eraHistory
    protocolParams = lift protocolParams
    stakePools = lift stakePools
