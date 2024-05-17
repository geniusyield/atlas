{-|
Module      : GeniusYield.Types.Providers
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Providers
    ( -- * Lookup Datum
      GYLookupDatum
      -- * Submit Tx
    , GYSubmitTx
      -- * Await Tx Confirmed
    , GYAwaitTx
    , GYAwaitTxParameters (..)
    , GYAwaitTxException (..)
      -- * Get current slot
    , GYSlotActions (..)
    , gyGetSlotOfCurrentBlock
    , gyWaitForNextBlock
    , gyWaitForNextBlock_
    , gyWaitForNextBlockDefault
    , gyWaitUntilSlot
    , gyWaitUntilSlotDefault
    , makeSlotActions
      -- * Get network parameters
    , GYGetParameters (..)
    , gyGetProtocolParameters
    , gyGetSystemStart
    , gyGetEraHistory
    , gyGetStakePools
    , gyGetSlotConfig
    , makeGetParameters
      -- * Query UTxO
    , gyQueryUtxosAtAddressWithDatumsDefault
    , gyQueryUtxosAtAddressesWithDatumsDefault
    , gyQueryUtxosAtPaymentCredsWithDatumsDefault
    , gyQueryUtxosAtPaymentCredWithDatumsDefault
    , gyQueryUtxosAtTxOutRefsWithDatumsDefault
    , GYQueryUTxO (..)
    , gyQueryUtxosAtAddresses
    , gyQueryUtxosAtAddressWithDatums
    , gyQueryUtxosAtAddressesWithDatums
    , gyQueryUtxosAtPaymentCredWithDatums
    , gyQueryUtxosAtPaymentCredsWithDatums
    , gyQueryUtxosAtAddress
    , gyQueryUtxosAtPaymentCredential
    , gyQueryUtxosAtPaymentCredentials
    , gyQueryUtxosAtTxOutRefs
    , gyQueryUtxosAtTxOutRefsWithDatums
    , gyQueryUtxoAtTxOutRef
    , gyQueryUtxoRefsAtAddress
    , gyQueryUtxoRefsAtAddressDefault
    , gyQueryUtxoAtAddressesDefault
    , gyQueryUtxoAtPaymentCredentialsDefault
    , gyQueryUtxosAtTxOutRefsDefault
      -- * Logging
    , GYLog (..)
    , gyLog
    , gyLogDebug
    , gyLogInfo
    , gyLogWarning
    , gyLogError
    , noLogging
    , simpleConsoleLogging
      -- * Providers
    , GYProviders (..)
    ) where

import qualified Cardano.Api                        as Api
import qualified Cardano.Api.Shelley                as Api.S
import           Cardano.Slotting.Time              (SystemStart)
import           Control.Concurrent                 (MVar, modifyMVar, newMVar,
                                                     threadDelay)
import           Control.Monad                      ((<$!>))
import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.Default                       (Default, def)
import qualified Data.Text                          as Txt
import           Data.Time
import           Data.Word                          (Word64)
import           GeniusYield.CardanoApi.EraHistory  (getEraEndSlot)
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types.Address
import           GeniusYield.Types.Credential       (GYPaymentCredential)
import           GeniusYield.Types.Datum
import           GeniusYield.Types.Logging
import           GeniusYield.Types.Slot
import           GeniusYield.Types.SlotConfig
import           GeniusYield.Types.StakeAddressInfo (GYStakeAddressInfo)
import           GeniusYield.Types.Tx
import           GeniusYield.Types.TxOutRef
import           GeniusYield.Types.UTxO
import           GeniusYield.Types.Value            (GYAssetClass)

{- Note [Caching and concurrently accessible MVars]

Certain IO actions within GYProviders are wrapped with a "cache" to ensure they aren't called
too many times. In particular, there are indicators to suggest whether or not a particular piece
of information should be refetched, as some information only changes per block change/epoch change etc.

As such, we store the fetched data into an MVar, alongside indication of its validity. When the MVar is
read and it is found that the data within has passed expiration, it is refetched and the MVar updated.

It's important to ensure sanity of this flow in a concurrent environment. We rely on 'modifyMVar' to
obtain this psuedo-atomicity (as well as proper exception safety). In particular, one must ensure such
MVars are only accessed through 'modifyMVar', and never unconditionally written to outside of it.
This ensures that when one thread is running its 'modifyMVar', all other threads must wait for it to finish
(or encounter an exception). Other threads should not be allowed to unconditionally write to the MVar.

== Why not use STM?

The required atomic operation here is:
- Read the var
- Compare its value to some state obtained via IO
- Conditionally fetch new data, using an IO operation
- Update the var

There is no (safe)way to perform IO within an 'atomically' block. So STM doesn't buy us much here.
-}

-------------------------------------------------------------------------------
-- All providers
-------------------------------------------------------------------------------

data GYProviders = GYProviders
    { gyLookupDatum         :: !GYLookupDatum
    , gySubmitTx            :: !GYSubmitTx
    , gyAwaitTxConfirmed    :: !GYAwaitTx
    -- ^ This is a function to see whether the submitted transaction is successfully seen on chain. __NOTE:__ Don't call `gyAwaitTxConfirmed` on transaction that has been submitted long ago as we determine presence of submitted transaction by looking for any UTxO generated by it. Though we maintain information for even spent UTxOs until the block which spent them is sufficiently deep (2160 blocks for mainnet).
    , gySlotActions         :: !GYSlotActions
    , gyGetParameters       :: !GYGetParameters
    , gyQueryUTxO           :: !GYQueryUTxO
    , gyGetStakeAddressInfo :: !(GYStakeAddress -> IO (Maybe GYStakeAddressInfo))
    , gyLog'                :: !GYLog
    }

gyGetSlotOfCurrentBlock :: GYProviders -> IO GYSlot
gyGetSlotOfCurrentBlock = gyGetSlotOfCurrentBlock' . gySlotActions

gyWaitForNextBlock :: GYProviders -> IO GYSlot
gyWaitForNextBlock = gyWaitForNextBlock' . gySlotActions

gyWaitUntilSlot :: GYProviders -> GYSlot -> IO GYSlot
gyWaitUntilSlot providers = gyWaitUntilSlot' (gySlotActions providers)

-- | 'gyWaitForNextBlock' variant which doesn't return current slot.
--
gyWaitForNextBlock_ :: GYProviders -> IO ()
gyWaitForNextBlock_ = void . gyWaitForNextBlock

gyQueryUtxosAtAddress :: GYProviders -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
gyQueryUtxosAtAddress = gyQueryUtxosAtAddress' . gyQueryUTxO

gyQueryUtxosAtAddresses :: GYProviders -> [GYAddress] -> IO  GYUTxOs
gyQueryUtxosAtAddresses = gyQueryUtxosAtAddresses' . gyQueryUTxO

gyQueryUtxosAtPaymentCredential :: GYProviders -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
gyQueryUtxosAtPaymentCredential = gyQueryUtxosAtPaymentCredential' . gyQueryUTxO

gyQueryUtxosAtPaymentCredentials :: GYProviders -> [GYPaymentCredential] -> IO  GYUTxOs
gyQueryUtxosAtPaymentCredentials = gyQueryUtxosAtPaymentCredentials' . gyQueryUTxO

gyQueryUtxosAtAddressWithDatums :: GYProviders -> GYAddress -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtAddressWithDatums provider addr mAssetClass =
  case gyQueryUtxosAtAddressWithDatums' $ gyQueryUTxO provider of
    Nothing -> gyQueryUtxosAtAddressWithDatumsDefault (gyQueryUtxosAtAddress provider) (gyLookupDatum provider) addr mAssetClass
    Just f  -> f addr mAssetClass

gyQueryUtxosAtAddressesWithDatums :: GYProviders -> [GYAddress] -> IO [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtAddressesWithDatums provider addrs =
  case gyQueryUtxosAtAddressesWithDatums' $ gyQueryUTxO provider of
    Nothing -> gyQueryUtxosAtAddressesWithDatumsDefault (gyQueryUtxosAtAddresses provider) (gyLookupDatum provider) addrs
    Just f  -> f addrs

gyQueryUtxosAtPaymentCredWithDatums :: GYProviders -> GYPaymentCredential -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtPaymentCredWithDatums provider cred mAssetClass =
  case gyQueryUtxosAtPaymentCredWithDatums' $ gyQueryUTxO provider of
    Nothing -> gyQueryUtxosAtPaymentCredWithDatumsDefault (gyQueryUtxosAtPaymentCredential provider) (gyLookupDatum provider) cred mAssetClass
    Just f  -> f cred mAssetClass

gyQueryUtxosAtPaymentCredsWithDatums :: GYProviders -> [GYPaymentCredential] -> IO [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtPaymentCredsWithDatums provider pcs =
  case gyQueryUtxosAtPaymentCredsWithDatums' $ gyQueryUTxO provider of
    Nothing -> gyQueryUtxosAtPaymentCredsWithDatumsDefault (gyQueryUtxosAtPaymentCredentials provider) (gyLookupDatum provider) pcs
    Just f  -> f pcs

gyQueryUtxosAtTxOutRefs :: GYProviders -> [GYTxOutRef] -> IO GYUTxOs
gyQueryUtxosAtTxOutRefs = gyQueryUtxosAtTxOutRefs' . gyQueryUTxO

gyQueryUtxosAtTxOutRefsWithDatums :: GYProviders -> [GYTxOutRef] -> IO [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtTxOutRefsWithDatums provider refs =
  case gyQueryUtxosAtTxOutRefsWithDatums' $ gyQueryUTxO provider of
    Nothing -> gyQueryUtxosAtTxOutRefsWithDatumsDefault (gyQueryUtxosAtTxOutRefs provider) (gyLookupDatum provider) refs
    Just f  -> f refs

gyQueryUtxoAtTxOutRef :: GYProviders -> GYTxOutRef -> IO (Maybe GYUTxO)
gyQueryUtxoAtTxOutRef = gyQueryUtxoAtTxOutRef' . gyQueryUTxO

gyQueryUtxoRefsAtAddress :: GYProviders -> GYAddress -> IO [GYTxOutRef]
gyQueryUtxoRefsAtAddress = gyQueryUtxoRefsAtAddress' . gyQueryUTxO

gyGetProtocolParameters :: GYProviders -> IO Api.S.ProtocolParameters
gyGetProtocolParameters = gyGetProtocolParameters' . gyGetParameters

gyGetSystemStart :: GYProviders -> IO SystemStart
gyGetSystemStart = gyGetSystemStart' . gyGetParameters

gyGetEraHistory :: GYProviders -> IO (Api.EraHistory Api.CardanoMode)
gyGetEraHistory = gyGetEraHistory' . gyGetParameters

gyGetStakePools :: GYProviders -> IO (Set Api.S.PoolId)
gyGetStakePools = gyGetStakePools' . gyGetParameters

gyGetSlotConfig :: GYProviders -> IO GYSlotConfig
gyGetSlotConfig = gyGetSlotConfig' . gyGetParameters

-------------------------------------------------------------------------------
-- Lookup datum
-------------------------------------------------------------------------------

-- | How to query a datum by its hash?
type GYLookupDatum = GYDatumHash -> IO (Maybe GYDatum)

-------------------------------------------------------------------------------
-- Submit tx
-------------------------------------------------------------------------------

-- | How to submit a transaction?
type GYSubmitTx = GYTx -> IO GYTxId

-------------------------------------------------------------------------------
-- Await tx confirmation
-------------------------------------------------------------------------------

-- | How to await for a transaction confirmation?
type GYAwaitTx = GYAwaitTxParameters -> GYTxId -> IO ()

-- | Await transaction parameters.
data GYAwaitTxParameters = GYAwaitTxParameters
                           { maxAttempts   :: !Int
                           -- ^ Max number of attempts before give up.
                           , checkInterval :: !Int
                           -- ^ Wait time for each attempt (in microseconds).
                           , confirmations :: !Word64
                           -- ^ Min number of block confirmation. __NOTE:__ We might wait for more blocks than what is mentioned here but certainly not less.
                           }
    deriving stock (Show)

instance Default GYAwaitTxParameters where
    def = GYAwaitTxParameters
          { maxAttempts   = 10
          , checkInterval = 3_000_000
          , confirmations = 1
          }

newtype GYAwaitTxException = GYAwaitTxException GYAwaitTxParameters
    deriving anyclass (Exception)

instance Show GYAwaitTxException where
    show (GYAwaitTxException awaitTxParams) =
        "Tries exceeded, given maximum: " ++ show awaitTxParams

-------------------------------------------------------------------------------
-- Current slot
-------------------------------------------------------------------------------

-- | How to get current slot?
data GYSlotActions = GYSlotActions
    { gyGetSlotOfCurrentBlock' :: !(IO GYSlot)
    , gyWaitForNextBlock'      :: !(IO GYSlot)
    , gyWaitUntilSlot'         :: !(GYSlot -> IO GYSlot)
    }

-- | Wait for the next block.
--
-- 'threadDelay' until current slot getter returns another value.
gyWaitForNextBlockDefault :: IO GYSlot -> IO GYSlot
gyWaitForNextBlockDefault getSlotOfCurrentBlock = do
    s <- getSlotOfCurrentBlock
    go s
  where
    go :: GYSlot -> IO GYSlot
    go s = do
        threadDelay 100_000
        t <- getSlotOfCurrentBlock
        if t > s
            then return t
            else go s

-- | Wait until slot.
--
-- Returns the new current slot, which might be larger.
gyWaitUntilSlotDefault :: IO GYSlot -> GYSlot -> IO GYSlot
gyWaitUntilSlotDefault getSlotOfCurrentBlock s = loop
  where
    loop :: IO GYSlot
    loop = do
        t <- getSlotOfCurrentBlock
        if t >= s
            then return t
            else do
                threadDelay 100_000
                loop

-- | Contains the data, alongside the time after which it should be refetched.
data GYSlotStore = GYSlotStore !UTCTime !GYSlot

{- | Construct efficient 'GYSlotActions' methods by ensuring the supplied getSlotOfCurrentBlock is only made after
a given duration of time has passed.

This uses IO to set up some mutable references used for caching.
-}
makeSlotActions :: NominalDiffTime
                -- ^ The time to cache current slots for.
                -> IO GYSlot
                -- ^ Getting current slot directly from the provider
                -> IO GYSlotActions
makeSlotActions t getSlotOfCurrentBlock = do
    slotRefetchTime <- addUTCTime t <$> getCurrentTime
    initSlot        <- getSlotOfCurrentBlock
    slotStoreRef    <- newMVar $ GYSlotStore slotRefetchTime initSlot
    let gcs = getSlotOfCurrentBlock' slotStoreRef
    pure GYSlotActions
        { gyGetSlotOfCurrentBlock' = gcs
        , gyWaitForNextBlock'      = gyWaitForNextBlockDefault gcs
        , gyWaitUntilSlot'         = gyWaitUntilSlotDefault gcs
        }
  where
    getSlotOfCurrentBlock' :: MVar GYSlotStore -> IO GYSlot
    getSlotOfCurrentBlock' var = do
        -- See note: [Caching and concurrently accessible MVars].
        modifyMVar var $ \(GYSlotStore slotRefetchTime slotData) -> do
            now <- getCurrentTime
            if now < slotRefetchTime then do
                -- Return unmodified.
                pure (GYSlotStore slotRefetchTime slotData, slotData)
            else do
                newSlot <- getSlotOfCurrentBlock
                newNow <- getCurrentTime
                let newSlotRefetchTime = addUTCTime t newNow
                pure (GYSlotStore newSlotRefetchTime newSlot, newSlot)

-------------------------------------------------------------------------------
-- Protocol parameters
-------------------------------------------------------------------------------

-- | How to get protocol parameters? ... and other data to do balancing.
data GYGetParameters = GYGetParameters
    { gyGetProtocolParameters' :: !(IO Api.S.ProtocolParameters)
    , gyGetSystemStart'        :: !(IO SystemStart)
    , gyGetEraHistory'         :: !(IO (Api.EraHistory Api.CardanoMode))
    , gyGetStakePools'         :: !(IO (Set Api.S.PoolId))
    , gyGetSlotConfig'         :: !(IO GYSlotConfig)
    }

-- | Contains the data, optionally alongside the slot after which it should be refetched.
data GYParameterStore a = GYParameterStore !(Maybe GYSlot) !a

{- | Construct efficient 'GYGetParameters' methods by ensuring the supplied IO queries are only made when necessary.

This uses IO to set up some mutable references used for caching.
-}
makeGetParameters :: IO GYSlot
                -- ^ Getting current slot
                -> IO Api.S.ProtocolParameters
                -- ^ Getting protocol parameters
                -> IO SystemStart
                -- ^ Getting system start
                -> IO (Api.EraHistory Api.CardanoMode)
                -- ^ Getting era history
                -> IO (Set Api.S.PoolId)
                -- ^ Getting stake pools
                -> IO GYGetParameters
makeGetParameters getSlotOfCurrentBlock getProtParams getSysStart getEraHist getStkPools = do
    sysStart       <- getSysStart
    let getSlotConf = makeSlotConfigIO sysStart
    initProtParams <- getProtParams
    initEraHist    <- getEraHist
    initStkPools   <- getStkPools
    initSlotConf   <- getSlotConf initEraHist

    let buildParam :: a -> GYParameterStore a
        buildParam     = GYParameterStore (slotFromApi <$!> getEraEndSlot initEraHist)
        getProtParams' = newMVar (buildParam initProtParams) >>= mkMethod (const getProtParams)
        getEraHist'    = newMVar (buildParam initEraHist)    >>= mkMethod pure
        getStkPools'   = newMVar (buildParam initStkPools)   >>= mkMethod (const getStkPools)
        getSlotConf'   = newMVar (buildParam initSlotConf)   >>= mkMethod getSlotConf
    pure $ GYGetParameters
        { gyGetSystemStart' = pure sysStart
        , gyGetProtocolParameters' = getProtParams'
        , gyGetEraHistory' = getEraHist'
        , gyGetStakePools' = getStkPools'
        , gyGetSlotConfig' = getSlotConf'
        }
  where
    beforeEnd _ Nothing               = True
    beforeEnd currSlot (Just endSlot) = currSlot < endSlot
    {- | Make an efficient 'GYGetParameters' method.
    This will only refresh the data (using the provided 'dataRefreshF') if current slot has passed the
    era end. It will also update the 'eraEndSlotRef' to the new era end when necessary.

    If refreshing is not necessary, the data is simply returned from the storage.
    -}
    mkMethod :: (Api.EraHistory Api.CardanoMode -> IO a) -> MVar (GYParameterStore a) -> IO a
    mkMethod dataRefreshF dataRef = do
        -- See note: [Caching and concurrently accessible MVars].
        modifyMVar dataRef $ \(GYParameterStore eraEndSlot a) -> do
            currSlot <- getSlotOfCurrentBlock
            if beforeEnd currSlot eraEndSlot then
                pure (GYParameterStore eraEndSlot a, a)
            else do
                newEraHist <- getEraHist
                newData    <- dataRefreshF newEraHist
                pure (GYParameterStore (slotFromApi <$> getEraEndSlot newEraHist) newData, newData)
    makeSlotConfigIO sysStart = either
        (throwIO . GYConversionException . GYEraSummariesToSlotConfigError . Txt.pack)
        pure
        . makeSlotConfig sysStart

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

-- | How to query utxos?
data GYQueryUTxO = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefs'             :: !([GYTxOutRef] -> IO GYUTxOs)
    , gyQueryUtxosAtTxOutRefsWithDatums'   :: !(Maybe ([GYTxOutRef] -> IO [(GYUTxO, Maybe GYDatum)]))
    -- ^ `gyQueryUtxosAtTxOutRefsWithDatums'` is as `Maybe` so that if an implementation is not given, a default one is used.
    , gyQueryUtxoAtTxOutRef'               :: !(GYTxOutRef -> IO (Maybe GYUTxO))
    , gyQueryUtxoRefsAtAddress'            :: !(GYAddress -> IO [GYTxOutRef])
    , gyQueryUtxosAtAddress'               :: !(GYAddress -> Maybe GYAssetClass -> IO GYUTxOs)
    , gyQueryUtxosAtAddressWithDatums'     :: !(Maybe (GYAddress -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]))
    , gyQueryUtxosAtAddresses'             :: !([GYAddress] -> IO GYUTxOs)
    , gyQueryUtxosAtAddressesWithDatums'   :: !(Maybe ([GYAddress] -> IO [(GYUTxO, Maybe GYDatum)]))
    -- ^ `gyQueryUtxosAtAddressesWithDatums'` is as `Maybe` so that if an implementation is not given, a default one is used.
    , gyQueryUtxosAtPaymentCredential'     :: !(GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs)
    , gyQueryUtxosAtPaymentCredWithDatums' :: !(Maybe (GYPaymentCredential -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]))
    -- ^ `gyQueryUtxosAtPaymentCredWithDatums'` is as `Maybe` so that if an implementation is not given, a default one is used.
    , gyQueryUtxosAtPaymentCredentials'    :: !([GYPaymentCredential] -> IO GYUTxOs)
    , gyQueryUtxosAtPaymentCredsWithDatums'
                                           :: !(Maybe ([GYPaymentCredential] -> IO [(GYUTxO, Maybe GYDatum)]))
    -- ^ `gyQueryUtxosAtPaymentCredsWithDatums'` is as `Maybe` so that if an implementation is not given, a default one is used.
    }

-- | Query Utxo Refs at address (default implementation)
gyQueryUtxoRefsAtAddressDefault :: (GYAddress -> Maybe GYAssetClass -> IO GYUTxOs) -> GYAddress -> IO [GYTxOutRef]
gyQueryUtxoRefsAtAddressDefault queryUtxosAtAddress = fmap utxosRefs . (`queryUtxosAtAddress` Nothing)

-- | Query Utxo for addresses (default implementation)
gyQueryUtxoAtAddressesDefault :: (GYAddress -> Maybe GYAssetClass -> IO GYUTxOs) -> [GYAddress] -> IO GYUTxOs
gyQueryUtxoAtAddressesDefault queryUtxosAtAddress addrs = do
  utxos <- traverse (`queryUtxosAtAddress` Nothing) addrs
  pure $ mconcat utxos

-- | Query Utxo for payment credentials (default implementation)
gyQueryUtxoAtPaymentCredentialsDefault :: (GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs) -> [GYPaymentCredential] -> IO GYUTxOs
gyQueryUtxoAtPaymentCredentialsDefault queryUtxosAtPaymentCredential pcs = do
  utxos <- traverse (`queryUtxosAtPaymentCredential` Nothing) pcs
  pure $ mconcat utxos

-- | Query Utxos at output refs (default implementation).
gyQueryUtxosAtTxOutRefsDefault :: (GYTxOutRef -> IO (Maybe GYUTxO)) -> [GYTxOutRef] -> IO GYUTxOs
gyQueryUtxosAtTxOutRefsDefault queryUtxoAtTxOutRef orefs = do
  utxos <- traverse queryUtxoAtTxOutRef orefs
  pure $ utxosFromList $ catMaybes utxos

-- | Lookup UTxOs at given 'GYAddress' with their datums. This is a default implementation using `utxosAtAddress` and `lookupDatum`.
gyQueryUtxosAtAddressWithDatumsDefault :: Monad m => (GYAddress -> Maybe GYAssetClass -> m GYUTxOs) -> (GYDatumHash -> m (Maybe GYDatum)) -> GYAddress -> Maybe GYAssetClass -> m [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtAddressWithDatumsDefault utxosAtAddressFun lookupDatumFun addr mAssetClass = do
  utxosWithoutDatumResolutions <- utxosAtAddressFun addr mAssetClass
  utxosDatumResolver utxosWithoutDatumResolutions lookupDatumFun

-- | Lookup UTxOs at zero or more 'GYAddress' with their datums. This is a default implementation using `utxosAtAddresses` and `lookupDatum`.
gyQueryUtxosAtAddressesWithDatumsDefault :: Monad m => ([GYAddress] -> m GYUTxOs) -> (GYDatumHash -> m (Maybe GYDatum)) -> [GYAddress] -> m [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtAddressesWithDatumsDefault utxosAtAddressesFun lookupDatumFun addrs = do
  utxosWithoutDatumResolutions <- utxosAtAddressesFun addrs
  utxosDatumResolver utxosWithoutDatumResolutions lookupDatumFun

-- | Lookup UTxOs at zero or more 'GYPaymentCredential' with their datums. This is a default implementation using `utxosAtPaymentCredentials` and `lookupDatum`.
gyQueryUtxosAtPaymentCredsWithDatumsDefault :: Monad m => ([GYPaymentCredential] -> m GYUTxOs) -> (GYDatumHash -> m (Maybe GYDatum)) -> [GYPaymentCredential] -> m [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtPaymentCredsWithDatumsDefault utxosAtPaymentCredsFun lookupDatumFun pcs = do
  utxosWithoutDatumResolutions <- utxosAtPaymentCredsFun pcs
  utxosDatumResolver utxosWithoutDatumResolutions lookupDatumFun

-- | Lookup UTxOs at given 'GYPaymentCredential' with their datums. This is a default implementation using `utxosAtPaymentCredential` and `lookupDatum`.
gyQueryUtxosAtPaymentCredWithDatumsDefault :: Monad m => (GYPaymentCredential -> Maybe GYAssetClass -> m GYUTxOs) -> (GYDatumHash -> m (Maybe GYDatum)) -> GYPaymentCredential -> Maybe GYAssetClass -> m [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtPaymentCredWithDatumsDefault utxosAtPaymentCredFun lookupDatumFun cred mAssetClass = do
  utxosWithoutDatumResolutions <- utxosAtPaymentCredFun cred mAssetClass
  utxosDatumResolver utxosWithoutDatumResolutions lookupDatumFun

-- | Append UTxO information with their fetched datum.
utxosDatumResolver :: Monad m => GYUTxOs -> (GYDatumHash -> m (Maybe GYDatum)) -> m [(GYUTxO, Maybe GYDatum)]
utxosDatumResolver utxos lookupDatumFun = do
  let utxosWithoutDatumResolutions = utxosToList utxos
  forM utxosWithoutDatumResolutions $ \utxo -> do
    case utxoOutDatum utxo of
      GYOutDatumNone     -> return (utxo, Nothing)
      GYOutDatumInline d -> return (utxo, Just d)
      GYOutDatumHash h   -> (utxo, ) <$> lookupDatumFun h

-- | Lookup UTxOs at zero or more 'GYTxOutRef' with their datums. This is a default implementation using `utxosAtTxOutRefs` and `lookupDatum`.
gyQueryUtxosAtTxOutRefsWithDatumsDefault :: Monad m => ([GYTxOutRef] -> m GYUTxOs) -> (GYDatumHash -> m (Maybe GYDatum)) -> [GYTxOutRef] -> m [(GYUTxO, Maybe GYDatum)]
gyQueryUtxosAtTxOutRefsWithDatumsDefault utxosAtTxOutRefsFun lookupDatumFun refs = do
  utxosWithoutDatumResolutions <- utxosToList <$> utxosAtTxOutRefsFun refs
  forM utxosWithoutDatumResolutions $ \utxo -> do
    case utxoOutDatum utxo of
      GYOutDatumNone     -> return (utxo, Nothing)
      GYOutDatumInline d -> return (utxo, Just d)
      GYOutDatumHash h   -> (utxo, ) <$> lookupDatumFun h

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

data GYLog = GYLog
    { logRun     :: HasCallStack => GYLogNamespace -> GYLogSeverity -> String -> IO ()
    , logCleanUp :: IO ()
    }

gyLog :: (HasCallStack, MonadIO m) => GYProviders -> GYLogNamespace -> GYLogSeverity -> String -> m ()
gyLog providers ns s = liftIO . logRun (gyLog' providers) ns s

gyLogDebug, gyLogInfo, gyLogWarning, gyLogError :: (HasCallStack, MonadIO m) => GYProviders -> GYLogNamespace -> String -> m ()
gyLogDebug   p ns = gyLog p ns GYDebug
gyLogInfo    p ns = gyLog p ns GYInfo
gyLogWarning p ns = gyLog p ns GYWarning
gyLogError   p ns = gyLog p ns GYError

noLogging :: GYLog
noLogging = GYLog
    { logRun     = \_ns _s _msg -> return ()
    , logCleanUp = return ()
    }

simpleConsoleLogging
    :: (String -> IO ())   -- ^ putStrLn variant
    -> GYLog
simpleConsoleLogging f = GYLog
    { logRun     = \ns _s msg -> f $ printf "*** [%s] %s" ns msg
    , logCleanUp = return ()
    }
