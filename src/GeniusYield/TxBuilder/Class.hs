{-# LANGUAGE InstanceSigs #-}
{-|
Module      : GeniusYield.TxBuilder.Class
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Class
    ( MonadError (..)
    , MonadRandom (..)
    , GYTxMonad (..)
    , GYTxQueryMonad (..)
    , GYTxSkeleton (..)
    , GYTxSkeletonRefIns (..)
    , gyTxSkeletonRefInsToList
    , RandT
    , lookupDatum'
    , utxoAtTxOutRef'
    , slotToBeginTime
    , slotToEndTime
    , enclosingSlotFromTime
    , enclosingSlotFromTime'
    , scriptAddress
    , scriptAddress'
    , addressFromText'
    , addressFromPlutusM
    , addressFromPlutusHushedM
    , addressFromPlutus'
    , addressToPubKeyHash'
    , addressToPubKeyHashIO
    , addressToValidatorHash'
    , addressToValidatorHashIO
    , valueFromPlutus'
    , valueFromPlutusIO
    , makeAssetClass'
    , makeAssetClassIO
    , assetClassFromPlutus'
    , tokenNameFromPlutus'
    , txOutRefFromPlutus'
    , datumHashFromPlutus'
    , pubKeyHashFromPlutus'
    , advanceSlot'
    , utxosDatums
    , utxoDatum
    , utxoDatumHushed
    , utxoDatum'
    , mustHaveInput
    , mustHaveRefInput
    , mustHaveOutput
    , mustHaveOptionalOutput
    , mustMint
    , mustBeSignedBy
    , isInvalidBefore
    , isInvalidAfter
    , gyLogDebug'
    , gyLogInfo'
    , gyLogWarning'
    , gyLogError'
    , skeletonToRefScriptsORefs
    ) where

import           Control.Monad.Except         (ExceptT, MonadError (..), liftEither)
import           Control.Monad.Random         (MonadRandom (..), RandT, lift)
import           Control.Monad.Reader         (ReaderT)
import           Data.List                    (nubBy)

import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as Txt
import qualified Plutus.V1.Ledger.Api         as Plutus
import qualified Plutus.V1.Ledger.Value       as PlutusValue

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class of monads for querying chain data.
class MonadError GYTxMonadException m => GYTxQueryMonad m where
    {-# MINIMAL networkId, lookupDatum, (utxoAtTxOutRef | utxosAtTxOutRefs), (utxosAtAddress | utxosAtAddresses), slotConfig, currentSlot, logMsg #-}

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

    -- | Lookup 'GYUTxOs' at multiple 'GYTxOutRef's at once
    utxosAtTxOutRefs :: [GYTxOutRef] -> m GYUTxOs
    utxosAtTxOutRefs orefs = utxosFromList <$> wither utxoAtTxOutRef orefs

    -- | Lookup 'GYUTxOs' at 'GYAddress'.
    utxosAtAddress :: GYAddress -> m GYUTxOs
    utxosAtAddress = utxosAtAddresses . return

    -- | Lookup 'GYUTxOs' at zero or more 'GYAddress'.
    utxosAtAddresses :: [GYAddress] -> m GYUTxOs
    utxosAtAddresses = foldM f mempty
      where
        f :: GYUTxOs -> GYAddress -> m GYUTxOs
        f utxos addr = (<> utxos) <$> utxosAtAddress addr

    -- | Lookup the `[GYTxOutRef]`s at a `GYAddress`
    utxoRefsAtAddress :: GYAddress -> m [GYTxOutRef]
    utxoRefsAtAddress = fmap (Map.keys . mapUTxOs id) . utxosAtAddress

    {- | Obtain the slot config for the network.

    Implementations using era history to create slot config may raise 'GYEraSummariesToSlotConfigError'.
    -}
    slotConfig :: m GYSlotConfig

    -- | Lookup the current 'GYSlot'.
    currentSlot :: m GYSlot

    -- | Log a message with specified namespace and severity.
    logMsg :: HasCallStack => GYLogNamespace -> GYLogSeverity -> String -> m ()

-- | Class of monads for querying monads as a user.
class GYTxQueryMonad m => GYTxMonad m where
    -- | Return some unspend transaction output
    --
    -- /Note:/ may or may not return the same value
    someUTxO :: m GYTxOutRef

    -- | A seed to inject non-determinism.
    randSeed :: m Int

instance GYTxQueryMonad m => GYTxQueryMonad (RandT g m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtAddress = lift . utxosAtAddress
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    slotConfig = lift slotConfig
    currentSlot = lift currentSlot
    logMsg ns s = lift . logMsg ns s

instance GYTxMonad m => GYTxMonad (RandT g m) where
    someUTxO = lift someUTxO
    randSeed = lift randSeed

instance GYTxQueryMonad m => GYTxQueryMonad (ReaderT env m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtAddress = lift . utxosAtAddress
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    slotConfig = lift slotConfig
    currentSlot = lift currentSlot
    logMsg ns s = lift . logMsg ns s

instance GYTxMonad m => GYTxMonad (ReaderT g m) where
    someUTxO = lift someUTxO
    randSeed = lift randSeed

instance GYTxQueryMonad m => GYTxQueryMonad (ExceptT GYTxMonadException m) where
    networkId = lift networkId
    lookupDatum = lift . lookupDatum
    utxoAtTxOutRef = lift . utxoAtTxOutRef
    utxosAtTxOutRefs = lift . utxosAtTxOutRefs
    utxosAtAddress = lift . utxosAtAddress
    utxoRefsAtAddress = lift . utxoRefsAtAddress
    slotConfig = lift slotConfig
    currentSlot = lift currentSlot
    logMsg ns s = lift . logMsg ns s

instance GYTxMonad m => GYTxMonad (ExceptT GYTxMonadException m) where
    someUTxO = lift someUTxO
    randSeed = lift randSeed

-- | A version of 'lookupDatum' that raises 'GYNoDatumForHash' if the datum is not found.
lookupDatum' :: GYTxQueryMonad m => GYDatumHash -> m GYDatum
lookupDatum' h = lookupDatum h >>= maybe (throwError . GYQueryDatumException $ GYNoDatumForHash h) pure

-- | A version of 'utxoAtTxOutRef' that raises 'GYNoUtxoAtRef' if the utxo is not found.
utxoAtTxOutRef' :: GYTxQueryMonad m => GYTxOutRef -> m GYUTxO
utxoAtTxOutRef' ref = utxoAtTxOutRef ref
    >>= maybe
        (throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref)
        pure

-------------------------------------------------------------------------------
-- Slot <-> Time conversion functions within the monad
-------------------------------------------------------------------------------

-- | Get the starting 'GYTime' of a 'GYSlot' in 'GYTxMonad'.
slotToBeginTime :: GYTxQueryMonad f => GYSlot -> f GYTime
slotToBeginTime x = flip slotToBeginTimePure x <$> slotConfig

-- | Get the ending 'GYTime' of a 'GYSlot' (inclusive) in 'GYTxMonad'.
slotToEndTime :: GYTxQueryMonad f => GYSlot -> f GYTime
slotToEndTime x = flip slotToEndTimePure x <$> slotConfig

{- | Get the 'GYSlot' of a 'GYTime' in 'GYTxMonad'.

Returns 'Nothing' if given time is before known system start.
-}
enclosingSlotFromTime :: GYTxQueryMonad f => GYTime -> f (Maybe GYSlot)
enclosingSlotFromTime x = flip enclosingSlotFromTimePure x <$> slotConfig

{- | Partial version of 'enclosingSlotFromTime'.

Raises 'GYTimeUnderflowException' if given time is before known system start.
-}
enclosingSlotFromTime' :: GYTxQueryMonad m => GYTime -> m GYSlot
enclosingSlotFromTime' x = do
    sysStart <- gyscSystemStart <$> slotConfig
    enclosingSlotFromTime x >>= maybe (throwError $ GYTimeUnderflowException sysStart x) pure

-------------------------------------------------------------------------------
-- Transaction skeleton
-------------------------------------------------------------------------------

-- | Transaction skeleton
--
-- /Note:/ let's add fields as we need them.
--
-- The parameter @v@ indicates the minimum version of scripts allowed
-- as inputs.
--
data GYTxSkeleton (v :: PlutusVersion) = GYTxSkeleton
    { gytxIns           :: ![GYTxIn v]
    , gytxOuts          :: ![GYTxOut v]
    , gytxRefIns        :: !(GYTxSkeletonRefIns v)
    -- TODO: restrict versions of minting policies similarly to validators.
    , gytxMint          :: !(Map (Some GYMintingPolicy) (Map GYTokenName Integer, GYRedeemer))
    , gytxSigs          :: !(Set GYPubKeyHash)
    , gytxInvalidBefore :: !(Maybe GYSlot)
    , gytxInvalidAfter  :: !(Maybe GYSlot)
    } deriving Show

data GYTxSkeletonRefIns :: PlutusVersion -> Type where
    GYTxSkeletonRefIns :: VersionIsGreaterOrEqual v PlutusV2 => !(Set GYTxOutRef) -> GYTxSkeletonRefIns v
    GYTxSkeletonNoRefIns :: GYTxSkeletonRefIns v

deriving instance Show (GYTxSkeletonRefIns v)
deriving instance Eq (GYTxSkeletonRefIns v)

gyTxSkeletonRefInsToList :: GYTxSkeletonRefIns v -> [GYTxOutRef]
gyTxSkeletonRefInsToList (GYTxSkeletonRefIns xs) = Set.toList xs
gyTxSkeletonRefInsToList GYTxSkeletonNoRefIns    = []

instance Semigroup (GYTxSkeletonRefIns v) where
    GYTxSkeletonRefIns a <> GYTxSkeletonRefIns b = GYTxSkeletonRefIns (Set.union a b)
    GYTxSkeletonRefIns a <> GYTxSkeletonNoRefIns = GYTxSkeletonRefIns a
    GYTxSkeletonNoRefIns <> GYTxSkeletonRefIns b = GYTxSkeletonRefIns b
    GYTxSkeletonNoRefIns <> GYTxSkeletonNoRefIns = GYTxSkeletonNoRefIns

emptyGYTxSkeleton :: GYTxSkeleton v
emptyGYTxSkeleton = GYTxSkeleton
    { gytxIns           = []
    , gytxOuts          = []
    , gytxRefIns        = GYTxSkeletonNoRefIns
    , gytxMint          = Map.empty
    , gytxSigs          = Set.empty
    , gytxInvalidBefore = Nothing
    , gytxInvalidAfter  = Nothing
    }

instance Semigroup (GYTxSkeleton v) where
    x <> y = GYTxSkeleton
        { gytxIns           = combineIns (gytxIns x) (gytxIns y)
        , gytxOuts          = gytxOuts x ++ gytxOuts y
        , gytxRefIns        = gytxRefIns x <> gytxRefIns y
        , gytxMint          = combineMint (gytxMint x) (gytxMint y)
        , gytxSigs          = Set.union (gytxSigs x) (gytxSigs y)
        , gytxInvalidBefore = combineInvalidBefore (gytxInvalidBefore x) (gytxInvalidBefore y)
        , gytxInvalidAfter  = combineInvalidAfter (gytxInvalidAfter x) (gytxInvalidAfter y)
        }
      where
        -- we keep only one input per utxo to spend
        combineIns u v = nubBy ((==) `on` gyTxInTxOutRef) (u ++ v)
        -- we cannot combine redeemers, so we just pick first.
        combineMint = Map.unionWith (\(amt, r) (amt', _r) -> (Map.unionWith (+) amt amt', r))

        combineInvalidBefore :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
        combineInvalidBefore m        Nothing  = m
        combineInvalidBefore Nothing  n        = n
        combineInvalidBefore (Just s) (Just t) = Just (max s t)

        combineInvalidAfter :: Maybe GYSlot -> Maybe GYSlot -> Maybe GYSlot
        combineInvalidAfter m        Nothing  = m
        combineInvalidAfter Nothing  n        = n
        combineInvalidAfter (Just s) (Just t) = Just (min s t)

instance Monoid (GYTxSkeleton v) where
    mempty = emptyGYTxSkeleton

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Calculate script's address.
scriptAddress :: GYTxQueryMonad m => GYValidator v -> m GYAddress
scriptAddress v = do
    nid <- networkId
    return $ addressFromValidator nid v

-- | Calculate script's address.
scriptAddress' :: GYTxQueryMonad m => GYValidatorHash -> m GYAddress
scriptAddress' h = do
    nid <- networkId
    return $ addressFromValidatorHash nid h

-- | Convert a 'Plutus.Address' to 'GYAddress' in 'GYTxMonad'.
--
-- Explicitly returns an error rather than throwing it.
addressFromPlutusM :: GYTxQueryMonad m => Plutus.Address -> m (Either PlutusToCardanoError GYAddress)
addressFromPlutusM addr = flip addressFromPlutus addr <$> networkId

-- | 'hush'ed version of 'addressFromPlutusM'.
addressFromPlutusHushedM :: GYTxQueryMonad m => Plutus.Address -> m (Maybe GYAddress)
addressFromPlutusHushedM addr = fmap hush $ flip addressFromPlutus addr <$> networkId

-- | Convert a 'Plutus.Address' to 'GYAddress' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
addressFromPlutus' :: GYTxQueryMonad m => Plutus.Address -> m GYAddress
addressFromPlutus' addr = do
    x <- addressFromPlutusM addr
    liftEither $ first (GYConversionException . GYLedgerToCardanoError) x

-- | Convert 'GYAddress' to 'GYPubKeyHash' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if address is not key-hash one.
addressToPubKeyHash' :: MonadError GYTxMonadException m => GYAddress -> m GYPubKeyHash
addressToPubKeyHash' addr = maybe
    (throwError . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToPubKeyHash addr)

addressToPubKeyHashIO :: GYAddress -> IO GYPubKeyHash
addressToPubKeyHashIO addr = maybe
    (throwIO . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToPubKeyHash addr)

-- | Convert 'GYAddress' to 'GYValidatorHash' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if address is not script-hash one.
addressToValidatorHash' :: MonadError GYTxMonadException m => GYAddress -> m GYValidatorHash
addressToValidatorHash' addr = maybe
    (throwError . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToValidatorHash addr)

addressToValidatorHashIO :: GYAddress -> IO GYValidatorHash
addressToValidatorHashIO addr = maybe
    (throwIO . GYConversionException $ GYNotScriptAddress addr)
    pure
    (addressToValidatorHash addr)

-- | Convert a 'Plutus.Value' to 'GYValue' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
valueFromPlutus' :: MonadError GYTxMonadException m => Plutus.Value -> m GYValue
valueFromPlutus' val = either
    (throwError . GYConversionException . flip GYInvalidPlutusValue val)
    pure
    (valueFromPlutus val)

-- | Convert a 'Plutus.Value' to 'GYValue' in 'IO'.
--
-- Throw 'GYConversionException' if conversion fails.
valueFromPlutusIO :: PlutusValue.Value -> IO GYValue
valueFromPlutusIO val = either
    (throwIO . GYConversionException . flip GYInvalidPlutusValue val)
    pure
    (valueFromPlutus val)

-- | Create a 'GYAssetClass' from the textual representation of currency symbol and token name in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
makeAssetClass' :: MonadError GYTxMonadException m => Text -> Text -> m GYAssetClass
makeAssetClass' a b = either
    (throwError . GYConversionException . GYInvalidAssetClass . Txt.pack)
    pure
    (makeAssetClass a b)

-- | 'makeAssetClass'' in the IO monad.
--
-- Throw 'GYConversionException' if conversion fails.
makeAssetClassIO :: Text -> Text -> IO GYAssetClass
makeAssetClassIO a b = either
    (throwIO . GYConversionException . GYInvalidAssetClass . Txt.pack)
    pure
    (makeAssetClass a b)

-- | Convert a 'PlutusValue.AssetClass' to 'GYAssetClass' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
assetClassFromPlutus' :: MonadError GYTxMonadException m => PlutusValue.AssetClass -> m GYAssetClass
assetClassFromPlutus' x = either
    (throwError . GYConversionException . GYInvalidPlutusAsset)
    pure
    (assetClassFromPlutus x)

-- | Convert a 'PlutusValue.TokenName' to 'GYTokenName' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
tokenNameFromPlutus' :: MonadError GYTxMonadException m => PlutusValue.TokenName -> m GYTokenName
tokenNameFromPlutus' x = maybe
    (throwError . GYConversionException . GYInvalidPlutusAsset $ GYTokenNameTooBig x)
    pure
    (tokenNameFromPlutus x)

-- | Convert a 'Plutus.TxOutRef' to 'GYTxOutRef' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
txOutRefFromPlutus' :: MonadError GYTxMonadException m => Plutus.TxOutRef -> m GYTxOutRef
txOutRefFromPlutus' ref = either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (txOutRefFromPlutus ref)

-- | Convert a 'Plutus.DatumHash' to 'GYDatumHash' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
datumHashFromPlutus' :: MonadError GYTxMonadException m => Plutus.DatumHash -> m GYDatumHash
datumHashFromPlutus' dh = either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (datumHashFromPlutus dh)

-- | Convert a 'Plutus.PubKeyHash' to 'GYPubKeyHash' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
pubKeyHashFromPlutus' :: MonadError GYTxMonadException m => Plutus.PubKeyHash -> m GYPubKeyHash
pubKeyHashFromPlutus' pkh = either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (pubKeyHashFromPlutus pkh)

-- | Parse the bech32 representation of an address into 'GYAddress' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if parsing fails.
addressFromText' :: MonadError GYTxMonadException m => Text -> m GYAddress
addressFromText' addr = maybe
    (throwError . GYConversionException $ GYInvalidAddressText addr)
    pure
    (addressFromTextMaybe addr)

-- | Advance 'GYSlot' forward in 'GYTxMonad'. If slot value overflows, throw 'GYSlotOverflowException'.
advanceSlot' :: MonadError GYTxMonadException m => GYSlot -> Natural -> m GYSlot
advanceSlot' s t = maybe
    (throwError $ GYSlotOverflowException s t)
    pure
    (advanceSlot s t)

utxosDatums :: forall m a. (GYTxQueryMonad m, Plutus.FromData a) => GYUTxOs -> m (Map GYTxOutRef (GYAddress, GYValue, a))
utxosDatums = witherUTxOs utxoDatumHushed

utxoDatum :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (Either GYQueryDatumError (GYAddress, GYValue, a))
utxoDatum utxo = case utxoOutDatum utxo of
    GYOutDatumNone -> pure . Left $ GYNoDatumHash utxo
    GYOutDatumHash h  -> do
        md <- lookupDatum h
        case md of
            Nothing -> pure . Left $ GYNoDatumForHash h
            Just d  -> datumToRes d
    GYOutDatumInline d -> datumToRes d
  where
    datumToRes x = case Plutus.fromBuiltinData $ datumToPlutus' x of
        Nothing -> pure . Left $ GYInvalidDatum x
        Just a  -> pure $ Right (utxoAddress utxo, utxoValue utxo, a)

-- | Version of 'utxoDatum' that throws 'GYTxMonadException'.
utxoDatum' :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (GYAddress, GYValue, a)
utxoDatum' utxo = do
    x <- utxoDatum utxo
    liftEither $ first GYQueryDatumException x

utxoDatumHushed :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (Maybe (GYAddress, GYValue, a))
utxoDatumHushed = fmap hush . utxoDatum

mustHaveInput :: GYTxIn v -> GYTxSkeleton v
mustHaveInput i = emptyGYTxSkeleton {gytxIns = [i]}

mustHaveRefInput :: VersionIsGreaterOrEqual v PlutusV2 => GYTxOutRef -> GYTxSkeleton v
mustHaveRefInput i = emptyGYTxSkeleton { gytxRefIns = GYTxSkeletonRefIns (Set.singleton i) }

mustHaveOutput :: GYTxOut v -> GYTxSkeleton v
mustHaveOutput o = emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveOptionalOutput :: Maybe (GYTxOut v) -> GYTxSkeleton v
mustHaveOptionalOutput = maybe mempty $ \o -> emptyGYTxSkeleton {gytxOuts = [o]}

mustMint :: GYMintingPolicy u -> GYRedeemer -> GYTokenName -> Integer -> GYTxSkeleton v
mustMint _ _ _ 0  = mempty
mustMint p r tn n = emptyGYTxSkeleton {gytxMint = Map.singleton (Some p) (Map.singleton tn n, r)}

mustBeSignedBy :: GYPubKeyHash -> GYTxSkeleton v
mustBeSignedBy pkh = emptyGYTxSkeleton {gytxSigs = Set.singleton pkh}

isInvalidBefore :: GYSlot -> GYTxSkeleton v
isInvalidBefore s = emptyGYTxSkeleton {gytxInvalidBefore = Just s}

isInvalidAfter :: GYSlot -> GYTxSkeleton v
isInvalidAfter s = emptyGYTxSkeleton {gytxInvalidAfter = Just s}

gyLogDebug', gyLogInfo', gyLogWarning', gyLogError' :: GYTxQueryMonad m => GYLogNamespace -> String -> m ()
gyLogDebug'   ns = logMsg ns GYDebug
gyLogInfo'    ns = logMsg ns GYInfo
gyLogWarning' ns = logMsg ns GYWarning
gyLogError'   ns = logMsg ns GYError

-- | Given a skeleton, returns a list of reference to reference script UTxOs which are present as witness.
skeletonToRefScriptsORefs :: GYTxSkeleton v -> [GYTxOutRef]
skeletonToRefScriptsORefs GYTxSkeleton{ gytxIns } = go gytxIns []
  where
    go :: [GYTxIn v] -> [GYTxOutRef] -> [GYTxOutRef]
    go [] acc = acc
    go (gytxIn : rest) acc = case gyTxInWitness gytxIn of
      GYTxInWitnessScript gyInScript _ _ -> case gyInScript of
          GYInReference oRef _ -> go rest (oRef : acc)
          _anyOtherMatch       -> go rest acc
      _anyOtherMatch -> go rest acc
