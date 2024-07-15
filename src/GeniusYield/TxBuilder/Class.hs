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
    , GYTxGameMonad (..)
    , GYTxMonad (..)
    , GYTxQueryMonad (..)
    , GYTxSpecialQueryMonad (..)
    , GYTxUserQueryMonad (..)
    , GYTxSkeleton (..)
    , GYTxSkeletonRefIns (..)
    , waitNSlots
    , submitTx_
    , submitTxConfirmed
    , submitTxConfirmed_
    , submitTxConfirmed'
    , submitTxConfirmed'_
    , submitTxBody
    , submitTxBody_
    , submitTxBodyConfirmed
    , submitTxBodyConfirmed_
    , signAndSubmitConfirmed
    , signAndSubmitConfirmed_
    , awaitTxConfirmed
    , gyTxSkeletonRefInsToList
    , gyTxSkeletonRefInsSet
    , lookupDatum'
    , utxoAtTxOutRef'
    , utxoAtTxOutRefWithDatum'
    , someUTxOWithoutRefScript
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
    , utxosDatumsPure
    , utxosDatumsPureWithOriginalDatum
    , utxoDatum
    , utxoDatumPure
    , utxoDatumPureWithOriginalDatum
    , utxoDatumHushed
    , utxoDatumPureHushed
    , utxoDatumPureHushedWithOriginalDatum
    , utxoDatum'
    , utxoDatumPure'
    , utxoDatumPureWithOriginalDatum'
    , mustHaveInput
    , mustHaveRefInput
    , mustHaveOutput
    , mustHaveOptionalOutput
    , mustHaveTxMetadata
    , mustMint
    , mustHaveWithdrawal
    , mustHaveCertificate
    , mustBeSignedBy
    , isInvalidBefore
    , isInvalidAfter
    , gyLogDebug'
    , gyLogInfo'
    , gyLogWarning'
    , gyLogError'
    , skeletonToRefScriptsORefs
    , wrapReqWithTimeLog
    , wt
    ) where

import           Control.Monad.Except         (MonadError (..), liftEither)
import qualified Control.Monad.State.Strict   as Strict
import qualified Control.Monad.State.Lazy     as Lazy
import qualified Control.Monad.Writer.CPS     as CPS
import qualified Control.Monad.Writer.Strict  as Strict
import qualified Control.Monad.Writer.Lazy    as Lazy
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Random         (MonadRandom (..), RandT, lift)
import           Control.Monad.Reader         (ReaderT)
import           Data.Default                 (def)
import           Data.List                    (nubBy)
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as Txt
import           Data.Time                    (diffUTCTime, getCurrentTime)
import           Data.Word                    (Word64)
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.TxBuilder.Query.Class
import           GeniusYield.TxBuilder.User
import           GeniusYield.Types
import           GeniusYield.Types.Key.Class  (ToShelleyWitnessSigningKey)
import           GHC.Stack                    (withFrozenCallStack)
import qualified PlutusLedgerApi.V1           as Plutus (Address, DatumHash,
                                                         FromData (..),
                                                         PubKeyHash, TokenName,
                                                         TxOutRef, Value)
import qualified PlutusLedgerApi.V1.Value     as Plutus (AssetClass)


-- | Class of monads for interacting with the blockchain using transactions.
class (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m) => GYTxMonad m where
    -- | Sign a transaction body with the user payment key to produce a transaction with witnesses.
    --
    -- /Note:/ The key is not meant to be exposed to the monad, so it is only held
    -- within the closure that signs a given transaction.
    -- It is recommended to use 'signGYTxBody' and similar to implement this method.
    signTxBody :: GYTxBody -> m GYTx

    -- | Sign a transaction body with the user payment key AND user stake key to produce
    -- a transaction with witnesses.
    -- If the user wallet does not have a stake key, this function should be equivalent to
    -- 'signTxBody'.
    --
    -- See note on 'signTxBody'
    signTxBodyWithStake :: GYTxBody -> m GYTx

    -- | Submit a fully built transaction to the chain.
    --   Use 'buildTxBody' to build a transaction body, and 'signGYTxBody' to
    --   sign it before submitting.
    --
    -- /Note:/ Changes made to the chain by the submitted transaction may not be reflected immediately,
    -- see 'awaitTxConfirmed'.
    --
    -- /Law:/ 'someUTxO' calls made after a call to 'submitTx' may return previously returned UTxOs
    -- if they were not affected by the submitted transaction.
    submitTx :: GYTx -> m GYTxId

    -- | Wait for a _recently_ submitted transaction to be confirmed.
    --
    -- /Note:/ If used on a transaction submitted long ago, the behavior is undefined.
    --
    -- /Law:/ Queries made after a call to 'awaitTxConfirmed'' should reflect changes made to the chain
    -- by the identified transaction.
    awaitTxConfirmed' :: GYAwaitTxParameters -> GYTxId -> m ()

-- | Class of monads that can simulate a "game" between different users interacting with transactions.
class (GYTxMonad (TxMonadOf m), GYTxSpecialQueryMonad m) => GYTxGameMonad m where
    -- | Type of the supported 'GYTxMonad' instance that can participate within the "game".
    type TxMonadOf m = (r :: Type -> Type) | r -> m
    -- | Lift the supported 'GYTxMonad' instance into the game, as a participating user wallet.
    asUser :: User -> TxMonadOf m a -> m a
    -- | Wait until the chain tip is at given slot number.
    waitUntilSlot :: GYSlot -> m GYSlot
    -- | Wait until the chain tip is at the next block.
    waitForNextBlock :: m GYSlot

{- Note [Higher order effects, TxMonadOf, and GYTxGameMonad]

'GYTxGameMonad' is designed to give the implementor two choices: either make it a different data type
from its associated 'GYTxMonad' instance (such is the case for 'GYTxGameMonadIO' and 'GYTxMonadIO'), or
make the same data type a 'GYTxMonad' and 'GYTxGameMonad'.

The former would not be possible if 'GYTxGameMonad' was subsumed into 'GYTxMonad', or if the 'TxMonadOf' type family
was not present. Thus, both the seperation and the type family are the result of a conscious design decision.

It's important to allow the former case since it avoids making 'asUser' a higher order effect, unconditionally. Higher
order effects can be problematic. If, in the future, we are to use a proper effect system - we'd like to avoid having to
deal with higher order effects wherever feasible.

As to why the type family is injective, the goal is to have a unique 'GYTxMonad' instance for each 'GYTxGameMonad'. This
makes type checking easier regardless of the implementation choice above. Thus, one can have a block of 'GYTxGameMonad' code
with a bunch of 'asUser' calls sprinkled in with blocks of 'GYTxMonad' code, and no extraneous type signatures would be necessary.
Just one type inference (or signature) on the top most call that runs the 'GYTxGameMonad' code block, and all the 'asUser' code blocks
will be automatically inferred.
-}

-- | Wait until the chain tip has progressed by N slots.
waitNSlots :: GYTxGameMonad m => Word64 -> m GYSlot
waitNSlots (slotFromWord64 -> n) = do
    -- FIXME: Does this need to be an absolute slot getter instead?
    currentSlot <- slotOfCurrentBlock
    waitUntilSlot . slotFromApi $ currentSlot `addSlots` n
  where
    addSlots = (+) `on` slotToApi

-- | > submitTx_ = void . submitTx
submitTx_ :: GYTxMonad m => GYTx -> m ()
submitTx_ = void . submitTx

-- | > submitTxConfirmed_ = void . submitTxConfirmed
submitTxConfirmed_ :: GYTxMonad m => GYTx -> m ()
submitTxConfirmed_ = void . submitTxConfirmed

-- | 'submitTxConfirmed'' with default tx waiting parameters.
submitTxConfirmed :: GYTxMonad m => GYTx -> m GYTxId
submitTxConfirmed = submitTxConfirmed' def

-- | > submitTxConfirmed'_ p = void . submitTxConfirmed' p
submitTxConfirmed'_ :: GYTxMonad m => GYAwaitTxParameters -> GYTx -> m ()
submitTxConfirmed'_ awaitParams = void . submitTxConfirmed' awaitParams

-- | Equivalent to a call to 'submitTx' and then a call to 'awaitTxConfirmed'' with submitted tx id.
submitTxConfirmed' :: GYTxMonad m => GYAwaitTxParameters -> GYTx -> m GYTxId
submitTxConfirmed' awaitParams tx = do
    txId <- submitTx tx
    awaitTxConfirmed' awaitParams txId
    pure txId

-- | Wait for a _recently_ submitted transaction to be confirmed, with default waiting parameters.
awaitTxConfirmed :: GYTxMonad m => GYTxId -> m ()
awaitTxConfirmed = awaitTxConfirmed' def

-- | > submitTxBody_ t = void . submitTxBody t
submitTxBody_ :: (GYTxMonad f, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> f ()
submitTxBody_ txBody = void . submitTxBody txBody

-- | Signs a 'GYTxBody' with the given keys and submits the transaction.
-- Equivalent to a call to 'signGYTxBody', followed by a call to 'submitTx'
submitTxBody :: (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m GYTxId
submitTxBody txBody = submitTx . signGYTxBody txBody

-- | > submitTxBodyConfirmed_ t = void . submitTxBodyConfirmed t
submitTxBodyConfirmed_ :: (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m ()
submitTxBodyConfirmed_ txBody = void . submitTxBodyConfirmed txBody

-- | Signs a 'GYTxBody' with the given keys, submits the transaction, and waits for its confirmation.
-- Equivalent to a call to 'signGYTxBody', followed by a call to 'submitTxConfirmed'.
submitTxBodyConfirmed :: (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] ->  m GYTxId
submitTxBodyConfirmed txBody = submitTxConfirmed . signGYTxBody txBody

signAndSubmitConfirmed_ :: GYTxMonad m => GYTxBody -> m ()
signAndSubmitConfirmed_ = void . signAndSubmitConfirmed

signAndSubmitConfirmed :: GYTxMonad m => GYTxBody -> m GYTxId
signAndSubmitConfirmed txBody = signTxBody txBody >>= submitTxConfirmed

-------------------------------------------------------------------------------
-- Instances for useful transformers.
-------------------------------------------------------------------------------

{- Note [GYTxGameMonad instances for transformers]

No 'GYTxGameMonad' instances are provided for any transformer. This is intentional.
The design of 'GYTxGameMonad' has some intricate decisions. See note
"Higher order effects, TxMonadOf, and GYTxGameMonad" above.

Since 'TxMonadOf' is an injective type family, it is impossible to make transformer
instances in the likes of 'TxMonadOf (ReaderT env m) = TxMonadOf m'. Each 'GYTxGameMonad'
instance is expected to have its own unique 'GYTxMonad' instance. So
'GYTxGameMonad m => ReaderT env m' and 'GYTxGameMonad m => m' can't both use the same 'GYTxMonad'
instance.

The solution to this is to simply have a wrapper data type that brings generativity to the table.
Such as 'data ReaderTTxMonad m a = ReaderTTxMonad ((TxMonadOf m) a)' or similar.

Since these wrapper data types are usage specific, and 'GYTxGameMonad' instances are meant to be some
"overarching base" type, we do not provide these instances and users may define them if necessary.
-}


instance GYTxMonad m => GYTxMonad (RandT g m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p


instance GYTxMonad m => GYTxMonad (ReaderT env m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p

-------------------------------------------------------------------------------
-- Instances for less useful transformers, provided for completeness.
-- Many of these transformers are fundamentally riddled with issues.
--    See: https://github.com/haskell-effectful/effectful/blob/master/transformers.md
-------------------------------------------------------------------------------


instance GYTxMonad m => GYTxMonad (Strict.StateT s m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p


instance GYTxMonad m => GYTxMonad (Lazy.StateT s m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p


instance (GYTxMonad m, Monoid w) => GYTxMonad (CPS.WriterT w m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p


instance (GYTxMonad m, Monoid w) => GYTxMonad (Strict.WriterT w m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p


instance (GYTxMonad m, Monoid w) => GYTxMonad (Lazy.WriterT w m) where
    signTxBody = lift . signTxBody
    signTxBodyWithStake = lift . signTxBodyWithStake
    submitTx = lift . submitTx
    awaitTxConfirmed' p = lift . awaitTxConfirmed' p

-- | A version of 'lookupDatum' that raises 'GYNoDatumForHash' if the datum is not found.
lookupDatum' :: GYTxQueryMonad m => GYDatumHash -> m GYDatum
lookupDatum' h = lookupDatum h >>= maybe (throwError . GYQueryDatumException $ GYNoDatumForHash h) pure

-- | A version of 'utxoAtTxOutRef' that raises 'GYNoUtxoAtRef' if the utxo is not found.
utxoAtTxOutRef' :: GYTxQueryMonad m => GYTxOutRef -> m GYUTxO
utxoAtTxOutRef' ref = utxoAtTxOutRef ref
    >>= maybe
        (throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref)
        pure

-- | A version of 'utxoAtTxOutRefWithDatum' that raises 'GYNoUtxoAtRef' if the utxo is not found.
utxoAtTxOutRefWithDatum' :: GYTxQueryMonad m => GYTxOutRef -> m (GYUTxO, Maybe GYDatum)
utxoAtTxOutRefWithDatum' ref = utxoAtTxOutRefWithDatum ref
    >>= maybe
        (throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref)
        pure

-- | Returns some UTxO present in wallet which doesn't have reference script.
someUTxOWithoutRefScript :: GYTxMonad m => m GYTxOutRef
someUTxOWithoutRefScript = do
  utxosToConsider <- utxosRemoveRefScripts <$> availableUTxOs
  addrs           <- ownAddresses
  case someTxOutRef utxosToConsider of
    Just (oref, _) -> return oref
    Nothing        -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Possible to put better error message here?


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
    , gytxMint          :: !(Map (GYMintScript v) (Map GYTokenName Integer, GYRedeemer))
    , gytxWdrls         :: ![GYTxWdrl v]
    , gytxSigs          :: !(Set GYPubKeyHash)
    , gytxCerts         :: ![GYTxCert v]
    , gytxInvalidBefore :: !(Maybe GYSlot)
    , gytxInvalidAfter  :: !(Maybe GYSlot)
    , gytxMetadata      :: !(Maybe GYTxMetadata)
    } deriving Show

data GYTxSkeletonRefIns :: PlutusVersion -> Type where
    GYTxSkeletonRefIns :: VersionIsGreaterOrEqual v 'PlutusV2 => !(Set GYTxOutRef) -> GYTxSkeletonRefIns v
    GYTxSkeletonNoRefIns :: GYTxSkeletonRefIns v

deriving instance Show (GYTxSkeletonRefIns v)
deriving instance Eq (GYTxSkeletonRefIns v)

gyTxSkeletonRefInsToList :: GYTxSkeletonRefIns v -> [GYTxOutRef]
gyTxSkeletonRefInsToList = Set.toList . gyTxSkeletonRefInsSet

gyTxSkeletonRefInsSet :: GYTxSkeletonRefIns v -> Set GYTxOutRef
gyTxSkeletonRefInsSet (GYTxSkeletonRefIns xs) = xs
gyTxSkeletonRefInsSet GYTxSkeletonNoRefIns    = Set.empty

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
    , gytxWdrls         = []
    , gytxSigs          = Set.empty
    , gytxCerts         = []
    , gytxInvalidBefore = Nothing
    , gytxInvalidAfter  = Nothing
    , gytxMetadata      = Nothing
    }

instance Semigroup (GYTxSkeleton v) where
    x <> y = GYTxSkeleton
        { gytxIns           = combineIns (gytxIns x) (gytxIns y)
        , gytxOuts          = gytxOuts x ++ gytxOuts y
        , gytxRefIns        = gytxRefIns x <> gytxRefIns y
        , gytxMint          = combineMint (gytxMint x) (gytxMint y)
        , gytxWdrls         = combineWdrls (gytxWdrls x) (gytxWdrls y)
        , gytxSigs          = Set.union (gytxSigs x) (gytxSigs y)
        , gytxCerts         = gytxCerts x <> gytxCerts y
        , gytxInvalidBefore = combineInvalidBefore (gytxInvalidBefore x) (gytxInvalidBefore y)
        , gytxInvalidAfter  = combineInvalidAfter (gytxInvalidAfter x) (gytxInvalidAfter y)
        , gytxMetadata      = gytxMetadata x <> gytxMetadata y
        }
      where
        -- we keep only one input per utxo to spend
        combineIns u v = nubBy ((==) `on` gyTxInTxOutRef) (u ++ v)
        -- we cannot combine redeemers, so we just pick first.
        combineMint = Map.unionWith (\(amt, r) (amt', _r) -> (Map.unionWith (+) amt amt', r))
        -- we keep only one withdrawal per stake address
        combineWdrls u v = nubBy ((==) `on` gyTxWdrlStakeAddress) (u ++ v)

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
valueFromPlutusIO :: Plutus.Value -> IO GYValue
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

-- | Convert a 'Plutus.AssetClass' to 'GYAssetClass' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
assetClassFromPlutus' :: MonadError GYTxMonadException m => Plutus.AssetClass -> m GYAssetClass
assetClassFromPlutus' x = either
    (throwError . GYConversionException . GYInvalidPlutusAsset)
    pure
    (assetClassFromPlutus x)

-- | Convert a 'PlutusValue.TokenName' to 'GYTokenName' in 'GYTxMonad'.
--
-- Throw 'GYConversionException' if conversion fails.
tokenNameFromPlutus' :: MonadError GYTxMonadException m => Plutus.TokenName -> m GYTokenName
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

-- | Pure variant of `utxosDatums`.
utxosDatumsPure :: Plutus.FromData a => [(GYUTxO, Maybe GYDatum)] -> Map GYTxOutRef (GYAddress, GYValue, a)
utxosDatumsPure = Map.fromList . mapMaybe utxoDatumPureHushed

-- | Like `utxosDatumsPure` but also returns original raw `GYDatum`.
utxosDatumsPureWithOriginalDatum :: Plutus.FromData a => [(GYUTxO, Maybe GYDatum)] -> Map GYTxOutRef (GYAddress, GYValue, a, GYDatum)
utxosDatumsPureWithOriginalDatum = Map.fromList . mapMaybe utxoDatumPureHushedWithOriginalDatum

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

-- | Obtain original datum representation of an UTxO.
utxoDatumPureHushed :: Plutus.FromData a => (GYUTxO, Maybe GYDatum) -> Maybe (GYTxOutRef, (GYAddress, GYValue, a))
utxoDatumPureHushed (_utxo, Nothing) = Nothing
utxoDatumPureHushed (GYUTxO {..}, Just d) =
  datumToPlutus' d & Plutus.fromBuiltinData <&> \d' -> (utxoRef, (utxoAddress, utxoValue, d'))

-- | Like `utxoDatumPureHushed` but also returns original raw `GYDatum`.
utxoDatumPureHushedWithOriginalDatum :: Plutus.FromData a => (GYUTxO, Maybe GYDatum) -> Maybe (GYTxOutRef, (GYAddress, GYValue, a, GYDatum))
utxoDatumPureHushedWithOriginalDatum (_utxo, Nothing) = Nothing
utxoDatumPureHushedWithOriginalDatum (GYUTxO {..}, Just d) =
  datumToPlutus' d & Plutus.fromBuiltinData <&> \d' -> (utxoRef, (utxoAddress, utxoValue, d', d))

-- | Pure variant of `utxoDatum`.
utxoDatumPure :: Plutus.FromData a => (GYUTxO, Maybe GYDatum) -> Either GYQueryDatumError (GYAddress, GYValue, a)
utxoDatumPure (utxo, Nothing) = Left $ GYNoDatumHash utxo
utxoDatumPure (GYUTxO {..}, Just d) =
  case Plutus.fromBuiltinData $ datumToPlutus' d of
    Nothing -> Left $ GYInvalidDatum d
    Just a  -> Right (utxoAddress, utxoValue, a)

-- | Like `utxoDatumPure` but also returns original raw datum.
utxoDatumPureWithOriginalDatum :: Plutus.FromData a => (GYUTxO, Maybe GYDatum) -> Either GYQueryDatumError (GYAddress, GYValue, a, GYDatum)
utxoDatumPureWithOriginalDatum (utxo, Nothing) = Left $ GYNoDatumHash utxo
utxoDatumPureWithOriginalDatum (GYUTxO {..}, Just d) =
  case Plutus.fromBuiltinData $ datumToPlutus' d of
    Nothing -> Left $ GYInvalidDatum d
    Just a  -> Right (utxoAddress, utxoValue, a, d)

-- | Version of 'utxoDatum' that throws 'GYTxMonadException'.
utxoDatum' :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (GYAddress, GYValue, a)
utxoDatum' utxo = do
    x <- utxoDatum utxo
    liftEither $ first GYQueryDatumException x

-- | Version of 'utxoDatumPure' that throws 'GYTxMonadException'.
utxoDatumPure' :: (MonadError GYTxMonadException m, Plutus.FromData a) => (GYUTxO, Maybe GYDatum) -> m (GYAddress, GYValue, a)
utxoDatumPure' utxoWithDatum = do
    let x = utxoDatumPure utxoWithDatum
    liftEither $ first GYQueryDatumException x

-- | Like `utxoDatumPure'` but also returns original raw datum.
utxoDatumPureWithOriginalDatum' :: (MonadError GYTxMonadException m, Plutus.FromData a) => (GYUTxO, Maybe GYDatum) -> m (GYAddress, GYValue, a, GYDatum)
utxoDatumPureWithOriginalDatum' utxoWithDatum = do
    let x = utxoDatumPureWithOriginalDatum utxoWithDatum
    liftEither $ first GYQueryDatumException x

utxoDatumHushed :: (GYTxQueryMonad m, Plutus.FromData a) => GYUTxO -> m (Maybe (GYAddress, GYValue, a))
utxoDatumHushed = fmap hush . utxoDatum

mustHaveInput :: GYTxIn v -> GYTxSkeleton v
mustHaveInput i = emptyGYTxSkeleton {gytxIns = [i]}

mustHaveRefInput :: VersionIsGreaterOrEqual v 'PlutusV2 => GYTxOutRef -> GYTxSkeleton v
mustHaveRefInput i = emptyGYTxSkeleton { gytxRefIns = GYTxSkeletonRefIns (Set.singleton i) }

mustHaveOutput :: GYTxOut v -> GYTxSkeleton v
mustHaveOutput o = emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveOptionalOutput :: Maybe (GYTxOut v) -> GYTxSkeleton v
mustHaveOptionalOutput = maybe mempty $ \o -> emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveTxMetadata :: Maybe GYTxMetadata -> GYTxSkeleton v
mustHaveTxMetadata m = emptyGYTxSkeleton {gytxMetadata = m}

mustMint :: GYMintScript v -> GYRedeemer -> GYTokenName -> Integer -> GYTxSkeleton v
mustMint _ _ _ 0  = mempty
mustMint p r tn n = emptyGYTxSkeleton {gytxMint = Map.singleton p (Map.singleton tn n, r)}

mustHaveWithdrawal :: GYTxWdrl v -> GYTxSkeleton v
mustHaveWithdrawal w = mempty {gytxWdrls = [w]}

mustHaveCertificate :: GYTxCert v -> GYTxSkeleton v
mustHaveCertificate c = mempty {gytxCerts = [c]}

mustBeSignedBy :: CanSignTx a => a -> GYTxSkeleton v
mustBeSignedBy pkh = emptyGYTxSkeleton {gytxSigs = Set.singleton $ toPubKeyHash pkh}

isInvalidBefore :: GYSlot -> GYTxSkeleton v
isInvalidBefore s = emptyGYTxSkeleton {gytxInvalidBefore = Just s}

isInvalidAfter :: GYSlot -> GYTxSkeleton v
isInvalidAfter s = emptyGYTxSkeleton {gytxInvalidAfter = Just s}

gyLogDebug', gyLogInfo', gyLogWarning', gyLogError' :: (GYTxQueryMonad m, HasCallStack) => GYLogNamespace -> String -> m ()
gyLogDebug'   ns = withFrozenCallStack $ logMsg ns GYDebug
gyLogInfo'    ns = withFrozenCallStack $ logMsg ns GYInfo
gyLogWarning' ns = withFrozenCallStack $ logMsg ns GYWarning
gyLogError'   ns = withFrozenCallStack $ logMsg ns GYError

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

-- | Log the time a particular monad action took.
wrapReqWithTimeLog :: (GYTxQueryMonad m, MonadIO m) => String -> m a -> m a
wrapReqWithTimeLog label m = do
    start <- liftIO getCurrentTime
    a <- m
    end <- liftIO getCurrentTime
    let dur = end `diffUTCTime` start
    logMsg mempty GYDebug $ label <> " took " <> show dur
    pure a

-- | Synonym of 'wrapReqWithTimeLog'.
wt :: (GYTxQueryMonad m, MonadIO m) => String -> m a -> m a
wt = wrapReqWithTimeLog