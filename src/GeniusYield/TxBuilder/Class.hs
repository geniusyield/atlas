{- |
Module      : GeniusYield.TxBuilder.Class
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.TxBuilder.Class (
  MonadError (..),
  MonadRandom (..),
  GYTxGameMonad (..),
  GYTxMonad (..),
  signTxBodyImpl,
  signTxBodyWithStakeImpl,
  GYTxBuilderMonad (..),
  GYTxQueryMonad (..),
  GYTxSpecialQueryMonad (..),
  GYTxUserQueryMonad (..),
  GYTxSkeleton (..),
  GYTxSkeletonRefIns (..),
  buildTxBody,
  buildTxBodyParallel,
  buildTxBodyChaining,
  waitNSlots,
  waitNSlots_,
  waitUntilSlot_,
  submitTx_,
  submitTxConfirmed,
  submitTxConfirmed_,
  submitTxConfirmed',
  submitTxConfirmed'_,
  submitTxBody,
  submitTxBody_,
  submitTxBodyConfirmed,
  submitTxBodyConfirmed_,
  signAndSubmitConfirmed,
  signAndSubmitConfirmed_,
  awaitTxConfirmed,
  gyTxSkeletonRefInsToList,
  gyTxSkeletonRefInsSet,
  lookupDatum',
  utxoAtTxOutRef',
  utxoAtTxOutRefWithDatum',
  someUTxOWithoutRefScript,
  slotToBeginTime,
  slotToEndTime,
  enclosingSlotFromTime,
  enclosingSlotFromTime',
  slotToEpoch,
  epochToBeginSlot,
  scriptAddress,
  scriptAddress',
  addressFromText',
  addressFromPlutusM,
  addressFromPlutusHushedM,
  addressFromPlutus',
  addressToPubKeyHash',
  addressToPubKeyHashIO,
  addressToValidatorHash',
  addressToValidatorHashIO,
  valueFromPlutus',
  valueFromPlutusIO,
  makeAssetClass',
  makeAssetClassIO,
  assetClassFromPlutus',
  tokenNameFromPlutus',
  txOutRefFromPlutus',
  datumHashFromPlutus',
  pubKeyHashFromPlutus',
  advanceSlot',
  utxosDatums,
  utxosDatumsPure,
  utxosDatumsPureWithOriginalDatum,
  utxoDatum,
  utxoDatumPure,
  utxoDatumPureWithOriginalDatum,
  utxoDatumHushed,
  utxoDatumPureHushed,
  utxoDatumPureHushedWithOriginalDatum,
  utxoDatum',
  utxoDatumPure',
  utxoDatumPureWithOriginalDatum',
  mustHaveInput,
  mustHaveRefInput,
  mustHaveOutput,
  mustHaveOptionalOutput,
  mustHaveTxMetadata,
  mustMint,
  mustHaveWithdrawal,
  mustHaveCertificate,
  mustBeSignedBy,
  isInvalidBefore,
  isInvalidAfter,
  gyLogDebug',
  gyLogInfo',
  gyLogWarning',
  gyLogError',
  skeletonToRefScriptsORefs,
  wrapReqWithTimeLog,
  wt,
) where

import Control.Monad.Except (MonadError (..), liftEither)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Random (
  MonadRandom (..),
  RandT,
  lift,
 )
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Writer.CPS qualified as CPS
import Control.Monad.Writer.Lazy qualified as Lazy
import Control.Monad.Writer.Strict qualified as Strict
import Data.Default (Default, def)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as Txt
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import GHC.Stack (withFrozenCallStack)
import GeniusYield.Imports
import GeniusYield.Transaction
import GeniusYield.TxBuilder.Common
import GeniusYield.TxBuilder.Errors
import GeniusYield.TxBuilder.Query.Class
import GeniusYield.TxBuilder.User
import GeniusYield.Types
import GeniusYield.Types.Key.Class (ToShelleyWitnessSigningKey)
import GeniusYield.Types.TxCert.Internal (GYTxCert (..))
import PlutusLedgerApi.V1 qualified as Plutus (
  Address,
  DatumHash,
  FromData (..),
  PubKeyHash,
  TokenName,
  TxOutRef,
  Value,
 )
import PlutusLedgerApi.V1.Value qualified as Plutus (AssetClass)

-- NOTE: The 'Default (TxBuilderStrategy m)' constraint is not necessary, but it is usually desired everytime
-- someone is building transactions with the below machinery.

{- | Class of monads for building transactions. This can be default derived if the requirements are met.
Specifically, set 'TxBuilderStrategy' to 'GYCoinSelectionStrategy' if you wish to use the default in-house
transaction building implementation.
-}
class (Default (TxBuilderStrategy m), GYTxSpecialQueryMonad m, GYTxUserQueryMonad m) => GYTxBuilderMonad m where
  type TxBuilderStrategy m :: Type
  type TxBuilderStrategy m = GYCoinSelectionStrategy

  -- | The most basic version of 'GYTxSkeleton' builder.
  --
  --     == NOTE ==
  --     This is not meant to be called multiple times with several 'GYTxSkeleton's before submission.
  --     Because the balancer will end up using the same utxos across the different txs.
  --
  --     Consider using 'buildTxBodyParallel' or 'buildTxBodyChaining' instead.
  buildTxBodyWithStrategy :: forall v. TxBuilderStrategy m -> GYTxSkeleton v -> m GYTxBody
  default buildTxBodyWithStrategy ::
    forall v.
    (MonadRandom m, TxBuilderStrategy m ~ GYCoinSelectionStrategy) =>
    TxBuilderStrategy m ->
    GYTxSkeleton v ->
    m GYTxBody
  buildTxBodyWithStrategy = buildTxBodyWithStrategy'

  -- | A multi 'GYTxSkeleton' builder. The result containing built bodies must be in the same order as the skeletons.
  --
  --     This does not perform chaining, i.e does not use utxos created by one of the given transactions in the next one.
  --     However, it does ensure that the balancer does not end up using the same own utxos when building multiple
  --     transactions at once.
  --
  --     This supports failure recovery by utilizing 'GYTxBuildResult'.
  buildTxBodyParallelWithStrategy :: forall v. TxBuilderStrategy m -> [GYTxSkeleton v] -> m GYTxBuildResult
  default buildTxBodyParallelWithStrategy ::
    forall v.
    (MonadRandom m, TxBuilderStrategy m ~ GYCoinSelectionStrategy) =>
    TxBuilderStrategy m ->
    [GYTxSkeleton v] ->
    m GYTxBuildResult
  buildTxBodyParallelWithStrategy = buildTxBodyParallelWithStrategy'

  -- | A chaining 'GYTxSkeleton' builder. The result containing built bodies must be in the same order as the skeletons.
  --
  --     This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.
  --
  --     This supports failure recovery by utilizing 'GYTxBuildResult'.
  buildTxBodyChainingWithStrategy :: forall v. TxBuilderStrategy m -> [GYTxSkeleton v] -> m GYTxBuildResult
  default buildTxBodyChainingWithStrategy ::
    forall v.
    (MonadRandom m, TxBuilderStrategy m ~ GYCoinSelectionStrategy) =>
    TxBuilderStrategy m ->
    [GYTxSkeleton v] ->
    m GYTxBuildResult
  buildTxBodyChainingWithStrategy = buildTxBodyChainingWithStrategy'

-- | 'buildTxBodyWithStrategy' with the default coin selection strategy.
buildTxBody :: forall v m. GYTxBuilderMonad m => GYTxSkeleton v -> m GYTxBody
buildTxBody = buildTxBodyWithStrategy def

-- | 'buildTxBodyParallelWithStrategy' with the default coin selection strategy.
buildTxBodyParallel :: forall v m. GYTxBuilderMonad m => [GYTxSkeleton v] -> m GYTxBuildResult
buildTxBodyParallel = buildTxBodyParallelWithStrategy def

-- | 'buildTxBodyChainingWithStrategy' with the default coin selection strategy.
buildTxBodyChaining :: forall v m. GYTxBuilderMonad m => [GYTxSkeleton v] -> m GYTxBuildResult
buildTxBodyChaining = buildTxBodyChainingWithStrategy def

-- | Class of monads for interacting with the blockchain using transactions.
class GYTxBuilderMonad m => GYTxMonad m where
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

signTxBodyImpl :: GYTxMonad m => m GYPaymentSigningKey -> GYTxBody -> m GYTx
signTxBodyImpl kM txBody = signGYTxBody txBody . (: []) <$> kM

signTxBodyWithStakeImpl :: GYTxMonad m => m (GYPaymentSigningKey, Maybe GYStakeSigningKey) -> GYTxBody -> m GYTx
signTxBodyWithStakeImpl kM txBody = (\(pKey, sKey) -> signGYTxBody txBody $ GYSomeSigningKey pKey : maybeToList (GYSomeSigningKey <$> sKey)) <$> kM

-- | Class of monads that can simulate a "game" between different users interacting with transactions.
class (GYTxMonad (TxMonadOf m), GYTxSpecialQueryMonad m) => GYTxGameMonad m where
  -- | Type of the supported 'GYTxMonad' instance that can participate within the "game".
  type TxMonadOf m = (r :: Type -> Type) | r -> m

  -- TODO: Maybe introduce a user generation config type that this function can take?

  -- | Create a new user within the chain. This does not fund the user. See "GeniusYield.Test.Utils.createUserWithLovelace"
  --       or "GeniusYield.Test.Utils.createUserWithAssets".
  --
  --       This _must not_ fund the user.
  --       Note: The generated user may be arbitrarily complex. i.e may have zero or more stake keys (and thus one or more addresses).
  createUser :: m User

  -- | Lift the supported 'GYTxMonad' instance into the game, as a participating user wallet.
  asUser :: User -> TxMonadOf m a -> m a

{- Note [Higher order effects, TxMonadOf, and GYTxGameMonad]

'GYTxGameMonad' is designed to give the implementor two choices: either make it a different data type
from its associated 'GYTxMonad' instance (such is the case for 'GYTxGameMonadIO' and 'GYTxMonadIO'), or
make the same data type a 'GYTxMonad' and 'GYTxGameMonad'.

The former would not be possible if 'GYTxGameMonad' was subsumed into 'GYTxMonad', or if the 'TxMonadOf' type family
was not present. Thus, both the separation and the type family are the result of a conscious design decision.

It's important to allow the former case since it avoids making 'asUser' a higher order effect, unconditionally. Higher
order effects can be problematic. If, in the future, we are to use a proper effect system - we'd like to avoid having to
deal with higher order effects wherever feasible.

As to why the type family is injective, the goal is to have a unique 'GYTxMonad' instance for each 'GYTxGameMonad'. This
makes type checking easier regardless of the implementation choice above. Thus, one can have a block of 'GYTxGameMonad' code
with a bunch of 'asUser' calls sprinkled in with blocks of 'GYTxMonad' code, and no extraneous type signatures would be necessary.
Just one type inference (or signature) on the top most call that runs the 'GYTxGameMonad' code block, and all the 'asUser' code blocks
will be automatically inferred.
-}

-- | > waitUntilSlot_ = void . waitUntilSlot
waitUntilSlot_ :: GYTxQueryMonad m => GYSlot -> m ()
waitUntilSlot_ = void . waitUntilSlot

-- | Wait until the chain tip has progressed by N slots.
waitNSlots :: GYTxQueryMonad m => Word64 -> m GYSlot
waitNSlots (slotFromWord64 -> n) = do
  -- FIXME: Does this need to be an absolute slot getter instead?
  currentSlot <- slotOfCurrentBlock
  waitUntilSlot . slotFromApi $ currentSlot `addSlots` n
 where
  addSlots = (+) `on` slotToApi

-- | > waitNSlots_ = void . waitNSlots
waitNSlots_ :: GYTxQueryMonad m => Word64 -> m ()
waitNSlots_ = void . waitNSlots

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
submitTxBody_ :: forall a m. (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m ()
submitTxBody_ txBody = void . submitTxBody txBody

{- | Signs a 'GYTxBody' with the given keys and submits the transaction.
Equivalent to a call to 'signGYTxBody', followed by a call to 'submitTx'
-}
submitTxBody :: forall a m. (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m GYTxId
submitTxBody txBody = submitTx . signGYTxBody txBody

-- | > submitTxBodyConfirmed_ t = void . submitTxBodyConfirmed t
submitTxBodyConfirmed_ :: (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m ()
submitTxBodyConfirmed_ txBody = void . submitTxBodyConfirmed txBody

{- | Signs a 'GYTxBody' with the given keys, submits the transaction, and waits for its confirmation.
Equivalent to a call to 'signGYTxBody', followed by a call to 'submitTxConfirmed'.
-}
submitTxBodyConfirmed :: forall a m. (GYTxMonad m, ToShelleyWitnessSigningKey a) => GYTxBody -> [a] -> m GYTxId
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
Such as 'data ReaderTTxMonad m a = ReaderTTxMonad ((TxMonadOf m) a)' or similar. See
"GeniusYield.Test.FeeTracker.FeeTrackerGame" for a tutorial on how to do this.

Since these wrapper data types are usage specific, and 'GYTxGameMonad' instances are meant to be some
"overarching base" type, we do not provide these instances and users may define them if necessary.
-}

instance GYTxBuilderMonad m => GYTxBuilderMonad (RandT g m) where
  type TxBuilderStrategy (RandT g m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

instance GYTxMonad m => GYTxMonad (RandT g m) where
  signTxBody = lift . signTxBody
  signTxBodyWithStake = lift . signTxBodyWithStake
  submitTx = lift . submitTx
  awaitTxConfirmed' p = lift . awaitTxConfirmed' p

instance GYTxBuilderMonad m => GYTxBuilderMonad (ReaderT env m) where
  type TxBuilderStrategy (ReaderT env m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

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

instance GYTxBuilderMonad m => GYTxBuilderMonad (Strict.StateT s m) where
  type TxBuilderStrategy (Strict.StateT s m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

instance GYTxMonad m => GYTxMonad (Strict.StateT s m) where
  signTxBody = lift . signTxBody
  signTxBodyWithStake = lift . signTxBodyWithStake
  submitTx = lift . submitTx
  awaitTxConfirmed' p = lift . awaitTxConfirmed' p

instance GYTxBuilderMonad m => GYTxBuilderMonad (Lazy.StateT s m) where
  type TxBuilderStrategy (Lazy.StateT s m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

instance GYTxMonad m => GYTxMonad (Lazy.StateT s m) where
  signTxBody = lift . signTxBody
  signTxBodyWithStake = lift . signTxBodyWithStake
  submitTx = lift . submitTx
  awaitTxConfirmed' p = lift . awaitTxConfirmed' p

instance (GYTxBuilderMonad m, Monoid w) => GYTxBuilderMonad (CPS.WriterT w m) where
  type TxBuilderStrategy (CPS.WriterT w m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

instance (GYTxMonad m, Monoid w) => GYTxMonad (CPS.WriterT w m) where
  signTxBody = lift . signTxBody
  signTxBodyWithStake = lift . signTxBodyWithStake
  submitTx = lift . submitTx
  awaitTxConfirmed' p = lift . awaitTxConfirmed' p

instance (GYTxBuilderMonad m, Monoid w) => GYTxBuilderMonad (Strict.WriterT w m) where
  type TxBuilderStrategy (Strict.WriterT w m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

instance (GYTxMonad m, Monoid w) => GYTxMonad (Strict.WriterT w m) where
  signTxBody = lift . signTxBody
  signTxBodyWithStake = lift . signTxBodyWithStake
  submitTx = lift . submitTx
  awaitTxConfirmed' p = lift . awaitTxConfirmed' p

instance (GYTxBuilderMonad m, Monoid w) => GYTxBuilderMonad (Lazy.WriterT w m) where
  type TxBuilderStrategy (Lazy.WriterT w m) = TxBuilderStrategy m
  buildTxBodyWithStrategy x = lift . buildTxBodyWithStrategy x
  buildTxBodyParallelWithStrategy x = lift . buildTxBodyParallelWithStrategy x
  buildTxBodyChainingWithStrategy x = lift . buildTxBodyChainingWithStrategy x

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
utxoAtTxOutRef' ref =
  utxoAtTxOutRef ref
    >>= maybe
      (throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref)
      pure

-- | A version of 'utxoAtTxOutRefWithDatum' that raises 'GYNoUtxoAtRef' if the utxo is not found.
utxoAtTxOutRefWithDatum' :: GYTxQueryMonad m => GYTxOutRef -> m (GYUTxO, Maybe GYDatum)
utxoAtTxOutRefWithDatum' ref =
  utxoAtTxOutRefWithDatum ref
    >>= maybe
      (throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref)
      pure

-- | Returns some UTxO present in wallet which doesn't have reference script.
someUTxOWithoutRefScript :: GYTxUserQueryMonad m => m GYTxOutRef
someUTxOWithoutRefScript = do
  utxosToConsider <- utxosRemoveRefScripts <$> availableUTxOs
  addrs <- ownAddresses
  case someTxOutRef utxosToConsider of
    Just (oref, _) -> return oref
    Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs -- TODO: Possible to put better error message here?

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

-- | Get epoch number in which the given slot belongs to.
slotToEpoch :: GYTxQueryMonad m => GYSlot -> m GYEpochNo
slotToEpoch s = flip slotToEpochPure s <$> slotConfig

-- | Get the first slot in the given epoch.
epochToBeginSlot :: GYTxQueryMonad m => GYEpochNo -> m GYSlot
epochToBeginSlot e = flip epochToBeginSlotPure e <$> slotConfig

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Calculate script's address.
scriptAddress :: GYTxQueryMonad m => GYScript v -> m GYAddress
scriptAddress v = do
  nid <- networkId
  return $ addressFromValidator nid v

-- | Calculate script's address.
scriptAddress' :: GYTxQueryMonad m => GYScriptHash -> m GYAddress
scriptAddress' h = do
  nid <- networkId
  return $ addressFromValidatorHash nid h

{- | Convert a 'Plutus.Address' to 'GYAddress' in 'GYTxMonad'.

Explicitly returns an error rather than throwing it.
-}
addressFromPlutusM :: GYTxQueryMonad m => Plutus.Address -> m (Either PlutusToCardanoError GYAddress)
addressFromPlutusM addr = flip addressFromPlutus addr <$> networkId

-- | 'hush'ed version of 'addressFromPlutusM'.
addressFromPlutusHushedM :: GYTxQueryMonad m => Plutus.Address -> m (Maybe GYAddress)
addressFromPlutusHushedM addr = fmap hush $ flip addressFromPlutus addr <$> networkId

{- | Convert a 'Plutus.Address' to 'GYAddress' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
addressFromPlutus' :: GYTxQueryMonad m => Plutus.Address -> m GYAddress
addressFromPlutus' addr = do
  x <- addressFromPlutusM addr
  liftEither $ first (GYConversionException . GYLedgerToCardanoError) x

{- | Convert 'GYAddress' to 'GYPubKeyHash' in 'GYTxMonad'.

Throw 'GYConversionException' if address is not key-hash one.
-}
addressToPubKeyHash' :: MonadError GYTxMonadException m => GYAddress -> m GYPubKeyHash
addressToPubKeyHash' addr =
  maybe
    (throwError . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToPubKeyHash addr)

addressToPubKeyHashIO :: GYAddress -> IO GYPubKeyHash
addressToPubKeyHashIO addr =
  maybe
    (throwIO . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToPubKeyHash addr)

{- | Convert 'GYAddress' to 'GYScriptHash' in 'GYTxMonad'.

Throw 'GYConversionException' if address is not script-hash one.
-}
addressToValidatorHash' :: MonadError GYTxMonadException m => GYAddress -> m GYScriptHash
addressToValidatorHash' addr =
  maybe
    (throwError . GYConversionException $ GYNotPubKeyAddress addr)
    pure
    (addressToValidatorHash addr)

addressToValidatorHashIO :: GYAddress -> IO GYScriptHash
addressToValidatorHashIO addr =
  maybe
    (throwIO . GYConversionException $ GYNotScriptAddress addr)
    pure
    (addressToValidatorHash addr)

{- | Convert a 'Plutus.Value' to 'GYValue' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
valueFromPlutus' :: MonadError GYTxMonadException m => Plutus.Value -> m GYValue
valueFromPlutus' val =
  either
    (throwError . GYConversionException . flip GYInvalidPlutusValue val)
    pure
    (valueFromPlutus val)

{- | Convert a 'Plutus.Value' to 'GYValue' in 'IO'.

Throw 'GYConversionException' if conversion fails.
-}
valueFromPlutusIO :: Plutus.Value -> IO GYValue
valueFromPlutusIO val =
  either
    (throwIO . GYConversionException . flip GYInvalidPlutusValue val)
    pure
    (valueFromPlutus val)

{- | Create a 'GYAssetClass' from the textual representation of currency symbol and token name in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
makeAssetClass' :: MonadError GYTxMonadException m => Text -> Text -> m GYAssetClass
makeAssetClass' a b =
  either
    (throwError . GYConversionException . GYInvalidAssetClass . Txt.pack)
    pure
    (makeAssetClass a b)

{- | 'makeAssetClass'' in the IO monad.

Throw 'GYConversionException' if conversion fails.
-}
makeAssetClassIO :: Text -> Text -> IO GYAssetClass
makeAssetClassIO a b =
  either
    (throwIO . GYConversionException . GYInvalidAssetClass . Txt.pack)
    pure
    (makeAssetClass a b)

{- | Convert a 'Plutus.AssetClass' to 'GYAssetClass' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
assetClassFromPlutus' :: MonadError GYTxMonadException m => Plutus.AssetClass -> m GYAssetClass
assetClassFromPlutus' x =
  either
    (throwError . GYConversionException . GYInvalidPlutusAsset)
    pure
    (assetClassFromPlutus x)

{- | Convert a 'PlutusValue.TokenName' to 'GYTokenName' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
tokenNameFromPlutus' :: MonadError GYTxMonadException m => Plutus.TokenName -> m GYTokenName
tokenNameFromPlutus' x =
  maybe
    (throwError . GYConversionException . GYInvalidPlutusAsset $ GYTokenNameTooBig x)
    pure
    (tokenNameFromPlutus x)

{- | Convert a 'Plutus.TxOutRef' to 'GYTxOutRef' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
txOutRefFromPlutus' :: MonadError GYTxMonadException m => Plutus.TxOutRef -> m GYTxOutRef
txOutRefFromPlutus' ref =
  either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (txOutRefFromPlutus ref)

{- | Convert a 'Plutus.DatumHash' to 'GYDatumHash' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
datumHashFromPlutus' :: MonadError GYTxMonadException m => Plutus.DatumHash -> m GYDatumHash
datumHashFromPlutus' dh =
  either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (datumHashFromPlutus dh)

{- | Convert a 'Plutus.PubKeyHash' to 'GYPubKeyHash' in 'GYTxMonad'.

Throw 'GYConversionException' if conversion fails.
-}
pubKeyHashFromPlutus' :: MonadError GYTxMonadException m => Plutus.PubKeyHash -> m GYPubKeyHash
pubKeyHashFromPlutus' pkh =
  either
    (throwError . GYConversionException . GYLedgerToCardanoError)
    pure
    (pubKeyHashFromPlutus pkh)

{- | Parse the bech32 representation of an address into 'GYAddress' in 'GYTxMonad'.

Throw 'GYConversionException' if parsing fails.
-}
addressFromText' :: MonadError GYTxMonadException m => Text -> m GYAddress
addressFromText' addr =
  maybe
    (throwError . GYConversionException $ GYInvalidAddressText addr)
    pure
    (addressFromTextMaybe addr)

-- | Advance 'GYSlot' forward in 'GYTxMonad'. If slot value overflows, throw 'GYSlotOverflowException'.
advanceSlot' :: MonadError GYTxMonadException m => GYSlot -> Natural -> m GYSlot
advanceSlot' s t =
  maybe
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
  GYOutDatumHash h -> do
    md <- lookupDatum h
    case md of
      Nothing -> pure . Left $ GYNoDatumForHash h
      Just d -> datumToRes d
  GYOutDatumInline d -> datumToRes d
 where
  datumToRes x = case Plutus.fromBuiltinData $ datumToPlutus' x of
    Nothing -> pure . Left $ GYInvalidDatum x
    Just a -> pure $ Right (utxoAddress utxo, utxoValue utxo, a)

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
    Just a -> Right (utxoAddress, utxoValue, a)

-- | Like `utxoDatumPure` but also returns original raw datum.
utxoDatumPureWithOriginalDatum :: Plutus.FromData a => (GYUTxO, Maybe GYDatum) -> Either GYQueryDatumError (GYAddress, GYValue, a, GYDatum)
utxoDatumPureWithOriginalDatum (utxo, Nothing) = Left $ GYNoDatumHash utxo
utxoDatumPureWithOriginalDatum (GYUTxO {..}, Just d) =
  case Plutus.fromBuiltinData $ datumToPlutus' d of
    Nothing -> Left $ GYInvalidDatum d
    Just a -> Right (utxoAddress, utxoValue, a, d)

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
mustHaveRefInput i = emptyGYTxSkeleton {gytxRefIns = GYTxSkeletonRefIns (Set.singleton i)}

mustHaveOutput :: GYTxOut v -> GYTxSkeleton v
mustHaveOutput o = emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveOptionalOutput :: Maybe (GYTxOut v) -> GYTxSkeleton v
mustHaveOptionalOutput = maybe mempty $ \o -> emptyGYTxSkeleton {gytxOuts = [o]}

mustHaveTxMetadata :: Maybe GYTxMetadata -> GYTxSkeleton v
mustHaveTxMetadata m = emptyGYTxSkeleton {gytxMetadata = m}

mustMint :: GYMintScript v -> GYRedeemer -> GYTokenName -> Integer -> GYTxSkeleton v
mustMint _ _ _ 0 = mempty
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
gyLogDebug' ns = withFrozenCallStack $ logMsg ns GYDebug
gyLogInfo' ns = withFrozenCallStack $ logMsg ns GYInfo
gyLogWarning' ns = withFrozenCallStack $ logMsg ns GYWarning
gyLogError' ns = withFrozenCallStack $ logMsg ns GYError

-- | Given a skeleton, returns a list of reference to reference script UTxOs which are present as witness.
skeletonToRefScriptsORefs :: GYTxSkeleton v -> [GYTxOutRef]
skeletonToRefScriptsORefs GYTxSkeleton {gytxIns} = go gytxIns []
 where
  go :: [GYTxIn v] -> [GYTxOutRef] -> [GYTxOutRef]
  go [] acc = acc
  go (gytxIn : rest) acc = case gyTxInWitness gytxIn of
    GYTxInWitnessScript gyInScript _ _ -> case gyInScript of
      GYInReference oRef _ -> go rest (oRef : acc)
      _anyOtherMatch -> go rest acc
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

{- | The most basic version of 'GYTxSkeleton' builder.

== NOTE ==
This is not meant to be called multiple times with several 'GYTxSkeleton's. As the balancer
will end up using the same utxos across the different txs.

Consider using 'buildTxBodyParallel' or 'buildTxBodyChaining' instead.
-}
buildTxBodyWithStrategy' ::
  forall v m.
  (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m) =>
  GYCoinSelectionStrategy ->
  GYTxSkeleton v ->
  m GYTxBody
buildTxBodyWithStrategy' cstrat m = do
  x <- buildTxBodyCore (const id) cstrat [m]
  case x of
    GYTxBuildSuccess ne -> pure $ NE.head ne
    GYTxBuildPartialSuccess be _ -> throwError . GYBuildTxException $ GYBuildTxBalancingError be
    GYTxBuildFailure be -> throwError . GYBuildTxException $ GYBuildTxBalancingError be
    -- We know there is precisely one input.
    GYTxBuildNoInputs -> error "buildTxBodyWithStrategy': absurd"

{- | A multi 'GYTxSkeleton' builder.

This does not perform chaining, i.e does not use utxos created by one of the given transactions in the next one.
However, it does ensure that the balancer does not end up using the same own utxos when building multiple
transactions at once.

This supports failure recovery by utilizing 'GYTxBuildResult'.
-}
buildTxBodyParallelWithStrategy' ::
  (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m) =>
  GYCoinSelectionStrategy ->
  [GYTxSkeleton v] ->
  m GYTxBuildResult
buildTxBodyParallelWithStrategy' cstrat m = do
  buildTxBodyCore updateOwnUtxosParallel cstrat m

{- | A chaining 'GYTxSkeleton' builder.

This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.

This supports failure recovery by utilizing 'GYTxBuildResult'.

**EXPERIMENTAL**
-}
buildTxBodyChainingWithStrategy' ::
  (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m) =>
  GYCoinSelectionStrategy ->
  [GYTxSkeleton v] ->
  m GYTxBuildResult
buildTxBodyChainingWithStrategy' cstrat m = do
  addrs <- ownAddresses
  buildTxBodyCore (updateOwnUtxosChaining $ Set.fromList addrs) cstrat m

{- | The core implementation of buildTxBody: Building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

See 'buildTxBodyF' for an example which _does not update_ used up own utxos for multi 'GYTxSkeleton' builds.

See 'buildTxBodyParallel' for an example which  _removes_ used up own utxos for next 'GYTxSkeleton's (if any).

See 'buildTxBodyChaining' for an example which _removes_ used up own utxos, **and** _adds_ newly created utxos addressed to
own wallet, for next 'GYTxSkeleton's (if any).

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
buildTxBodyCore ::
  forall v m.
  (GYTxSpecialQueryMonad m, GYTxUserQueryMonad m, MonadRandom m) =>
  -- | Function governing how to update UTxO set when building for multiple skeletons.
  (GYTxBody -> GYUTxOs -> GYUTxOs) ->
  -- | Coin selection strategy.
  GYCoinSelectionStrategy ->
  -- | Skeleton(s).
  [GYTxSkeleton v] ->
  m GYTxBuildResult
buildTxBodyCore ownUtxoUpdateF cstrat skeletons = do
  logSkeletons skeletons

  -- Obtain constant parameters to be used across several 'GYTxBody' generations.
  ss <- systemStart
  eh <- eraHistory
  pp <- protocolParams
  let isRegPool = any (any (\(GYTxCert pb _) -> case pb of GYStakePoolRegistrationCertificatePB _ -> True; _anyOther -> False) . gytxCerts) skeletons
  ps <- if isRegPool then stakePools else pure mempty -- TODO: For ONLY unregistration case, we can just put those stake pool ids.
  collateral <- ownCollateral
  addrs <- ownAddresses
  change <- ownChangeAddress

  e <- buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change collateral skeletons
  case e of
    Left err -> throwError $ GYBuildTxException err
    Right res -> pure res
 where
  logSkeletons :: [GYTxSkeleton v] -> m ()
  logSkeletons = mapM_ (logMsg "buildTxBody" GYDebug . show)

-- | Update own utxo set by removing any utxos used up in the given tx.
updateOwnUtxosParallel :: GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosParallel txBody = utxosRemoveTxOutRefs (Set.fromList txIns)
 where
  txIns = txBodyTxIns txBody

{- | Update own utxo set by removing any utxos used up in the given tx,
**and** adding newly created utxos addressed to own wallet.
-}
updateOwnUtxosChaining :: Set GYAddress -> GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosChaining ownAddrs txBody utxos = utxosRemoveTxOutRefs (Set.fromList txIns) utxos <> txOutsOwn
 where
  txIns = txBodyTxIns txBody
  txOuts = txBodyUTxOs txBody
  txOutsOwn = filterUTxOs (\GYUTxO {utxoAddress} -> utxoAddress `Set.member` ownAddrs) txOuts
