{-|
Module      : GeniusYield.TxBuilder.Node
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Node (
    GYTxMonadNode,
    GYTxBuildResult(..),
    runGYTxMonadNode,
    runGYTxMonadNodeC,
    runGYTxMonadNodeF,
    runGYTxMonadNodeParallel,
    runGYTxMonadNodeParallelF,
    runGYTxMonadNodeChaining,
    runGYTxMonadNodeChainingF,
) where

import qualified Cardano.Api                  as Api
import           Control.Monad.IO.Class       (MonadIO (..))
import qualified Data.ByteString              as BS
import qualified Data.List.NonEmpty           as NE
import qualified Data.Set                     as Set

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation run against real node.
newtype GYTxMonadNode a = GYTxMonadNode { unGYTxMonadNode :: GYTxNodeEnv -> IO a }
  deriving stock (Functor)

type role GYTxMonadNode representational

instance Applicative GYTxMonadNode where
    pure x = GYTxMonadNode $ \_ -> return x
    (<*>) = ap

instance Monad GYTxMonadNode where
    m >>= k = GYTxMonadNode $ \env -> do
        x <- unGYTxMonadNode m env
        unGYTxMonadNode (k x) env

instance MonadIO GYTxMonadNode where
    liftIO = GYTxMonadNode . const

data GYTxNodeEnv = GYTxNodeEnv
    { envNid           :: !GYNetworkId
    , envProviders     :: !GYProviders
    , envAddrs         :: ![GYAddress]
    , _envChangeAddr   :: !GYAddress
    , envCollateral    :: !GYTxOutRef
    , envUsedSomeUTxOs :: !(Set GYTxOutRef)
    }

instance MonadError GYTxMonadException GYTxMonadNode where
    throwError = liftIO . throwIO

    catchError action handler = GYTxMonadNode $ \env -> catch
        (unGYTxMonadNode action env)
        (\err -> unGYTxMonadNode (handler err) env)

instance GYTxQueryMonad GYTxMonadNode where
    networkId = GYTxMonadNode $ \env ->
        return $ envNid env

    lookupDatum h = GYTxMonadNode $ \env ->
        gyLookupDatum (envProviders env) h

    utxosAtAddress addr = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddress (envProviders env) addr

    utxosAtAddresses addrs = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddresses (envProviders env) addrs

    utxoRefsAtAddress addr = GYTxMonadNode $ \env ->
        gyQueryUtxoRefsAtAddress (envProviders env) addr

    utxoAtTxOutRef oref = GYTxMonadNode $ \env ->
        gyQueryUtxoAtTxOutRef (envProviders env) oref

    utxosAtTxOutRefs oref = GYTxMonadNode $ \env ->
        gyQueryUtxosAtTxOutRefs (envProviders env) oref

    slotConfig = GYTxMonadNode $ \env ->
        gyGetSlotConfig (envProviders env)

    currentSlot = GYTxMonadNode $ \env ->
        gyGetCurrentSlot (envProviders env)

    logMsg ns s msg = GYTxMonadNode $ \env ->
        gyLog (envProviders env) ns s msg

-- TODO Note: randSeed implementation should use some internal state #30
--            state so randSeed returns different seeds if called multiple times.
--            (https://github.com/geniusyield/atlas/issues/30)
instance GYTxMonad GYTxMonadNode where
    someUTxO = do
        addrs         <- ownAddresses
        collateral    <- getCollateral
        usedSomeUTxOs <- getUsedSomeUTxOs
        utxos         <- traverse utxosAtAddress addrs
        case someTxOutRef $ utxosRemoveTxOutRefs (Set.insert collateral usedSomeUTxOs) (mconcat utxos) of
            Just (oref, _) -> return oref
            Nothing        ->  throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs
      where
        getCollateral    = GYTxMonadNode $ return . envCollateral
        ownAddresses     = GYTxMonadNode $ return . envAddrs
        getUsedSomeUTxOs = GYTxMonadNode $ return . envUsedSomeUTxOs

    -- inject non-determinism from own-address
    -- thus different users will get different random seeds.
    randSeed = foldl' (\ m w -> 256 * m + fromIntegral w) 0
               . concatMap (BS.unpack . Api.serialiseToRawBytes . addressToApi)
               <$> ownAddresses
      where
        ownAddresses = GYTxMonadNode $ return . envAddrs

instance MonadRandom GYTxMonadNode where
  getRandomR  = GYTxMonadNode . const . getRandomR
  getRandom   = GYTxMonadNode $ const getRandom
  getRandomRs = GYTxMonadNode . const . getRandomRs
  getRandoms  = GYTxMonadNode $ const getRandoms

runGYTxMonadNode
    :: GYNetworkId
    -> GYProviders
    -> [GYAddress]                          -- ^ our addresses
    -> GYAddress                            -- ^ change address
    -> GYTxOutRef                           -- ^ collateral
    -> GYTxMonadNode (GYTxSkeleton v)
    -> IO GYTxBody
runGYTxMonadNode = coerce (runGYTxMonadNodeF @Identity GYLegacy)

runGYTxMonadNodeParallel
    :: GYNetworkId
    -> GYProviders
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> GYTxMonadNode [GYTxSkeleton v]
    -> IO (GYTxBuildResult Identity)
runGYTxMonadNodeParallel = coerce (runGYTxMonadNodeParallelF @Identity GYLegacy)

runGYTxMonadNodeChaining
    :: GYNetworkId
    -> GYProviders
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> GYTxMonadNode [GYTxSkeleton v]
    -> IO (GYTxBuildResult Identity)
runGYTxMonadNodeChaining = coerce (runGYTxMonadNodeChainingF @Identity GYLegacy)

runGYTxMonadNodeC
    :: forall a. GYNetworkId
    -> GYProviders
    -> [GYAddress]                          -- ^ our addresses
    -> GYAddress                            -- ^ change address
    -> GYTxOutRef                           -- ^ collateral
    -> GYTxMonadNode a
    -> IO a
runGYTxMonadNodeC = coerce (runGYTxMonadNodeF @(Const a) GYLegacy)

{- | The most basic version of 'GYTxMonadNode' interpreter over a generic 'Traversable'.

== NOTE ==
This is not meant to be used with structures containing _multiple_ 'GYTxSkeleton's. As the balancer
will end up using the same utxos across the different txs.

Consider using 'runGYTxMonadNodeParallel' or 'runGYTxMonadNodeChaining' instead.
-}
runGYTxMonadNodeF
    :: forall f v. Traversable f
    => GYCoinSelectionStrategy
    -> GYNetworkId
    -> GYProviders
    -> [GYAddress]                          -- ^ our addresses
    -> GYAddress                            -- ^ change address
    -> GYTxOutRef                           -- ^ collateral
    -> GYTxMonadNode (f (GYTxSkeleton v))
    -> IO (f GYTxBody)
runGYTxMonadNodeF cstrat nid providers addrs change collateral m = do
    x <- runGYTxMonadNodeCore (const id) cstrat nid providers addrs change collateral $ (:[]) <$> m
    case x of
      GYTxBuildSuccess ne          -> pure $ NE.head ne
      GYTxBuildPartialSuccess gv _ -> throwIO $ InsufficientFundsErr gv
      GYTxBuildFailure gv          -> throwIO $ InsufficientFundsErr gv
      -- We know there is precisely one input.
      GYTxBuildNoInputs            -> error "runGYTxMonadNodeF: absurd"

{- | A multi transaction building 'GYTxMonadNode' interpreter.

This does not perform chaining, i.e does not use utxos created by one of the given transactions in the next one.
However, it does ensure that the balancer does not end up using the same own utxos when building multiple
transactions at once.

This supports failure recovery by utilizing 'GYTxBuildResult'.
-}
runGYTxMonadNodeParallelF
    :: Traversable f
    => GYCoinSelectionStrategy
    -> GYNetworkId
    -> GYProviders
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> GYTxMonadNode [f (GYTxSkeleton v)]
    -> IO (GYTxBuildResult f)
runGYTxMonadNodeParallelF cstrat nid providers addrs change collateral m = do
    runGYTxMonadNodeCore updateOwnUtxosParallel cstrat nid providers addrs change collateral m

{- | A chaining transaction building 'GYTxMonadNode' interpreter.

This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.

This supports failure recovery by utilizing 'GYTxBuildResult'.

**EXPERIMENTAL**
-}
runGYTxMonadNodeChainingF :: Traversable f
    => GYCoinSelectionStrategy
    -> GYNetworkId
    -> GYProviders
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> GYTxMonadNode [f (GYTxSkeleton v)]
    -> IO (GYTxBuildResult f)
runGYTxMonadNodeChainingF cstrat nid providers addrs change collateral m = do
    runGYTxMonadNodeCore (updateOwnUtxosChaining $ Set.fromList addrs) cstrat nid providers addrs change collateral m

{- | The core implementation of 'GYTxMonadNode' interpreter for building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

See 'runGYTxMonadNodeF' for an example which _does not update_ used up own utxos for multi 'GYTxSkeleton' builds.

See 'runGYTxMonadNodeParallel' for an example which  _removes_ used up own utxos for next 'GYTxSkeleton's (if any).

See 'runGYTxMonadNodeChaining' for an example which _removes_ used up own utxos, **and** _adds_ newly created utxos addressed to
own wallet, for next 'GYTxSkeleton's (if any).

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
runGYTxMonadNodeCore
    :: forall f v. Traversable f
    => (GYTxBody -> GYUTxOs -> GYUTxOs)
    -> GYCoinSelectionStrategy
    -> GYNetworkId
    -> GYProviders
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> GYTxMonadNode [f (GYTxSkeleton v)]
    -> IO (GYTxBuildResult f)
runGYTxMonadNodeCore ownUtxoUpdateF cstrat nid providers addrs change collateral action = do

    -- Obtain constant parameters to be used across several 'GYTxBody' generations.
    ss          <- gyGetSystemStart providers
    eh          <- gyGetEraHistory providers
    pp          <- gyGetProtocolParameters providers
    ps          <- gyGetStakePools providers

    e <- unGYTxMonadNode (buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change collateral action) GYTxNodeEnv
            { envNid           = nid
            , envProviders     = providers
            , envAddrs         = addrs
            ,_envChangeAddr    = change
            , envCollateral    = collateral
            , envUsedSomeUTxOs = mempty
            }
    case e of
        Left err  -> throwIO err
        Right res -> return res

-- | Update own utxo set by removing any utxos used up in the given tx.
updateOwnUtxosParallel :: GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosParallel txBody = utxosRemoveTxOutRefs (Set.fromList txIns)
  where
    txIns = txBodyTxIns txBody

{- | Update own utxo set by removing any utxos used up in the given tx,
**and** adding newly created utxos addressed to own wallet. -}
updateOwnUtxosChaining :: Set GYAddress -> GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosChaining ownAddrs txBody utxos = utxosRemoveTxOutRefs (Set.fromList txIns) utxos <> txOutsOwn
  where
    txIns = txBodyTxIns txBody
    txOuts = txBodyUTxOs txBody
    txOutsOwn = filterUTxOs (\GYUTxO {utxoAddress} -> utxoAddress `Set.member` ownAddrs) txOuts
