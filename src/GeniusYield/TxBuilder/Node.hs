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
    runGYTxMonadNodeParallelWithStrategy,
    runGYTxMonadNodeParallelF,
    runGYTxMonadNodeChaining,
    runGYTxMonadNodeChainingF,
) where

import qualified Cardano.Api                     as Api
import           Control.Monad.IO.Class          (MonadIO (..))
import qualified Data.ByteString                 as BS
import qualified Data.List.NonEmpty              as NE
import qualified Data.Set                        as Set

import           Control.Monad.Trans.Maybe       (MaybeT (runMaybeT))
import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.TxBuilder.NodeQuery
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
    , envCollateral    :: !(Maybe GYUTxO)
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

    utxosAtAddress addr mAssetClass = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddress (envProviders env) addr mAssetClass

    utxosAtAddresses addrs = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddresses (envProviders env) addrs

    utxosAtAddressWithDatums addr mAssetClass = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddressWithDatums (envProviders env) addr mAssetClass

    utxosAtPaymentCredential cred = GYTxMonadNode $ \env ->
        gyQueryUtxosAtPaymentCredential (envProviders env) cred

    utxosAtAddressesWithDatums addrs = GYTxMonadNode $ \env ->
        gyQueryUtxosAtAddressesWithDatums (envProviders env) addrs

    utxosAtPaymentCredentialWithDatums cred = GYTxMonadNode $ \env ->
        gyQueryUtxosAtPaymentCredWithDatums (envProviders env) cred

    utxoRefsAtAddress addr = GYTxMonadNode $ \env ->
        gyQueryUtxoRefsAtAddress (envProviders env) addr

    utxoAtTxOutRef oref = GYTxMonadNode $ \env ->
        gyQueryUtxoAtTxOutRef (envProviders env) oref

    utxosAtTxOutRefs oref = GYTxMonadNode $ \env ->
        gyQueryUtxosAtTxOutRefs (envProviders env) oref

    utxosAtTxOutRefsWithDatums refs = GYTxMonadNode $ \env ->
        gyQueryUtxosAtTxOutRefsWithDatums (envProviders env) refs

    slotConfig = GYTxMonadNode $ \env ->
        gyGetSlotConfig (envProviders env)

    slotOfCurrentBlock = GYTxMonadNode $ \env ->
        gyGetSlotOfCurrentBlock (envProviders env)

    logMsg ns s msg = GYTxMonadNode $ \env ->
        gyLog (envProviders env) ns s msg

-- TODO Note: randSeed implementation should use some internal state #30
--            state so randSeed returns different seeds if called multiple times.
--            (https://github.com/geniusyield/atlas/issues/30)
instance GYTxMonad GYTxMonadNode where

    ownAddresses = GYTxMonadNode $ return . envAddrs

    availableUTxOs = do
        addrs         <- ownAddresses
        mCollateral   <- getCollateral
        usedSomeUTxOs <- getUsedSomeUTxOs
        utxos         <- utxosAtAddresses addrs
        return $ utxosRemoveTxOutRefs (maybe usedSomeUTxOs ((`Set.insert` usedSomeUTxOs) . utxoRef) mCollateral) utxos
      where
        getCollateral    = GYTxMonadNode $ return . envCollateral
        getUsedSomeUTxOs = GYTxMonadNode $ return . envUsedSomeUTxOs

    someUTxO lang = do
        addrs           <- ownAddresses
        utxosToConsider <- availableUTxOs
        case lang of
          PlutusV2 ->
            case someTxOutRef utxosToConsider  of
                Just (oref, _) -> return oref
                Nothing        -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs
          PlutusV1 ->
            case find utxoTranslatableToV1 $ utxosToList utxosToConsider of
              Just u  -> return $ utxoRef u
              Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Better error message here?

    -- inject non-determinism from own-address
    -- thus different users will get different random seeds.
    randSeed = foldl' (\ m w -> 256 * m + fromIntegral w) 0
               . concatMap (BS.unpack . Api.serialiseToRawBytes . addressToApi)
               <$> ownAddresses

instance MonadRandom GYTxMonadNode where
  getRandomR  = GYTxMonadNode . const . getRandomR
  getRandom   = GYTxMonadNode $ const getRandom
  getRandomRs = GYTxMonadNode . const . getRandomRs
  getRandoms  = GYTxMonadNode $ const getRandoms

runGYTxMonadNode
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode (GYTxSkeleton v)  -- ^ Skeleton.
    -> IO GYTxBody
runGYTxMonadNode = coerce (runGYTxMonadNodeF @Identity GYRandomImproveMultiAsset)

runGYTxMonadNodeParallel
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [GYTxSkeleton v]  -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadNodeParallel = coerce (runGYTxMonadNodeParallelF @Identity GYRandomImproveMultiAsset)

runGYTxMonadNodeParallelWithStrategy
    :: GYCoinSelectionStrategy         -- ^ Coin selection strategy.
    -> GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [GYTxSkeleton v]  -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadNodeParallelWithStrategy strat = coerce (runGYTxMonadNodeParallelF @Identity strat)

runGYTxMonadNodeChaining
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [GYTxSkeleton v]  -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadNodeChaining = coerce (runGYTxMonadNodeChainingF @Identity GYRandomImproveMultiAsset)

runGYTxMonadNodeC
    :: forall a.
       GYNetworkId               -- ^ Network ID.
    -> GYProviders               -- ^ Provider.
    -> [GYAddress]               -- ^ Addresses belonging to wallet.
    -> GYAddress                 -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)  -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode a           -- ^ When we don't want to build a skeleton.
    -> IO a
runGYTxMonadNodeC = coerce (runGYTxMonadNodeF @(Const a) GYRandomImproveMultiAsset)

{- | The most basic version of 'GYTxMonadNode' interpreter over a generic 'Traversable'.

== NOTE ==
This is not meant to be used with structures containing _multiple_ 'GYTxSkeleton's. As the balancer
will end up using the same utxos across the different txs.

Consider using 'runGYTxMonadNodeParallel' or 'runGYTxMonadNodeChaining' instead.
-}
runGYTxMonadNodeF
    :: forall f v. Traversable f
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode (f (GYTxSkeleton v))  -- ^ Skeleton(s).
    -> IO (f GYTxBody)
runGYTxMonadNodeF cstrat nid providers addrs change collateral m = do
    x <- runGYTxMonadNodeCore (const id) cstrat nid providers addrs change collateral $ (:[]) <$> m
    case x of
      GYTxBuildSuccess ne          -> pure $ NE.head ne
      GYTxBuildPartialSuccess be _ -> throwIO $ BuildTxBalancingError be
      GYTxBuildFailure be          -> throwIO $ BuildTxBalancingError be
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
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [f (GYTxSkeleton v)]  -- ^ Skeleton(s).
    -> IO (GYTxBuildResult f)
runGYTxMonadNodeParallelF cstrat nid providers addrs change collateral m = do
    runGYTxMonadNodeCore updateOwnUtxosParallel cstrat nid providers addrs change collateral m

{- | A chaining transaction building 'GYTxMonadNode' interpreter.

This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.

This supports failure recovery by utilizing 'GYTxBuildResult'.

**EXPERIMENTAL**
-}
runGYTxMonadNodeChainingF :: Traversable f
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [f (GYTxSkeleton v)]  -- ^ Skeleton(s).
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
    => (GYTxBody -> GYUTxOs -> GYUTxOs)    -- ^ Function governing how to update UTxO set when building for multiple skeletons.
    -> GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadNode [f (GYTxSkeleton v)]  -- ^ Skeleton(s).
    -> IO (GYTxBuildResult f)
runGYTxMonadNodeCore ownUtxoUpdateF cstrat nid providers addrs change collateral action = do

    -- Obtain constant parameters to be used across several 'GYTxBody' generations.
    ss          <- gyGetSystemStart providers
    eh          <- gyGetEraHistory providers
    pp          <- gyGetProtocolParameters providers
    ps          <- gyGetStakePools providers

    bpp <- case Api.bundleProtocolParams Api.BabbageEra pp of
                Left e     -> throwIO $ BuildTxPPConversionError e
                Right bpp' -> pure bpp'

    collateral' <- obtainCollateral

    e <- unGYTxMonadNode (buildTxCore ss eh bpp ps cstrat ownUtxoUpdateF addrs change collateral' loggedAction) GYTxNodeEnv
            { envNid           = nid
            , envProviders     = providers
            , envAddrs         = addrs
            ,_envChangeAddr    = change
            , envCollateral    = collateral'
            , envUsedSomeUTxOs = mempty
            }
    case e of
        Left err  -> throwIO err
        Right res -> return res

    where
      obtainCollateral :: IO (Maybe GYUTxO)
      obtainCollateral = runMaybeT $ do
        (collateralRef, toCheck) <- hoistMaybe collateral
        collateralUtxo <- liftIO $ runGYTxQueryMonadNode nid providers $ utxoAtTxOutRef' collateralRef
        if not toCheck || (utxoValue collateralUtxo == collateralValue) then return collateralUtxo
        else hoistMaybe Nothing

      loggedAction :: GYTxMonadNode [f (GYTxSkeleton v)]
      loggedAction = action >>= \skeletons -> logSkeletons skeletons
                     >> return skeletons

      logSkeletons :: [f (GYTxSkeleton v)] -> GYTxMonadNode ()
      logSkeletons = mapM_ (mapM_ (logMsg "runGYTxMonadNodeCore" GYDebug . show))

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
