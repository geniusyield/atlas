{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : GeniusYield.TxBuilder.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Common
    ( GYTxBuildResult (..)
    , pattern InsufficientFundsErr
    , buildTxCore
    ) where

import qualified Cardano.Api                  as Api
import qualified Cardano.Api.Shelley          as Api.S
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

{- | Result of building 'GYTxBody's with the option of recovery from error.

Consider the act of building five 'GYTxSkeleton's into 'GYTxBody's. If three out of the five succeed, but the next
one fails due to insufficient funds - this type facilitates recovering the three rather than failing outright and discarding
the results.
-}
data GYTxBuildResult f
    -- | All given 'GYTxSkeleton's were successfully built.
    = GYTxBuildSuccess !(NonEmpty (f GYTxBody))
    -- | Some of the given 'GYTxSkeleton's were successfully built, but the rest failed due to _insufficient funds_.
    | GYTxBuildPartialSuccess !GYValue !(NonEmpty (f GYTxBody))
    -- | None of the given 'GYTxSkeleton's could be built due to _insufficient funds_.
    | GYTxBuildFailure !GYValue
    -- | Input did not contain any 'GYTxSkeleton's.
    | GYTxBuildNoInputs

{- | The core implementation of 'GYTxQueryMonad' for building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
buildTxCore
    :: forall m f v. (GYTxQueryMonad m, MonadRandom m, Traversable f)
    => Api.SystemStart
    -> Api.EraHistory Api.CardanoMode
    -> Api.S.ProtocolParameters
    -> Set Api.S.PoolId
    -> GYCoinSelectionStrategy
    -> (GYTxBody -> GYUTxOs -> GYUTxOs)
    -> [GYAddress]
    -> GYAddress
    -> GYTxOutRef
    -> m [f (GYTxSkeleton v)]
    -> m (Either BuildTxException (GYTxBuildResult f))
buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change collateral action = do
    fbodies <- action
    collateralUtxo <- utxoAtTxOutRef' collateral
    ownUtxos <- utxosAtAddresses addrs

    let buildEnvWith ownUtxos' = GYBuildTxEnv
            { gyBTxEnvSystemStart    = ss
            , gyBTxEnvEraHistory     = eh
            , gyBTxEnvProtocolParams = pp
            , gyBTxEnvPools          = ps
            , gyBTxEnvOwnUtxos       = utxosRemoveRefScripts $ utxosRemoveTxOutRef collateral ownUtxos'
            , gyBTxEnvChangeAddr     = change
            , gyBTxEnvCollateral     = collateralUtxo
            }

        helper :: GYUTxOs -> GYTxSkeleton v -> m (Either BuildTxException GYTxBody)
        helper ownUtxos' GYTxSkeleton {..} = do
            let gytxMint' :: Maybe (GYValue, [(Some GYMintingPolicy, GYRedeemer)])
                gytxMint' | null gytxMint = Nothing
                    | otherwise = Just
                    ( valueFromList [ (GYToken (mintingPolicyId mp) tn, n) | (Some mp, (tokens, _)) <- itoList gytxMint, (tn, n) <- itoList tokens ]
                    , [(mp, redeemer) | (mp, (_, redeemer)) <- itoList gytxMint]
                    )

            -- Convert the 'GYTxIn's to 'GYTxInDetailed's by fetching chain information about them.
            gyInUtxos       <- utxosAtTxOutRefs $ gyTxInTxOutRef <$> gytxIns
            gyTxInsDetailed <- forM gytxIns $ \gyTxIn -> do
                let ref = gyTxInTxOutRef gyTxIn
                case utxosLookup ref gyInUtxos of
                    Nothing                                                           -> throwError . GYQueryUTxOException $ GYNoUtxoAtRef ref
                    Just GYUTxO {utxoAddress, utxoValue, utxoRefScript, utxoOutDatum} -> pure $
                        GYTxInDetailed gyTxIn utxoAddress utxoValue utxoRefScript (isInlineDatum utxoOutDatum)

            refInsUtxos <- utxosAtTxOutRefs $ gyTxSkeletonRefInsToList gytxRefIns

            -- Build the transaction.
            buildUnsignedTxBody
                (buildEnvWith ownUtxos')
                cstrat
                gyTxInsDetailed
                gytxOuts
                refInsUtxos
                gytxMint'
                gytxInvalidBefore
                gytxInvalidAfter
                gytxSigs

        go :: GYUTxOs -> GYTxBuildResult f -> [f (GYTxSkeleton v)] -> m (Either BuildTxException  (GYTxBuildResult f))
        go _         acc []           = pure $ Right acc
        go ownUtxos' acc (fbody:rest) = do
            res <- sequence <$> traverse (helper ownUtxos') fbody
            case res of
                {- Not enough funds for this transaction
                We assume it's not worth continuing with the next transactions (which is often the case) -}
                Left (InsufficientFundsErr v) -> pure $ Right $ updateBuildRes (Left v) acc
                -- Any other exception is fatal.
                Left err                      -> pure $ Left err
                Right fres                    -> do
                    -- Update the available utxos set by user supplied function.
                    let ownUTxos'' = foldl' (flip ownUtxoUpdateF) ownUtxos' fres
                    -- Continue with an updated accumulator (set of built results).
                    go ownUTxos'' (updateBuildRes (Right fres) acc) rest
    go ownUtxos GYTxBuildNoInputs fbodies
  where
    {- This function updates 'GYTxBuildResult' based on a build outcome

    In case of insufficient funds failure ('Left' argument):
      We return either 'GYTxBuildFailure' or 'GYTxBuildPartialSuccess'
      Depending on whether or not any previous transactions were built succesfully.

    In case of successful build:
      We save the newly built tx body into the existing ones (if any)

    It's impossible for the second argument to ever be 'GYTxBuildFailure' or 'GYTxBuildPartialSuccess', as
    the outer function 'go' (see above) always exits as soon as the accumulator updates to one of these.
    -}
    updateBuildRes (Left v) GYTxBuildNoInputs      = GYTxBuildFailure v
    updateBuildRes (Left v) (GYTxBuildSuccess ne)  = GYTxBuildPartialSuccess v ne
    updateBuildRes (Right x) GYTxBuildNoInputs     = GYTxBuildSuccess (x :| [])
    updateBuildRes (Right x) (GYTxBuildSuccess ne) = GYTxBuildSuccess (NE.cons x ne)
    updateBuildRes _ _                             = error "buildTxCore.flippedFoldM.updateBuildRes: absurd"

pattern InsufficientFundsErr :: GYValue -> BuildTxException
pattern InsufficientFundsErr v = BuildTxBalancingError (BalancingErrorInsufficientFunds v)
