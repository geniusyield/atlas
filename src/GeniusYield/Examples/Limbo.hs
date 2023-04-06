{-|
Module      : GeniusYield.Examples.Limbo
Description : Another simple script: never succeeds.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Examples.Limbo (
    -- * Scripts
    limboValidatorV1,
    limboValidatorV2,
    -- * API
    getRefInfos,
    addRefScript,
    addRefScript',
    findRefScriptsInBody,
) where

import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types

import qualified Data.Map.Strict             as Map
import qualified Plutus.V1.Ledger.Scripts    as Plutus
import qualified PlutusCore                  as PLC
import qualified UntypedPlutusCore           as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> ERROR@
limboScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
limboScript
    = UPLC.LamAbs ann datumName
    $ UPLC.LamAbs ann redeemerName
    $ UPLC.LamAbs ann scName
    $ UPLC.Error ann
  where
    ann = ()

    datumName    = UPLC.Name "datum" (UPLC.Unique 0)
    redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
    scName       = UPLC.Name "sc" (UPLC.Unique 2)

limboScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
limboScript' = case UPLC.deBruijnTerm limboScript of
    Left exc    -> error $ "Converting to deBruijn " ++ show (exc :: UPLC.FreeVariableError)
    Right term' -> renameUPLC (\(UPLC.NamedDeBruijn _ i) -> UPLC.DeBruijn i) term'

renameUPLC :: (name -> name') -> UPLC.Term name uni fun ann -> UPLC.Term name' uni fun ann
renameUPLC rnm = go where
    go (UPLC.Var ann n       ) = UPLC.Var ann (rnm n)
    go (UPLC.LamAbs ann n t  ) = UPLC.LamAbs ann (rnm n) (go t)
    go (UPLC.Apply ann t1 t2 ) = UPLC.Apply ann (go t1) (go t2)
    go (UPLC.Delay ann t     ) = UPLC.Delay ann (go t)
    go (UPLC.Force ann t     ) = UPLC.Force ann (go t)
    go (UPLC.Constant ann con) = UPLC.Constant ann con
    go (UPLC.Builtin ann bn  ) = UPLC.Builtin ann bn
    go (UPLC.Error ann       ) = UPLC.Error ann

limboValidatorPlutus :: Plutus.Validator
limboValidatorPlutus = Plutus.Validator $ Plutus.Script $
    UPLC.Program () (PLC.defaultVersion ()) limboScript'

limboValidatorV1 :: GYValidator 'PlutusV1
limboValidatorV1 = validatorFromPlutus limboValidatorPlutus

limboValidatorV2 :: GYValidator 'PlutusV2
limboValidatorV2 = validatorFromPlutus limboValidatorPlutus

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

utxoToRefMap :: GYUTxOs ->  Map (Some GYScript) GYTxOutRef
utxoToRefMap utxo = Map.fromList
    [ (sc, ref)
    | GYUTxO { utxoRef = ref, utxoRefScript = Just sc} <- utxosToList utxo
    ]

-- | Find reference scripts at 'limboValidatorV2' address.
--
getRefInfos :: GYTxQueryMonad m => m (Map (Some GYScript) GYTxOutRef)
getRefInfos = do
    addr <- scriptAddress limboValidatorV2
    utxo <- utxosAtAddress addr
    return $ utxoToRefMap utxo

-- | Create UTxO with a reference script.
--
-- This is optimized version.
-- First it checks whether there is an UTxO already with a script.
-- Only if there aren't the new transaction skeleton is constructed.
--
addRefScript :: GYTxMonad m => GYScript 'PlutusV2 -> m (Either GYTxOutRef (GYTxSkeleton v))
addRefScript sc = do
    addr <- scriptAddress limboValidatorV2
    utxo <- utxosAtAddress addr

    let refs :: Map (Some GYScript) GYTxOutRef
        refs = utxoToRefMap utxo

    case Map.lookup (Some sc) refs of
        Just ref -> return $ Left ref
        Nothing  -> return $ Right $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) { gyTxOutRefS = Just sc }

-- | Create UTxO with a reference script.
--
addRefScript' :: GYTxMonad m => GYScript 'PlutusV2 -> m (GYTxSkeleton v)
addRefScript' sc = do
    addr <- scriptAddress limboValidatorV2
    return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) { gyTxOutRefS = Just sc }

-- | Find reference scripts in transaction body.
findRefScriptsInBody :: GYTxBody -> Map (Some GYScript) GYTxOutRef
findRefScriptsInBody body = do
    let utxo = txBodyUTxOs body
    utxoToRefMap utxo
