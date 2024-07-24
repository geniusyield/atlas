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
import           GeniusYield.Examples.Common (toDeBruijn)
import qualified PlutusCore.Version          as PLC
import qualified PlutusLedgerApi.Common      as Plutus
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
limboScript' = toDeBruijn limboScript

limboValidatorV1 :: GYValidator 'PlutusV1
limboValidatorV1 = validatorFromSerialisedScript limboValidatorPlutusSerialised

limboValidatorV2 :: GYValidator 'PlutusV2
limboValidatorV2 = validatorFromSerialisedScript limboValidatorPlutusSerialised

limboValidatorPlutusSerialised :: Plutus.SerialisedScript
limboValidatorPlutusSerialised = Plutus.serialiseUPLC $
    UPLC.Program () PLC.plcVersion100 limboScript'

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
    utxo <- utxosAtAddress addr Nothing
    return $ utxoToRefMap utxo

-- | Create UTxO with a reference script.
--
-- This is optimized version.
-- First it checks whether there is an UTxO already with a script.
-- Only if there aren't the new transaction skeleton is constructed.
--
addRefScript :: GYTxQueryMonad m => GYScript 'PlutusV2 -> m (Either GYTxOutRef (GYTxSkeleton v))
addRefScript sc = do
    addr <- scriptAddress limboValidatorV2
    utxo <- utxosAtAddress addr Nothing

    let refs :: Map (Some GYScript) GYTxOutRef
        refs = utxoToRefMap utxo

    case Map.lookup (Some sc) refs of
        Just ref -> return $ Left ref
        Nothing  -> return $ Right $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) { gyTxOutRefS = Just $ GYPlutusScript sc }

-- | Create UTxO with a reference script.
--
addRefScript' :: GYTxQueryMonad m => GYScript 'PlutusV2 -> m (GYTxSkeleton v)
addRefScript' sc = do
    addr <- scriptAddress limboValidatorV2
    return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) { gyTxOutRefS = Just $ GYPlutusScript sc }

-- | Find reference scripts in transaction body.
findRefScriptsInBody :: GYTxBody -> Map (Some GYScript) GYTxOutRef
findRefScriptsInBody body = do
    let utxo = txBodyUTxOs body
    utxoToRefMap utxo
