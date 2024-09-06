{- |
Module      : GeniusYield.Examples.Limbo
Description : Another simple script: never succeeds.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Examples.Limbo (
  limboValidatorV1,
  limboValidatorV2,
) where

import GeniusYield.Types

import GeniusYield.Examples.Common (toDeBruijn)
import PlutusCore.Version qualified as PLC
import PlutusLedgerApi.Common qualified as Plutus
import UntypedPlutusCore qualified as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> ERROR@
limboScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
limboScript =
  UPLC.LamAbs ann datumName $
    UPLC.LamAbs ann redeemerName $
      UPLC.LamAbs ann scName $
        UPLC.Error ann
 where
  ann = ()

  datumName = UPLC.Name "datum" (UPLC.Unique 0)
  redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
  scName = UPLC.Name "sc" (UPLC.Unique 2)

limboScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
limboScript' = toDeBruijn limboScript

limboValidatorV1 :: GYValidator 'PlutusV1
limboValidatorV1 = validatorFromSerialisedScript limboValidatorPlutusSerialised

limboValidatorV2 :: GYValidator 'PlutusV2
limboValidatorV2 = validatorFromSerialisedScript limboValidatorPlutusSerialised

limboValidatorPlutusSerialised :: Plutus.SerialisedScript
limboValidatorPlutusSerialised =
  Plutus.serialiseUPLC $
    UPLC.Program () PLC.plcVersion100 limboScript'
