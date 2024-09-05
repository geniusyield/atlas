{- |
Module      : GeniusYield.Examples.Treat
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

This script is similar to "GeniusYield.Examples.Gift",
except it uses @serializeData@ builtin.
Therefore V1 version is invalid.
-}
module GeniusYield.Examples.Treat (
  -- * Scripts
  treatValidatorV1,
  treatValidatorV2,
) where

import GeniusYield.Examples.Common
import GeniusYield.Types

import PlutusCore qualified as PLC
import PlutusCore.Version qualified as PLC
import PlutusLedgerApi.Common qualified as Plutus
import UntypedPlutusCore qualified as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> sc@
treatScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
treatScript =
  UPLC.LamAbs ann datumName $
    UPLC.LamAbs ann redeemerName $
      UPLC.LamAbs ann scName $
        UPLC.Apply
          ann
          (UPLC.Builtin ann PLC.SerialiseData)
          (UPLC.Var ann scName)
  where
    ann = ()

    datumName = UPLC.Name "datum" (UPLC.Unique 0)
    redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
    scName = UPLC.Name "sc" (UPLC.Unique 2)

treatScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
treatScript' = toDeBruijn treatScript

treatValidatorPlutusSerialised :: Plutus.SerialisedScript
treatValidatorPlutusSerialised =
  Plutus.serialiseUPLC $
    UPLC.Program () PLC.plcVersion100 treatScript'

treatValidatorV1 :: GYValidator 'PlutusV1
treatValidatorV1 = validatorFromSerialisedScript treatValidatorPlutusSerialised

treatValidatorV2 :: GYValidator 'PlutusV2
treatValidatorV2 = validatorFromSerialisedScript treatValidatorPlutusSerialised
