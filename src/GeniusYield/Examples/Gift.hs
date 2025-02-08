{- |
Module      : GeniusYield.Examples.Gift
Description : The simplest script imaginable, which does nothing: always succeeds.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Examples.Gift (
  -- * Scripts
  giftValidatorV1,
  giftValidatorV2,
  giftValidatorV3,
) where

import GeniusYield.Types

import GeniusYield.Examples.Common (toDeBruijn)
import PlutusCore.MkPlc qualified as UPLC
import PlutusCore.Version qualified as PLC
import PlutusLedgerApi.Common qualified as Plutus
import UntypedPlutusCore qualified as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> sc@
giftScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
giftScript =
  UPLC.LamAbs ann datumName $
    UPLC.LamAbs ann redeemerName $
      UPLC.LamAbs ann scName $
        UPLC.Var ann scName
 where
  ann = ()

  datumName = UPLC.Name "datum" (UPLC.Unique 0)
  redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
  scName = UPLC.Name "sc" (UPLC.Unique 2)

-- | A very simple script: @\sc -> ()@
giftScriptV3 :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
giftScriptV3 =
  UPLC.LamAbs ann scName $
    UPLC.mkConstant ann ()
 where
  ann = ()

  scName = UPLC.Name "sc" (UPLC.Unique 0)

giftScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
giftScript' = toDeBruijn giftScript

giftScriptV3' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
giftScriptV3' = toDeBruijn giftScriptV3

giftValidatorV1 :: GYScript 'PlutusV1
giftValidatorV1 = validatorFromSerialisedScript giftValidatorPlutusSerialised

giftValidatorV2 :: GYScript 'PlutusV2
giftValidatorV2 = validatorFromSerialisedScript giftValidatorPlutusSerialised

giftValidatorV3 :: GYScript 'PlutusV3
giftValidatorV3 = validatorFromSerialisedScript giftValidatorPlutusV3Serialised

giftValidatorPlutusSerialised :: Plutus.SerialisedScript
giftValidatorPlutusSerialised =
  Plutus.serialiseUPLC $
    UPLC.Program () PLC.plcVersion100 giftScript'

giftValidatorPlutusV3Serialised :: Plutus.SerialisedScript
giftValidatorPlutusV3Serialised =
  Plutus.serialiseUPLC $
    UPLC.Program () PLC.plcVersion110 giftScriptV3'
