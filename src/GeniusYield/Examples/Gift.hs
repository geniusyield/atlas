{-|
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
) where

import           GeniusYield.Types

import           GeniusYield.Examples.Common (toDeBruijn)
import qualified PlutusCore                  as PLC
import qualified PlutusLedgerApi.Common      as Plutus
import qualified UntypedPlutusCore           as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> sc@
giftScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
giftScript
    = UPLC.LamAbs ann datumName
    $ UPLC.LamAbs ann redeemerName
    $ UPLC.LamAbs ann scName
    $ UPLC.Var ann scName
  where
    ann = ()

    datumName    = UPLC.Name "datum" (UPLC.Unique 0)
    redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
    scName       = UPLC.Name "sc" (UPLC.Unique 2)

giftScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
giftScript' = toDeBruijn giftScript

giftValidatorV1 :: GYValidator 'PlutusV1
giftValidatorV1 = validatorFromSerialisedScript giftValidatorPlutusSerialised

giftValidatorV2 :: GYValidator 'PlutusV2
giftValidatorV2 = validatorFromSerialisedScript giftValidatorPlutusSerialised

giftValidatorPlutusSerialised :: Plutus.SerialisedScript
giftValidatorPlutusSerialised = Plutus.serialiseUPLC $
    UPLC.Program () PLC.latestVersion giftScript'
