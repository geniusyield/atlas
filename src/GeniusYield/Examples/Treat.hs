{-|
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

import           GeniusYield.Types

import qualified Plutus.V1.Ledger.Scripts          as Plutus
import qualified PlutusCore                        as PLC
import qualified UntypedPlutusCore                 as UPLC

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | A very simple script: @\\datum redeemer sc -> sc@
treatScript :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ()
treatScript
    = UPLC.LamAbs ann datumName
    $ UPLC.LamAbs ann redeemerName
    $ UPLC.LamAbs ann scName
    $ UPLC.Apply ann
      (UPLC.Builtin ann PLC.SerialiseData)
      (UPLC.Var ann scName)
  where
    ann = ()

    datumName    = UPLC.Name "datum" (UPLC.Unique 0)
    redeemerName = UPLC.Name "redeemer" (UPLC.Unique 1)
    scName       = UPLC.Name "sc" (UPLC.Unique 2)

treatScript' :: UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
treatScript' = case UPLC.deBruijnTerm treatScript of
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

treatValidatorPlutus :: Plutus.Validator
treatValidatorPlutus = Plutus.Validator $ Plutus.Script $
    UPLC.Program () (PLC.defaultVersion ()) treatScript'

treatValidatorV1 :: GYValidator PlutusV1
treatValidatorV1 = validatorFromPlutus treatValidatorPlutus

treatValidatorV2 :: GYValidator PlutusV2
treatValidatorV2 = validatorFromPlutus treatValidatorPlutus
