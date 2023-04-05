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

import qualified Plutus.V1.Ledger.Scripts          as Plutus
import qualified PlutusCore                        as PLC
import qualified UntypedPlutusCore                 as UPLC

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
giftScript' = case UPLC.deBruijnTerm giftScript of
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

giftValidatorPlutus :: Plutus.Validator
giftValidatorPlutus = Plutus.Validator $ Plutus.Script $
    UPLC.Program () (PLC.defaultVersion ()) giftScript'

giftValidatorV1 :: GYValidator 'PlutusV1
giftValidatorV1 = validatorFromPlutus giftValidatorPlutus

giftValidatorV2 :: GYValidator 'PlutusV2
giftValidatorV2 = validatorFromPlutus giftValidatorPlutus
