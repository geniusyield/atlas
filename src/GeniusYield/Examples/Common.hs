{-|
Module      : GeniusYield.Examples.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}

module GeniusYield.Examples.Common (
  toDeBruijn,
) where

import qualified PlutusCore.DeBruijn as PLC
import qualified UntypedPlutusCore   as UPLC

toDeBruijn :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
toDeBruijn script = case UPLC.deBruijnTerm script of
    Left exc    -> error $ "Converting to deBruijn " ++ show (exc :: UPLC.FreeVariableError)
    Right term' -> UPLC.termMapNames PLC.unNameDeBruijn term'
