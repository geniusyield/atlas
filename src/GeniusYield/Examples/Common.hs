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

import qualified UntypedPlutusCore as UPLC

toDeBruijn :: UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
toDeBruijn script = case UPLC.deBruijnTerm script of
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
    go (UPLC.Constr ann i ts ) = UPLC.Constr ann i (map go ts)
    go (UPLC.Case ann t ts   ) = UPLC.Case ann (go t) (map go ts)
