module GeniusYield.OnChain.Utils (PlutusProgram, mkScript) where

import UntypedPlutusCore
    ( DefaultUni,
      Program,
      DeBruijn,
      DefaultFun,
      termMapNames,
      unNameDeBruijn,
      progTerm )
import PlutusPrelude (over)
import qualified PlutusTx
import PlutusTx (CompiledCode)

type PlutusProgram = Program DeBruijn DefaultUni DefaultFun ()

mkScript :: CompiledCode a -> PlutusProgram
mkScript = toNameless . PlutusTx.getPlc
  where
    toNameless = over progTerm $ termMapNames unNameDeBruijn
