module GeniusYield.Test.Privnet.Examples.Common (addRefScriptToLimbo) where

import GeniusYield.Examples.Limbo
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

addRefScriptToLimbo :: forall v. GYScript v -> GYTxMonadIO GYTxOutRef
addRefScriptToLimbo sc = scriptAddress limboValidatorV2 >>= flip addRefScript sc
