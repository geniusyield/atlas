module GeniusYield.Test.Privnet.Examples.Common (addRefScriptToLimbo) where

import GeniusYield.Examples.Limbo
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

addRefScriptToLimbo :: forall v. v `VersionIsGreaterOrEqual` 'PlutusV2 => GYScript v -> GYTxMonadIO GYTxOutRef
addRefScriptToLimbo sc = scriptAddress limboValidatorV2 >>= flip addRefScript sc
