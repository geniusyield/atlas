module GeniusYield.Test.Privnet.Examples.Common (addRefScriptToLimbo) where

import GeniusYield.Examples.Limbo
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

addRefScriptToLimbo :: GYScript PlutusV2 -> GYTxMonadIO GYTxOutRef
addRefScriptToLimbo sc = scriptAddress limboValidatorV2 >>= flip addRefScript sc
