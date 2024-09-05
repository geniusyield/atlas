-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module GeniusYield.Test.Unified.OnChain.BetRef.Compiled (
  betRefValidator,
  BetRefParams (..),
  OracleAnswerDatum (..),
  BetRefDatum (..),
  BetRefAction (..),
) where

import PlutusCore.Version (plcVersion100)
import PlutusTx qualified

import GeniusYield.Test.Unified.OnChain.BetRef

-- Since makeLift doesn't seem to work on BetRefParams. We just convert it to data and apply that instead.
betRefValidator :: BetRefParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
betRefValidator betRefParams =
  $$(PlutusTx.compile [||mkBetRefValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (PlutusTx.toBuiltinData betRefParams)
