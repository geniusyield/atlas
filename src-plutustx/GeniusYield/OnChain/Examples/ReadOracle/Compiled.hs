{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.OnChain.Examples.ReadOracle.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.Examples.ReadOracle.Compiled (
  readOracleValidator,
) where

import PlutusTx qualified

import GeniusYield.OnChain.Examples.ReadOracle

readOracleValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
readOracleValidator = $$(PlutusTx.compile [||mkReadOracleValidator||])
