{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.Test.OnChain.AlwaysSucceeds.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.OnChain.AlwaysSucceeds.Compiled (alwaysSucceedsValidator) where

import PlutusTx qualified

import GeniusYield.Test.OnChain.AlwaysSucceeds

alwaysSucceedsValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
alwaysSucceedsValidator = $$(PlutusTx.compile [||mkAlwaysSucceedsValidator||])
