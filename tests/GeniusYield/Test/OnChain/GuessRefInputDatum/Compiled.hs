{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled (
  guessRefInputDatumValidator,
  RefInputDatum (..),
  Guess (..),
) where

import PlutusTx qualified

import GeniusYield.Test.OnChain.GuessRefInputDatum

guessRefInputDatumValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
guessRefInputDatumValidator = $$(PlutusTx.compile [||mkGuessRefInputDatumValidator||])
