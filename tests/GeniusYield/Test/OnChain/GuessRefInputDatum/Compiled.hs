{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop

-}
module GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
    ( guessRefInputDatumValidator
    , RefInputDatum (..)
    , Guess (..)
    ) where

import           PlutusLedgerApi.V2
import qualified PlutusTx

import           GeniusYield.Test.OnChain.GuessRefInputDatum

guessRefInputDatumValidator :: Validator
guessRefInputDatumValidator = mkValidatorScript
    $$(PlutusTx.compile [|| mkGuessRefInputDatumValidator ||])
