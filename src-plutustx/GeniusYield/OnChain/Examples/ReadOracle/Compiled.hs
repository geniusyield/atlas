{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.OnChain.Examples.ReadOracle.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OnChain.Examples.ReadOracle.Compiled
    ( readOracleValidator
    ) where

import           Plutus.V2.Ledger.Api
import qualified PlutusTx

import           GeniusYield.OnChain.Examples.ReadOracle

readOracleValidator :: Validator
readOracleValidator = mkValidatorScript
    $$(PlutusTx.compile [|| mkReadOracleValidator ||])
