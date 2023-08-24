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

import           PlutusLedgerApi.V2
import qualified PlutusTx

import           GeniusYield.OnChain.Examples.ReadOracle
import           GeniusYield.OnChain.Utils

readOracleValidator :: PlutusProgram
readOracleValidator = mkScript
    $$(PlutusTx.compile [|| mkReadOracleValidator ||])
