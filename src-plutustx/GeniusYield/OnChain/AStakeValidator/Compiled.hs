{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.OnChain.AStakeValidator.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OnChain.AStakeValidator.Compiled (
    originalAStakeValidator,
) where

import           GeniusYield.OnChain.AStakeValidator
import           PlutusCore.Version                  (plcVersion100)
import qualified PlutusLedgerApi.V2
import qualified PlutusTx

originalAStakeValidator
  :: PlutusLedgerApi.V2.Address
  -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalAStakeValidator addr =
    $$(PlutusTx.compile [|| mkAStakeValidator ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 addr
