{- |
Module      : GeniusYield.OnChain.AStakeValidator.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.AStakeValidator.Compiled (
  originalAStakeValidator,
) where

import GeniusYield.OnChain.AStakeValidator
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 qualified
import PlutusTx qualified

originalAStakeValidator ::
  PlutusLedgerApi.V2.Address ->
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalAStakeValidator addr =
  $$(PlutusTx.compile [||mkAStakeValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 addr
