{-# LANGUAGE NoImplicitPrelude #-}
module GeniusYield.OnChain.FakeCoin (fakeMintingPolicyPlutus) where

import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude

fakeMintingPolicyPlutus :: TokenName -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
fakeMintingPolicyPlutus mintParam =
  $$(PlutusTx.compile [||fakeMintingPolicyUntypedContract||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 mintParam

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  PlutusValue.valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag /= 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: TokenName -> BuiltinData -> BuiltinData -> ()
fakeMintingPolicyUntypedContract tag red ctx
  | fakeMintingPolicyContract tag (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx) = ()
  | otherwise = error ()
