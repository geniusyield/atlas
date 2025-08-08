{-# LANGUAGE NoImplicitPrelude #-}

module GeniusYield.OnChain.FakeCoin (fakeMintingPolicyUntypedContract) where

import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusTx.Prelude

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  PlutusValue.valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag /= 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: BuiltinData -> BuiltinData -> BuiltinData -> ()
fakeMintingPolicyUntypedContract tag red ctx
  | fakeMintingPolicyContract (unsafeFromBuiltinData tag) (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx) = ()
  | otherwise = error ()
