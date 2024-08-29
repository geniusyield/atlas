{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module GeniusYield.Test.FakeCoin (FakeCoin (..), fakeValue, fakeCoin) where

import qualified Cardano.Api                 as Api
import qualified Cardano.Api.Shelley         as Api.S
import           PlutusCore.Core             (plcVersion100)
import qualified PlutusLedgerApi.V1.Value    as PlutusValue
import           PlutusLedgerApi.V2
import           PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Prelude

-- | Test assets.
newtype FakeCoin = FakeCoin { fakeCoin'tag :: BuiltinByteString }

fakeValue :: FakeCoin -> Integer -> Value
fakeValue tag = PlutusValue.assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> PlutusValue.AssetClass
fakeCoin (FakeCoin tag) = PlutusValue.assetClass sym tok
  where
    sym =
      CurrencySymbol $ toBuiltin $
        Api.serialiseToRawBytes $ Api.hashScript $ Api.PlutusScript Api.PlutusScriptV2
          $ Api.S.PlutusScriptSerialised $ serialiseCompiledCode $ fakeMintingPolicy tok
    tok = TokenName tag

fakeMintingPolicy :: TokenName -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
fakeMintingPolicy mintParam =
  $$(PlutusTx.compile [|| fakeMintingPolicyUntypedContract ||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 mintParam

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: TokenName -> () -> ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  PlutusValue.valueOf (txInfoMint (scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag > 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: TokenName -> BuiltinData -> BuiltinData -> BuiltinUnit
fakeMintingPolicyUntypedContract tag red ctx = check
    (fakeMintingPolicyContract tag (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx))
