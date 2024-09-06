{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-strictness -fno-spec-constr -fno-specialise #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module GeniusYield.Test.FakeCoin (FakeCoin (..), fakeValue, fakeCoin, fakePolicy) where

import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)
import PlutusTx qualified
import PlutusTx.Prelude

import GeniusYield.Types

-- | Test assets.
newtype FakeCoin = FakeCoin {fakeCoinName :: GYTokenName}

fakePolicy :: FakeCoin -> GYMintingPolicy PlutusV2
fakePolicy = fakeMintingPolicy . fakeCoinName

fakeValue :: FakeCoin -> Integer -> GYValue
fakeValue tag = valueSingleton (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> GYAssetClass
fakeCoin (FakeCoin tag) = mintingPolicyId (fakeMintingPolicy tag) `GYToken` tag

fakeMintingPolicy :: GYTokenName -> GYMintingPolicy PlutusV2
fakeMintingPolicy = mintingPolicyFromPlutus . fakeMintingPolicyPlutus . tokenNameToPlutus

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
