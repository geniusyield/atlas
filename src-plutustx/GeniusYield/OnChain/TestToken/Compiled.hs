{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.OnChain.TestToken.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OnChain.TestToken.Compiled (
    originalTestTokenPolicy,
) where

import           GeniusYield.OnChain.TestToken
import qualified PlutusLedgerApi.V2
import qualified PlutusTx
import PlutusCore.Version (plcVersion100)

originalTestTokenPolicy
  :: Integer                       -- ^ Count.
  -> PlutusLedgerApi.V2.TokenName  -- ^ Token name (e.g. @GOLD@).
  -> PlutusLedgerApi.V2.TxOutRef   -- ^ UTxO to base token on.
  -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalTestTokenPolicy count tn utxo =
    $$(PlutusTx.compile [|| mkTestTokenPolicy ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 count
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 utxo
