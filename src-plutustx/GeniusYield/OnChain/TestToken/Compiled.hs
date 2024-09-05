{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GeniusYield.OnChain.TestToken.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.TestToken.Compiled (
  originalTestTokenPolicy,
) where

import GeniusYield.OnChain.TestToken
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 qualified
import PlutusTx qualified

originalTestTokenPolicy ::
  -- | Count.
  Integer ->
  -- | Token name (e.g. @GOLD@).
  PlutusLedgerApi.V2.TokenName ->
  -- | UTxO to base token on.
  PlutusLedgerApi.V2.TxOutRef ->
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalTestTokenPolicy count tn utxo =
  $$(PlutusTx.compile [||mkTestTokenPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 count
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 tn
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 utxo
