{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      : GeniusYield.OnChain.TestToken
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.TestToken (
  mkTestTokenPolicy',
  mkTestTokenPolicy,
) where

import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2
import PlutusTx.Prelude

mkTestTokenPolicy' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkTestTokenPolicy' amt tn utxo red ctx = mkTestTokenPolicy (unsafeFromBuiltinData amt) (unsafeFromBuiltinData tn) (unsafeFromBuiltinData utxo) red (unsafeFromBuiltinData ctx)

{-# INLINEABLE mkTestTokenPolicy #-}
mkTestTokenPolicy :: Integer -> TokenName -> TxOutRef -> BuiltinData -> ScriptContext -> ()
mkTestTokenPolicy amt tn utxo _ ctx
  | hasn'tUTxO = traceError "UTxO not consumed"
  | tn /= tn' = traceError "wrong token"
  | amt /= amt' = traceError "wrong amount"
  | cs /= cs' = traceError "wrong currency symbol"
  | otherwise = ()
 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  Minting cs = scriptContextPurpose ctx

  [(cs', tn', amt')] = flattenValue $ txInfoMint info

  hasn'tUTxO :: Bool
  hasn'tUTxO = all (\i -> txInInfoOutRef i /= utxo) $ txInfoInputs info
