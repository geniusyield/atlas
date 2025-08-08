{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- |
Module      : GeniusYield.Scripts.TestToken
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Scripts.TestToken (
  testTokenPolicy,
) where

import Data.Function ((&))
import GeniusYield.Types
import PlutusLedgerApi.V1 (TokenName (unTokenName), TxId (getTxId))
import PlutusLedgerApi.V1 qualified as Plutus

$(makeBPTypes "data/compiled-scripts/test-token-policy.bp")

$(uponBPTypes "data/compiled-scripts/test-token-policy.bp")

testTokenPolicy ::
  -- | count
  Integer ->
  -- | token name (e.g. @GOLD@)
  GYTokenName ->
  -- | utxo to base token on
  GYTxOutRef ->
  GYScript 'PlutusV2
testTokenPolicy count tn utxo =
  scriptFromBPSerialisedScript $
    applyParamsToBPValidator_Test_token_policy count (tokenNameToPlutus tn & unTokenName) (case txOutRefToPlutus utxo of Plutus.TxOutRef txid ix -> BPTxOutRef (BPTxId $ getTxId txid) ix)
