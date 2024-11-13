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

import GeniusYield.OnChain.TestToken.Compiled (originalTestTokenPolicy)
import GeniusYield.Types

testTokenPolicy ::
  -- | count
  Integer ->
  -- | token name (e.g. @GOLD@)
  GYTokenName ->
  -- | utxo to base token on
  GYTxOutRef ->
  GYScript 'PlutusV2
testTokenPolicy count tn utxo =
  mintingPolicyFromPlutus @'PlutusV2 $
    originalTestTokenPolicy count (tokenNameToPlutus tn) (txOutRefToPlutus utxo)
