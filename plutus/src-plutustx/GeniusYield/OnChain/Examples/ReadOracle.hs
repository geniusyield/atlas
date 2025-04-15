{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : GeniusYield.OnChain.Examples.ReadOracle
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.Examples.ReadOracle (
  mkReadOracleValidator,
) where

import PlutusLedgerApi.V2
import PlutusTx.Prelude as PlutusTx

{-# INLINEABLE mkReadOracleValidator #-}

-- | Fail if there are no reference inputs with input datums.
mkReadOracleValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkReadOracleValidator _ _ ctx'
  | any (hasOutputDatum . txOutDatum) refins = ()
  | otherwise = error ()
 where
  ctx :: ScriptContext
  ctx = unsafeFromBuiltinData ctx'

  info :: TxInfo
  info = scriptContextTxInfo ctx

  refins :: [TxOut]
  refins = map txInInfoResolved (txInfoReferenceInputs info)

  hasOutputDatum :: OutputDatum -> Bool
  hasOutputDatum (OutputDatum _) = True
  hasOutputDatum _ = False
