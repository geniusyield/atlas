{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : GeniusYield.OnChain.AStakeValidator
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.OnChain.AStakeValidator
    ( mkAStakeValidator
    ) where

import           PlutusLedgerApi.V2
import           PlutusTx.Prelude   as PlutusTx

{-# INLINABLE mkAStakeValidator #-}
mkAStakeValidator :: Address -> BuiltinData -> BuiltinData -> ()
mkAStakeValidator addr _ ctx' = case scriptContextPurpose ctx of
  Certifying _ -> ()
  Rewarding _  -> if paidToAddress then () else error ()
  _            -> error ()
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    paidToAddress :: Bool
    paidToAddress = any (\o -> txOutAddress o == addr) $ txInfoOutputs info
