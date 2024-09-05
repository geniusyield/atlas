{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : GeniusYield.Test.OnChain.GuessRefInputDatum
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.OnChain.GuessRefInputDatum (
  mkGuessRefInputDatumValidator,
  RefInputDatum (..),
  Guess (..),
) where

import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (findDatum)
import PlutusTx qualified
import PlutusTx.Prelude as PlutusTx

newtype RefInputDatum = RefInputDatum Integer
PlutusTx.unstableMakeIsData ''RefInputDatum

-- Redeemer
newtype Guess = Guess Integer
PlutusTx.unstableMakeIsData ''Guess

{-# INLINEABLE mkGuessRefInputDatumValidator #-}
mkGuessRefInputDatumValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGuessRefInputDatumValidator _ red' ctx'
  | guess == original = ()
  | otherwise = error ()
  where
    ctx :: ScriptContext
    ctx = unsafeFromBuiltinData ctx'

    Guess guess = unsafeFromBuiltinData red'

    info :: TxInfo
    info = scriptContextTxInfo ctx

    refIn :: TxOut
    refIn = case map txInInfoResolved (txInfoReferenceInputs info) of
      [refIn'] -> refIn'
      [] -> traceError "No reference input provided."
      _anyOther -> traceError "Expected only one reference input but found more than one."

    outputToDatum :: (FromData b) => TxOut -> Maybe b
    outputToDatum o = case txOutDatum o of
      NoOutputDatum -> Nothing
      OutputDatum d -> processDatum d
      OutputDatumHash dh -> processDatum =<< findDatum dh info
      where
        processDatum = fromBuiltinData . getDatum

    original :: Integer
    original =
      case outputToDatum refIn of
        Nothing -> traceError "Datum not present or parsed."
        Just (RefInputDatum original') -> original'
