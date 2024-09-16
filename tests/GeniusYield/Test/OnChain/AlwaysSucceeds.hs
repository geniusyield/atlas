{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : GeniusYield.Test.OnChain.AlwaysSucceeds
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.OnChain.AlwaysSucceeds (
  mkAlwaysSucceedsValidator,
) where

import PlutusLedgerApi.V2

{-# INLINEABLE mkAlwaysSucceedsValidator #-}
mkAlwaysSucceedsValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkAlwaysSucceedsValidator _ _ _ = ()
