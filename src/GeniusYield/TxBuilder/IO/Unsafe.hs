{-|
Module      : GeniusYield.TxBuilder.IO.Unsafe
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.IO.Unsafe (
  unsafeIOToQueryMonad,
  unsafeIOToTxBuilderMonad,
) where

import           GeniusYield.TxBuilder.IO.Builder (GYTxBuilderMonadIO,
                                                   ioToTxBuilderMonad)
import           GeniusYield.TxBuilder.IO.Query   (GYTxQueryMonadIO,
                                                   ioToQueryMonad)

unsafeIOToQueryMonad :: IO a -> GYTxQueryMonadIO a
unsafeIOToQueryMonad = ioToQueryMonad

unsafeIOToTxBuilderMonad :: IO a -> GYTxBuilderMonadIO a
unsafeIOToTxBuilderMonad = ioToTxBuilderMonad
