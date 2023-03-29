{-|
Module      : GeniusYield.Transaction.CoinSelection.Types
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Transaction.CoinSelection.Types (CoinSelection (..)) where

import GeniusYield.Transaction.Common (GYTxInDetailed)
import GeniusYield.Types.Value        (GYValue)

data CoinSelection v = CoinSelection
    { inputs :: ![GYTxInDetailed v]
    , change :: ![GYValue]
    }

instance Semigroup (CoinSelection v) where
    CoinSelection{inputs=ia, change=ca} <> CoinSelection{inputs=ib, change=cb} =
        CoinSelection { inputs = ia <> ib, change = ca <> cb }

instance Monoid (CoinSelection v) where
    mempty = CoinSelection mempty mempty
