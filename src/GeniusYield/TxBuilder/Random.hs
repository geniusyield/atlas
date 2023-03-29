{-|
Module      : GeniusYield.TxBuilder.Random
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.Random
    ( MonadRandom (..)
    , withRandomness
    ) where

import           Control.Monad.Random        (evalRandT, mkStdGen)

import           GeniusYield.TxBuilder.Class

-- | Convert a `GYTxMonad`-computation using randomness into a deterministic one
-- by using one's own public key hash as seed for the random number generator.
--
withRandomness :: GYTxMonad m => (forall n. (GYTxMonad n, MonadRandom n) => n a) -> m a
withRandomness x = do
    seed <- randSeed
    evalRandT x (mkStdGen seed)
