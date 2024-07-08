{-|
Module      : GeniusYield.Providers.Node.AwaitTx
Description : AwaitTx provider using local node. Inefficient, for testing purposes only.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

**INTERNAL MODULE**
-}

module GeniusYield.Providers.Node.AwaitTx (
    nodeAwaitTxConfirmed
) where

import           Control.Concurrent               (threadDelay)
import           Control.Exception                (throwIO)
import           Control.Monad                    (unless)

import qualified Cardano.Api                      as Api

import           GeniusYield.Providers.Node.Query
import           GeniusYield.Types

{- TODO: Perhaps it's possible to do this more efficiently using one of the node mini protocols.

Idea using chain sync:-

On each RollForward event, store the transaction ids within that block, and the block number
with the transactions. The number of confirmations for each of those transactions is their block
number subtracted from the current block.

Since ChainTip doesn't return current block number, and instead returns the current slot number - maybe
store block's beginning slot number (absolute) instead (available in Rollforward event).
If (chain tip slot - tx block slot) >= 3 * k / active slot coeff, then at least k blocks have been
created since the tx - thus, there have been at least k confirmations.

See: https://docs.cardano.org/about-cardano/learn/chain-confirmation-versus-transaction-confirmation/
-}
nodeAwaitTxConfirmed :: GYEra -> Api.LocalNodeConnectInfo -> GYAwaitTx
nodeAwaitTxConfirmed era info p@GYAwaitTxParameters{..} txId = go 0
  where
    go attempt
      | attempt >= maxAttempts = throwIO $ GYAwaitTxException p
      | otherwise = do
          {- NOTE: Checking for created utxos is not always correct.

          Transactions that create stake deposit with a user who's remaining
          utxos are only enough to cover the transaction cost, create no outputs.
          However, this is an extreme edge case that is unlikely to ever exist in
          privnet tests (where this module is meant to be used, exclusively).
          -}
          utxos <- nodeUtxosFromTx era info txId
          -- FIXME: This doesn't actually wait for confirmations.
          unless (utxosSize utxos /= 0) $
              threadDelay checkInterval >> go (attempt + 1)

-- | Obtain UTxOs created by a transaction.
nodeUtxosFromTx :: GYEra -> Api.LocalNodeConnectInfo -> GYTxId -> IO GYUTxOs
nodeUtxosFromTx era info txId = do
    {- We don't have a way to obtain utxos produced by a TxId. As an alternative, we could
    obtain the whole UTxO set and filter from there, but there's a faster way.

    We can query utxos from `TxOutRef`s. And the `TxOutRef`s of all the utxos produced by a transaction
    will be a product of the TxId (which we know) and the index of each output of the transaction.

    We start by guessing that each transaction produces 10 outputs, and query with all `TxOutRef`s such that
    their TxId part is the transaction id and the index part is `0..10`. Then, we try with `11..20` and so on
    until we get no utxos in return. Then we are done.

    Hacky, but works fine for testing.
    -}
    let startIx = 0
        uptoIx = 10
    go mempty startIx uptoIx
  where
    go acc startIx uptoIx = do
        utxos <- nodeUtxosAtTxOutRefs era info $ curry txOutRefFromTuple txId <$> [startIx .. uptoIx]
        let acc' = acc <> utxos
        if utxosSize utxos == 0
            then pure acc'
            else go acc' (uptoIx + 1) (uptoIx * 2)
