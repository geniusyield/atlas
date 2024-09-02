-- TODO (simplify-genesis): Remove this module once user creation has been removed from test setup.
-- See note: 'simplify-genesis'.
{-|
Module      : GeniusYield.Api.TestTokens
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Api.TestTokens (
    mintTestTokens,
) where

import           GeniusYield.Scripts.TestToken
import           GeniusYield.TxBuilder
import           GeniusYield.Types

mintTestTokens :: GYTxUserQueryMonad m
               => GYTokenName
               -> Natural
               -> m (GYAssetClass, GYTxSkeleton 'PlutusV2)
mintTestTokens tn amt = do
    -- utxo to base token of.
    utxo <- someUTxO PlutusV1

    let amt'   = toInteger (max 1 amt) -- mint at least 1 token.
        policy = testTokenPolicy amt' tn utxo

    let txSkeleton = mustHaveInput (GYTxIn utxo GYTxInWitnessKey)
                  <> mustMint (GYMintScript policy) unitRedeemer tn amt'

    return (GYToken (mintingPolicyId policy) tn, txSkeleton)
