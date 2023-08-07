{-|
Module      : GeniusYield.Scripts.TestToken
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Scripts.TestToken (
    testTokenPolicy,
) where

import           Plutus.V1.Ledger.Api

import           GeniusYield.OnChain.TestToken.Compiled (originalTestTokenPolicy)
import           GeniusYield.Types

testTokenPolicy
    :: Integer           -- ^ count
    -> TokenName         -- ^ token name (e.g. @GOLD@)
    -> GYTxOutRef        -- ^ utxo to base token on
    -> GYMintingPolicy 'PlutusV2
testTokenPolicy count tn utxo =
    mintingPolicyFromPlutus  @'PlutusV2
        $ originalTestTokenPolicy count tn (txOutRefToPlutus utxo)
