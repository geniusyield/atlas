{- |
Module      : GeniusYield.Types.TxWdrl
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxWdrl (
  GYTxWdrl (..),
  GYTxWdrlWitness,
  pattern GYTxWdrlWitnessKey,
  pattern GYTxWdrlWitnessScript,
  txWdrlToApi,
) where

import Cardano.Api qualified as Api
import Cardano.Ledger.Coin qualified as Ledger
import GeniusYield.Imports (Natural)
import GeniusYield.Types.Address (GYStakeAddress, stakeAddressToApi)
import GeniusYield.Types.BuildWitness
import GeniusYield.Types.Era
import GeniusYield.Types.Redeemer
import GeniusYield.Types.TxIn (GYInScript)

{- | Transaction withdrawal.

The parameter @v@ indicates the minimum version of scripts allowed as withdrawals
in the transaction.
-}
data GYTxWdrl v = GYTxWdrl
  { gyTxWdrlStakeAddress :: !GYStakeAddress
  , gyTxWdrlAmount :: !Natural
  , gyTxWdrlWitness :: !(GYTxBuildWitness v)
  }
  deriving (Eq, Show)

type GYTxWdrlWitness v = GYTxBuildWitness v

pattern GYTxWdrlWitnessKey :: GYTxWdrlWitness v
pattern GYTxWdrlWitnessKey = GYTxBuildWitnessKey

pattern GYTxWdrlWitnessScript :: GYInScript v -> GYRedeemer -> GYTxWdrlWitness v
pattern GYTxWdrlWitnessScript v r = GYTxBuildWitnessPlutusScript v r

txWdrlToApi ::
  GYTxWdrl v ->
  (Api.StakeAddress, Ledger.Coin, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxStake ApiEra))
txWdrlToApi (GYTxWdrl stakeAddr amt wit) = (stakeAddressToApi stakeAddr, Ledger.Coin (toInteger amt), Api.BuildTxWith $ buildWitnessToApi wit)
