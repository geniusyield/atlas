{-|
Module      : GeniusYield.Types.TxWdrl
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.TxWdrl (
    GYTxWdrl (..),
    GYTxWdrlWitness (..),
    txWdrlToApi,
) where


import qualified Cardano.Api                as Api
import qualified Cardano.Ledger.Coin        as Ledger
import           GeniusYield.Imports        (Natural)
import           GeniusYield.Types.Address  (GYStakeAddress, stakeAddressToApi)
import           GeniusYield.Types.Era
import           GeniusYield.Types.Redeemer
import           GeniusYield.Types.Script
-- | Transaction withdrawal.
--
-- The parameter @v@ indicates the minimum version of scripts allowed as withdrawals
-- in the transaction.
--
data GYTxWdrl v = GYTxWdrl
    { gyTxWdrlStakeAddress :: !GYStakeAddress
    , gyTxWdrlAmount       :: !Natural
    , gyTxWdrlWitness      :: !(GYTxWdrlWitness v)
    }
  deriving (Eq, Show)

-- | Represents witness type and associated information for tx withdrawals.
data GYTxWdrlWitness v
    -- | Key witness.
    = GYTxWdrlWitnessKey
    -- | Script witness with associated script and redeemer.
    | GYTxWdrlWitnessScript !(GYStakeValScript v) !GYRedeemer
    deriving stock (Eq, Show)

txWdrlToApi
    :: GYTxWdrl v
    -> (Api.StakeAddress, Ledger.Coin, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxStake ApiEra))
txWdrlToApi (GYTxWdrl stakeAddr amt wit) = (stakeAddressToApi stakeAddr, Ledger.Coin (toInteger amt), Api.BuildTxWith $ f wit) where
    f :: GYTxWdrlWitness v -> Api.Witness Api.WitCtxStake ApiEra
    f GYTxWdrlWitnessKey = Api.KeyWitness Api.KeyWitnessForStakeAddr
    f (GYTxWdrlWitnessScript v r) =
        Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
          gyStakeValScriptWitnessToApiPlutusSW
            v
            (redeemerToApi r)
            (Api.ExecutionUnits 0 0)
