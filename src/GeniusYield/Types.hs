{- |
Module      : GeniusYield.Types
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types (
  Natural,
  module X,

  -- * Shelley params

  -- | Protocol parameters introduced in Shelley era

  -- ** @MinFeeA@

  -- | Min fee factor
  ppMinFeeAL,

  -- ** @MinFeeB@

  -- | Min fee constant
  ppMinFeeBL,

  -- ** @MaxBBSize@

  -- | Max block body size
  ppMaxBBSizeL,

  -- ** @AaxBHSize@

  -- | Max block header size
  ppMaxBHSizeL,

  -- ** @PoolDeposit@

  -- | Stake pool deposit
  ppPoolDepositL,

  -- ** @EMax@

  -- | Epoch bound on pool retirement
  ppEMaxL,

  -- ** @NOpt@

  -- | Desired number of pools
  ppNOptL,

  -- ** @A0@

  -- | Pool influence
  ppA0L,

  -- ** @Tau@

  -- | Treasury expansion
  ppTauL,

  -- ** @Rho@

  -- | Monetary expansion
  ppRhoL,

  -- ** @ProtocolVersion@

  -- | Protocol version
  ppProtocolVersionL,

  -- ** @MinUTxOValue@

  -- | Minimum allowed value of a new TxOut
  ppMinUTxOValueL,

  -- ** @MinPoolCast@

  -- | Miminum allowed stake pool cost
  ppMinPoolCostL,

  -- ** @KeyDeposit@
  ppKeyDepositL,

  -- ** @MaxTxSize@
  ppMaxTxSizeL,

  -- * Alonzo params

  -- ** @CostModels@
  ppCostModelsL,

  -- ** @Prices@
  ppPricesL,

  -- ** @MaxTxExUnits@

  -- | Limit the total per-transaction resource use for phase-2 scripts.
  ppMaxTxExUnitsL,

  -- ** @MaxBlockExUnits@

  -- | Limit the total per-transaction and per-block resource use for phase-2 scripts.
  ppMaxBlockExUnitsL,

  -- ** @MaxValSize@

  -- | The new parameter maxValSize replaces the constant @maxValSize@ used Mary era to
  -- limit the size of the Value part of an output in a serialised transaction.
  ppMaxValSizeL,

  -- ** @CollateralPercentage@

  -- | The parameter collateralPercent is used to specify the percentage of the total
  -- transaction fee its collateral must (at minimum) cover.
  ppCollateralPercentageL,

  -- ** @MaxCollateralInputs@

  -- | The parameter @maxCollateralInputs@ is used to limit, additionally, the total number
  -- of collateral inputs, and thus the total number of additional signatures that must be
  -- checked during validation.
  ppMaxCollateralInputsL,

  -- * Babbage params

  -- | Protocol parameters introduced in Babbage era

  -- ** @CoinsPerUTxOByte@

  -- | Cost in the amount of lovelace ber byte.
  CoinPerByte (..),
  ppCoinsPerUTxOByteL,
  ppGovActionDepositL,
) where

import Cardano.Ledger.Api (
  CoinPerByte (..),
  ppA0L,
  ppCoinsPerUTxOByteL,
  ppCollateralPercentageL,
  ppCostModelsL,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxBlockExUnitsL,
  ppMaxCollateralInputsL,
  ppMaxTxExUnitsL,
  ppMaxTxSizeL,
  ppMaxValSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppMinPoolCostL,
  ppMinUTxOValueL,
  ppNOptL,
  ppPoolDepositL,
  ppPricesL,
  ppProtocolVersionL,
  ppRhoL,
  ppTauL,
 )
import Cardano.Ledger.Conway.PParams (ppGovActionDepositL)
import GeniusYield.Types.Ada as X
import GeniusYield.Types.Address as X
import GeniusYield.Types.Anchor as X
import GeniusYield.Types.Blueprint as X
import GeniusYield.Types.BuildScript as X
import GeniusYield.Types.BuildWitness as X
import GeniusYield.Types.Certificate as X
import GeniusYield.Types.Credential as X
import GeniusYield.Types.DRep as X
import GeniusYield.Types.Datum as X
import GeniusYield.Types.Delegatee as X
import GeniusYield.Types.Epoch as X
import GeniusYield.Types.Era as X
import GeniusYield.Types.Governance as X
import GeniusYield.Types.Key as X
import GeniusYield.Types.KeyHash as X
import GeniusYield.Types.KeyRole as X
import GeniusYield.Types.Ledger as X
import GeniusYield.Types.Logging as X
import GeniusYield.Types.Natural as X
import GeniusYield.Types.NetworkId as X
import GeniusYield.Types.OpenApi as X
import GeniusYield.Types.PaymentKeyHash as X
import GeniusYield.Types.PlutusVersion as X
import GeniusYield.Types.Pool as X
import GeniusYield.Types.ProtocolParameters as X
import GeniusYield.Types.Providers as X
import GeniusYield.Types.PubKeyHash as X
import GeniusYield.Types.Rational as X
import GeniusYield.Types.Redeemer as X
import GeniusYield.Types.Reexpose as X
import GeniusYield.Types.Script as X
import GeniusYield.Types.Slot as X
import GeniusYield.Types.SlotConfig as X
import GeniusYield.Types.StakeAddressInfo as X
import GeniusYield.Types.StakeKeyHash as X
import GeniusYield.Types.StakePoolId as X
import GeniusYield.Types.Time as X
import GeniusYield.Types.Tx as X
import GeniusYield.Types.TxBody as X
import GeniusYield.Types.TxCert as X
import GeniusYield.Types.TxIn as X
import GeniusYield.Types.TxMetadata as X
import GeniusYield.Types.TxOut as X
import GeniusYield.Types.TxOutRef as X
import GeniusYield.Types.TxWdrl as X
import GeniusYield.Types.UTxO as X
import GeniusYield.Types.Value as X
import GeniusYield.Types.Wallet as X
import Numeric.Natural (Natural)
