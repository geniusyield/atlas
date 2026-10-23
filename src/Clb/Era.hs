{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Clb.Era (
  -- EmulatorEra,
  IsCardanoLedgerEra,
  IsCardanoLedgerEra',
  CardanoLedgerEra,
  IsMaryBasedEra (maryBasedEra),
  DefaultEmulatorEra,
) where

import Cardano.Api (
  AlonzoEra,
  BabbageEra,
  ConwayEra,
  IsMaryBasedEra (..),
  IsShelleyBasedEra,
  MaryEra,
  ShelleyLedgerEra,
 )
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Shelley.API (ApplyTx)
import Data.Default (Default)

type CardanoLedgerEra era = ShelleyLedgerEra era

{- | Helper class for constraining the 'CardanoLedgerEra' closed type family.
Ensures user chosen era (from Cardano.Api.Internals.Eras) is valid for usage with CLB.
-}
class
  ( ledgerEra ~ CardanoLedgerEra era
  , ledgerEra ~ ShelleyLedgerEra era
  , L.EraTxOut ledgerEra
  , ApplyTx ledgerEra
  , Default (L.GovState ledgerEra)
  , Show (L.GovState ledgerEra)
  , IsShelleyBasedEra era
  , IsMaryBasedEra era
  ) =>
  IsCardanoLedgerEra' era ledgerEra
    | era -> ledgerEra

type IsCardanoLedgerEra era = IsCardanoLedgerEra' era (CardanoLedgerEra era)

-- The `era` param should be the era to add (Mary onwards)
-- The `ledgerEra` param should be equal to 'CardanoLedgerEra era'. Look up its
-- definition in "Cardano.Api.Internals.Eras"
instance IsCardanoLedgerEra' MaryEra L.MaryEra
instance IsCardanoLedgerEra' AlonzoEra L.AlonzoEra
instance IsCardanoLedgerEra' BabbageEra L.BabbageEra
instance IsCardanoLedgerEra' ConwayEra L.ConwayEra

-- NOTE: Add more eras here as hardforks happen.

type DefaultEmulatorEra = ConwayEra
