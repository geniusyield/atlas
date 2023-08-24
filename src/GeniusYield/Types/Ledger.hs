{-|
Module      : GeniusYield.Types.Ledger
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Ledger (PlutusToCardanoError (..)) where

import           Data.Text            (Text)

import qualified PlutusLedgerApi.V1 as Plutus

{- | 'PlutusToCardanoError' is raised when using Plutus to Cardano API type conversion functions
from plutus-ledger. It is a focused version of "Ledger.Tx.CardanoAPI.ToCardanoError".
-}
data PlutusToCardanoError
    -- | Deserialization failed; tag indicates the type being deserialized.
    = DeserialiseRawBytesError { ptceTag :: Text }
    -- | Raised when trying to convert a stake ptr plutus address.
    | StakePtrAddressUnsupported Plutus.Address
    -- | Wildcard unhandled constructors; shouldn't happen usually.
    | UnknownPlutusToCardanoError { ptceTag :: Text }
    deriving stock Show
