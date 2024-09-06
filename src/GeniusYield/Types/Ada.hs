{- |
Module      : GeniusYield.Types.Ada
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Ada (
  Ada (Ada),
  adaSymbol,
  adaToken,
  toLovelace,
  toValue,
  fromValue,
  lovelaceOf,
  lovelaceValueOf,
) where

import Data.Fixed (Fixed (MkFixed), Micro)

import PlutusLedgerApi.V1.Value (Value, adaSymbol, adaToken)
import PlutusLedgerApi.V1.Value qualified as Value

-- | Ada represented with a 'Micro' value.
newtype Ada = Ada Micro
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

-- | Convert Ada amount to its corresponding Lovelace unit.
toLovelace :: Ada -> Integer
toLovelace (Ada (MkFixed i)) = i

-- | Create a 'Ada' from given amount in lovelace.
lovelaceOf :: Integer -> Ada
lovelaceOf = Ada . MkFixed

-- | Create a 'Value' containing given amount of Ada.
toValue :: Ada -> Value
toValue = Value.singleton adaSymbol adaToken . toLovelace

-- | Obtain the 'Ada' amount contained within a 'Value'.
fromValue :: Value -> Ada
fromValue v = Ada . MkFixed $ Value.valueOf v adaSymbol adaToken

-- | Obtain the lovelace amount contained within a 'Value'.
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = toValue . lovelaceOf
