{- |
Module      : GeniusYield.Types.Blueprint.Purpose
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Purpose (Purpose (..)) where

import Deriving.Aeson
import Maestro.Types.Common (LowerFirst)

{- |
  As per CIP-57, a validator arguments (redeemer, datum) and validator parameters
  all must specify a purpose that indicates in which context they are used.
-}
data Purpose = Spend | Mint | Withdraw | Publish
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] Purpose
