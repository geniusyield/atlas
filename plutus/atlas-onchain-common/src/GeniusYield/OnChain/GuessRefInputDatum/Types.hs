module GeniusYield.OnChain.GuessRefInputDatum.Types (
  RefInputDatum (..),
  Guess (..),
) where

import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Blueprint
import PlutusTx.Blueprint.TH qualified

newtype RefInputDatum = RefInputDatum Integer
PlutusTx.unstableMakeIsData ''RefInputDatum

-- Redeemer
newtype Guess = Guess Integer
  deriving stock Generic
  deriving anyclass HasBlueprintDefinition
$(PlutusTx.Blueprint.TH.unstableMakeIsDataSchema ''Guess)
