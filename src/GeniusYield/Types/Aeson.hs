module GeniusYield.Types.Aeson (LowerFirst) where

import Data.Char (toLower)
import Deriving.Aeson

-- Will lower the first character for your type.
data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier "" = ""
  getStringModifier (c : cs) = toLower c : cs