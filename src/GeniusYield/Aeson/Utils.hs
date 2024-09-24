{- |
Module      : GeniusYield.Aeson.Utils
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Aeson.Utils (
  buildObject,
  optionalField,
  requiredField,
) where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap

buildObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value
buildObject = Aeson.Object . ($ KeyMap.empty)

optionalField :: ToJSON a => Aeson.Key -> Maybe a -> Aeson.Object -> Aeson.Object
optionalField = maybe id . requiredField

requiredField :: ToJSON a => Aeson.Key -> a -> Aeson.Object -> Aeson.Object
requiredField key value = KeyMap.insert key (toJSON value)