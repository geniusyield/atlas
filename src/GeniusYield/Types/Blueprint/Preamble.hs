{- |
Module      : GeniusYield.Types.Blueprint.Preamble
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Preamble (Preamble (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (..), withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Deriving.Aeson
import GeniusYield.Aeson.Utils (buildObject, optionalField, requiredField)
import GeniusYield.Imports (Text)
import GeniusYield.Types.PlutusVersion (PlutusVersion (..))

-- | Meta-information about the contract.
data Preamble = MkPreamble
  { preambleTitle :: Text
  -- ^ A short and descriptive title of the contract application.
  , preambleDescription :: Maybe Text
  -- ^ A more elaborate description.
  , preambleVersion :: Text
  -- ^ A version number for the project.
  , preamblePlutusVersion :: PlutusVersion
  -- ^ The Plutus version assumed for all validators.
  , preambleLicense :: Maybe Text
  -- ^ A license under which the specification
  -- and contract code is distributed.
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON Preamble where
  parseJSON = withObject "Preamble" $ \o ->
    MkPreamble
      <$> o .: "title"
      <*> o .:? "description"
      <*> o .: "version"
      <*> ((o .: "plutusVersion") >>= parsePlutusVersion)
      <*> o .:? "license"
   where
    parsePlutusVersion :: Text -> Parser PlutusVersion
    parsePlutusVersion = \case
      "v1" -> pure PlutusV1
      "v2" -> pure PlutusV2
      "v3" -> pure PlutusV3
      _ -> fail "Invalid Plutus version"

instance ToJSON Preamble where
  toJSON MkPreamble {..} =
    buildObject $
      requiredField "title" preambleTitle
        . optionalField "description" preambleDescription
        . requiredField "version" preambleVersion
        . requiredField "plutusVersion" (encodePlutusVersion preamblePlutusVersion)
        . optionalField "license" preambleLicense
   where
    encodePlutusVersion :: PlutusVersion -> Text
    encodePlutusVersion = \case
      PlutusV1 -> "v1"
      PlutusV2 -> "v2"
      PlutusV3 -> "v3"
