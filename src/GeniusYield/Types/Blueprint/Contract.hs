{- |
Module      : GeniusYield.Types.Blueprint.Contract
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Contract (ContractBlueprint (..)) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Deriving.Aeson
import GeniusYield.Aeson.Utils (buildObject, optionalField, requiredField)
import GeniusYield.Imports (Text)
import GeniusYield.Types.Aeson (LowerFirst)
import GeniusYield.Types.Blueprint.DefinitionId (DefinitionId)
import GeniusYield.Types.Blueprint.Preamble (Preamble)
import GeniusYield.Types.Blueprint.Schema (Schema)
import GeniusYield.Types.Blueprint.Validator (ValidatorBlueprint)

-- | A blueprint of a smart contract, as defined by the CIP-0057.
data ContractBlueprint
  = MkContractBlueprint
  { contractId :: Maybe Text
  -- ^ An optional identifier for the contract.
  , contractPreamble :: Preamble
  -- ^ An object with meta-information about the contract.
  , contractValidators :: Set ValidatorBlueprint
  -- ^ A set of validator blueprints that are part of the contract.
  , contractDefinitions :: Map DefinitionId Schema
  -- ^ A registry of schema definitions used across the blueprint.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "contract", LowerFirst, Rename "id" "$id"]] ContractBlueprint

instance ToJSON ContractBlueprint where
  toJSON MkContractBlueprint {..} =
    buildObject $
      requiredField "$schema" schemaUrl
        . requiredField
          "$vocabulary"
          ( object
              [ "https://json-schema.org/draft/2020-12/vocab/core" .= True
              , "https://json-schema.org/draft/2020-12/vocab/applicator" .= True
              , "https://json-schema.org/draft/2020-12/vocab/validation" .= True
              , "https://cips.cardano.org/cips/cip57" .= True
              ]
          )
        . requiredField "preamble" contractPreamble
        . requiredField "validators" contractValidators
        . optionalField "$id" contractId
        . optionalField "definitions" (nonMempty contractDefinitions)
   where
    schemaUrl :: String
    schemaUrl = "https://cips.cardano.org/cips/cip57/schemas/plutus-blueprint.json"

    nonMempty :: (Eq a, Monoid a) => a -> Maybe a
    nonMempty a = if a == mempty then Nothing else Just a
