{- |
Module      : GeniusYield.Types.Blueprint.Validator
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Validator (ValidatorBlueprint (..), CompiledValidator (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Deriving.Aeson
import GeniusYield.Imports (Text)
import GeniusYield.Types.Blueprint.Argument (ArgumentBlueprint)
import GeniusYield.Types.Blueprint.Parameter (ParameterBlueprint)
import GeniusYield.Types.Script.ScriptHash (GYScriptHash)

-- | A blueprint of a validator, as defined by the CIP-0057.
data ValidatorBlueprint = MkValidatorBlueprint
  { validatorTitle :: Text
  -- ^ A short and descriptive name for the validator.
  , validatorDescription :: Maybe Text
  -- ^ An informative description of the validator.
  , validatorRedeemer :: ArgumentBlueprint
  -- ^ A description of the redeemer format expected by this validator.
  , validatorDatum :: Maybe ArgumentBlueprint
  -- ^ A description of the datum format expected by this validator.
  , validatorParameters :: [ParameterBlueprint]
  -- ^ A list of parameters required by the script.
  , validatorCompiled :: Maybe CompiledValidator
  -- ^ A full compiled and CBOR-encoded serialized flat script together with its hash.
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON ValidatorBlueprint where
  parseJSON = withObject "ValidatorBlueprint" $ \o -> do
    validatorTitle <- o .: "title"
    validatorDescription <- o .:? "description"
    validatorRedeemer <- o .: "redeemer"
    validatorDatum <- o .:? "datum"
    validatorParameters <- o .: "parameters"
    validatorCompiledCode <- o .:? "compiledCode"
    validatorHash <- o .:? "hash"

    validatorCompiled <- case (validatorCompiledCode, validatorHash) of
      (Just code, Just hash) -> pure $ Just $ MkCompiledValidator code hash
      (Nothing, Nothing) -> pure Nothing
      _ -> fail "Either both compiledCode and hash should be present or both should be absent."
    pure MkValidatorBlueprint {..}

data CompiledValidator = MkCompiledValidator
  { compiledValidatorCode :: Text
  , compiledValidatorHash :: GYScriptHash
  }
  deriving stock (Show, Eq, Ord)