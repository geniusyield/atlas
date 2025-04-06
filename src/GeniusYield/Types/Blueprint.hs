{- |
Module      : GeniusYield.Types.Blueprint
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

This module provides types related to contract blueprint (which largely correspond to ones defined by [PlutusTx](https://github.com/IntersectMBO/plutus/tree/master/plutus-tx/src/PlutusTx/Blueprint)) and related utilities.
-}
module GeniusYield.Types.Blueprint (
  module X,
  writeBlueprint,
  readBlueprint,
  extractBlueprintValidator,
) where

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Either.Extra (eitherToMaybe)
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import GeniusYield.Imports ((&), (<&>))
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types.Blueprint.Argument as X
import GeniusYield.Types.Blueprint.Contract as X
import GeniusYield.Types.Blueprint.DefinitionId as X
import GeniusYield.Types.Blueprint.Parameter as X
import GeniusYield.Types.Blueprint.Preamble as X
import GeniusYield.Types.Blueprint.Purpose as X
import GeniusYield.Types.Blueprint.Schema as X
import GeniusYield.Types.Blueprint.TH as X
import GeniusYield.Types.Blueprint.Validator as X
import GeniusYield.Types.PlutusVersion (SingPlutusVersionI)
import GeniusYield.Types.Script (GYScript, scriptFromSerialisedScript)

-- | Write a 'ContractBlueprint' to a file.
writeBlueprint :: FilePath -> ContractBlueprint -> IO ()
writeBlueprint f blueprint = LBS.writeFile f (encodeBlueprint blueprint)

encodeBlueprint :: ContractBlueprint -> LBS.ByteString
encodeBlueprint =
  encodePretty'
    Pretty.defConfig
      { Pretty.confIndent = Pretty.Spaces 2
      , Pretty.confCompare =
          Pretty.keyOrder
            [ "$id"
            , "$schema"
            , "$vocabulary"
            , "preamble"
            , "validators"
            , "definitions"
            , "title"
            , "description"
            , "version"
            , "plutusVersion"
            , "license"
            , "redeemer"
            , "datum"
            , "parameters"
            , "purpose"
            , "schema"
            ]
      , Pretty.confNumFormat = Pretty.Generic
      , Pretty.confTrailingNewline = True
      }
    . toJSON

-- | Read a 'ContractBlueprint' from a file.
readBlueprint :: FilePath -> IO ContractBlueprint
readBlueprint = readJSON

-- | Extracts a validator from 'ContractBlueprint'
extractBlueprintValidator :: SingPlutusVersionI v => ByteString -> Maybe (GYScript v)
extractBlueprintValidator bs = do
  bp <- Aeson.decodeStrict bs
  val <- contractValidators bp & Set.lookupMin
  valCC <- validatorCompiled val
  sbs <- encodeUtf8 (compiledValidatorCode valCC) & BS16.decode & eitherToMaybe <&> SBS.toShort
  pure $ scriptFromSerialisedScript sbs
