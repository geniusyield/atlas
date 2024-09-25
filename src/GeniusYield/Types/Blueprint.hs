-- TODO: Haddock properly, exports and all module functions, etc.

{- |
Module      : GeniusYield.Types.Blueprint
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint (
  module X,
  writeBlueprint,
  readBlueprint,
) where

import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as LBS
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

readBlueprint :: FilePath -> IO ContractBlueprint
readBlueprint = readJSON