{- |
Module      : GeniusYield.Types.Blueprint.Write
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Blueprint.Write (writeBlueprint, encodeBlueprint) where

import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy qualified as LBS
import GeniusYield.Types.Blueprint.Contract (ContractBlueprint)

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
