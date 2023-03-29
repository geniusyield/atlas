{-|
Module      : GeniusYield.Providers.GCP
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.GCP (gcpFormatter) where

import qualified Data.Text.Lazy             as LTxt
import qualified Data.Text.Lazy.Builder     as TxtB
import           Language.Haskell.TH.Syntax

import           Data.Aeson                 (Value, (.=))
import qualified Data.Aeson                 as Aeson
import           Katip
import           Katip.Scribes.Handle

import           GeniusYield.Imports

gcpFormatter :: LogItem a => ItemFormatter a
gcpFormatter
  withColor
  verb
  Item
    { _itemSeverity = severity
    , _itemThread = tid
    , _itemPayload = payload
    , _itemMessage = LogStr msgBuilder
    , _itemTime = time
    , _itemNamespace = Namespace namespaces
    , _itemLoc = locMaybe
    }
  = TxtB.fromText
        $ colorBySeverity withColor severity
            $ LTxt.toStrict $ lazyDecodeUtf8Lenient $ Aeson.encode obj
  where
    obj = Aeson.object
        [ "severity"     .= toGCPSeverity severity
        , "message"      .= TxtB.toLazyText msgBuilder
        , "extraPayload" .= payloadObject verb payload
        , "time"         .= time
        , "threadId"     .= tid
        , "logging.googleapis.com/sourceLocation" .= (toGCPLoc <$> locMaybe)
        , "logging.googleapis.com/labels" .= Aeson.object ["namespaces" .= namespaces]
        ]

toGCPLoc :: Loc -> Value
toGCPLoc Loc {loc_filename, loc_package, loc_module, loc_start=(!lineNum, _)} = Aeson.object
    [ "file"    .= loc_filename
    , "package" .= loc_package
    , "module"  .= loc_module
    , "line"    .= lineNum
    ]

toGCPSeverity :: Severity -> Text
toGCPSeverity DebugS     = "DEBUG"
toGCPSeverity InfoS      = "INFO"
toGCPSeverity NoticeS    = "NOTICE"
toGCPSeverity WarningS   = "WARNING"
toGCPSeverity ErrorS     = "ERROR"
toGCPSeverity CriticalS  = "CRITICAL"
toGCPSeverity AlertS     = "ALERT"
toGCPSeverity EmergencyS = "EMERGENCY"
