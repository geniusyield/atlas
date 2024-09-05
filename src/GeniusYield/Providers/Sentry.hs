{- |
Module      : GeniusYield.Providers.Sentry
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Sentry where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap (
  fromHashMapText,
  toHashMapText,
 )
import Data.Bifunctor (first)
import Data.HashMap.Internal (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as Builder
import Katip qualified
import Katip.Core qualified
import System.Log.Raven qualified as Raven
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import System.Log.Raven.Types qualified as Raven

mkSentryScribe :: Raven.SentryService -> Katip.PermitFunc -> Katip.Verbosity -> IO Katip.Scribe
mkSentryScribe ss pf vb = return $ Katip.Scribe logger (return ()) pf
  where
    logger :: (Katip.LogItem a) => Katip.Item a -> IO ()
    logger item = do
      let lvl = sentryLevel $ Katip._itemSeverity item
          msg = TL.unpack $ Builder.toLazyText $ Katip.unLogStr $ Katip._itemMessage item
          nmSpace = sentryNamespace $ Katip._itemNamespace item

      -- Register Sentry event
      -- https://hackage.haskell.org/package/raven-haskell-0.1.4.1/docs/System-Log-Raven.html#v:register
      Raven.register ss nmSpace lvl msg (`updateRecord` item)

    -- send Ktip.Loc data to sentry
    locAttr :: (Katip.LogItem a) => Katip.Item a -> HashMap T.Text Aeson.Value
    locAttr item = foldMap (HM.singleton "loc" . Aeson.toJSON . Katip.Core.LocJs) (Katip._itemLoc item)

    -- extra attributes we can send to sentry
    srExtra :: (Katip.LogItem a) => Katip.Item a -> HashMap String Aeson.Value
    srExtra item = toStringHashMap $ toHashMapText $ Katip.payloadObject vb (Katip._itemPayload item) <> fromHashMapText (locAttr item)
      where
        toStringHashMap :: HashMap T.Text Aeson.Value -> HashMap String Aeson.Value
        toStringHashMap = HM.fromList . map (first T.unpack) . HM.toList

    updateRecord :: (Katip.LogItem a) => Raven.SentryRecord -> Katip.Item a -> Raven.SentryRecord
    updateRecord record item =
      record
        { Raven.srEnvironment = Just $ T.unpack $ Katip.getEnvironment $ Katip._itemEnv item
        , Raven.srExtra = srExtra item
        , Raven.srTimestamp = Katip._itemTime item
        }

    -- Sentry Level for  Katip Log
    sentryLevel :: Katip.Severity -> Raven.SentryLevel
    sentryLevel Katip.DebugS = Raven.Debug
    sentryLevel Katip.InfoS = Raven.Info
    sentryLevel Katip.ErrorS = Raven.Error
    sentryLevel Katip.WarningS = Raven.Warning
    sentryLevel _ = Raven.Custom "Other"

    -- gives proper namespace for sentry
    --
    -- >>> sentryNamespace $ Katip.Namespace ["GeniusYield", "Providers", "Logging"]
    -- "GeniusYield.Providers.Logging"
    --
    sentryNamespace :: Katip.Namespace -> String
    sentryNamespace (Katip.Namespace ks) = T.unpack $ T.intercalate "." ks

--  minimum sentry service constructed from dsn
sentryService :: String -> Raven.SentryService
sentryService dsn =
  let sSettings = Raven.fromDSN dsn
   in Raven.SentryService sSettings id sendRecord Raven.silentFallback
