{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : GeniusYield.Types.Logging
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Logging
    ( -- * Severity
      GYLogSeverity (GYDebug, GYInfo, GYWarning, GYError)
      -- * Verbosity
    , GYLogVerbosity (..)
      -- * Namespace
    , GYLogNamespace
      -- * Scribe Configuration
    , GYLogScribeType (..)
    , GYLogScribeConfig (..)
    , LogSrc (..)
      -- * Utilities
    , logSeverityToKatip
    , logVerbosityToKatip
    , logNamespaceToKatip
    , prettyNamespace
    , mkLogEnv
    ) where

import           Data.Aeson                   (Key)
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Key               as Key
import           Data.List                    (intercalate, isSuffixOf)
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as Text
import           GeniusYield.Imports
import           GeniusYield.Providers.GCP    (gcpFormatter)
import qualified GeniusYield.Providers.Sentry as Sentry
import qualified Katip                        as K
import           Network.URI                  (URI (..), URIAuth (..),
                                               parseURIReference)
import           System.IO                    (stderr, stdout)
import qualified Text.Printf                  as Printf

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Text.Printf                (printf)

-------------------------------------------------------------------------------
-- Severity
-------------------------------------------------------------------------------

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode GYDebug
-- "Debug"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYInfo
-- "Info"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYWarning
-- "Warning"
--
-- >>> LBS8.putStrLn $ Aeson.encode GYError
-- "Error"
--
newtype GYLogSeverity = GYLogSeverity K.Severity
    deriving stock (Show, Read)
    deriving newtype (Eq, Ord, Enum, Bounded, Aeson.FromJSON, Aeson.ToJSON)

pattern GYDebug, GYInfo, GYWarning, GYError :: GYLogSeverity
pattern GYDebug = GYLogSeverity K.DebugS
pattern GYInfo = GYLogSeverity K.InfoS
pattern GYWarning = GYLogSeverity K.WarningS
pattern GYError = GYLogSeverity K.ErrorS

-- |
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Debug\""
-- Right GYDebug
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Info\""
-- Right GYInfo
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Warning\""
-- Right GYWarning
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Error\""
-- Right GYError
--
-- >>> Aeson.eitherDecode @GYLogSeverity "\"Fatal\""
-- Left "Error in $: unknown GYLogSeverity: Fatal"
--


logSeverityToKatip :: GYLogSeverity -> K.Severity
logSeverityToKatip = coerce

-------------------------------------------------------------------------------
-- Verbosity
-------------------------------------------------------------------------------

-- |
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V0\""
-- Right (GYLogVerbosity V0)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V1\""
-- Right (GYLogVerbosity V1)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V2\""
-- Right (GYLogVerbosity V2)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V3\""
-- Right (GYLogVerbosity V3)
--
-- >>> Aeson.eitherDecode @GYLogVerbosity "\"V4\""
-- Left "Error in $: Invalid Verbosity V4"
--
newtype GYLogVerbosity = GYLogVerbosity K.Verbosity
    deriving stock   (Show, Read)
    deriving newtype (Eq, Ord, Enum, Bounded, Aeson.FromJSON, Aeson.ToJSON)

logVerbosityToKatip :: GYLogVerbosity -> K.Verbosity
logVerbosityToKatip = coerce

-------------------------------------------------------------------------------
-- Namespace
-------------------------------------------------------------------------------

-- |
--
-- >>> "My" <> "Namespace" :: GYLogNamespace
-- GYLogNamespace (Namespace {unNamespace = ["My","Namespace"]})
--
newtype GYLogNamespace = GYLogNamespace K.Namespace
    deriving stock   (Show, Read, Eq, Ord)
    deriving newtype (Semigroup, Monoid, IsString)

-- |
--
-- >>> printf "%s" ("My" <> "Namespace" :: GYLogNamespace)
-- My.Namespace
--
instance Printf.PrintfArg GYLogNamespace where
    formatArg ns = Printf.formatArg (prettyNamespace ns)

logNamespaceToKatip :: GYLogNamespace -> K.Namespace
logNamespaceToKatip = coerce

prettyNamespace :: GYLogNamespace -> String
prettyNamespace ns = intercalate "." $ map Text.unpack $ K.unNamespace $ logNamespaceToKatip ns

-------------------------------------------------------------------------------
-- Scribe Configuration
-------------------------------------------------------------------------------

newtype LogSrc = LogSrc URI
  deriving (Show, Eq, Ord)

instance IsString LogSrc where
    fromString s = LogSrc $ fromJust $ parseURIReference s

instance Aeson.ToJSON LogSrc where
    toEncoding (LogSrc s) = Aeson.toEncoding $ show s
    toJSON (LogSrc s)     = Aeson.toJSON $ show s

instance Aeson.FromJSON LogSrc where
  parseJSON = Aeson.withText "LogSrc" $ \s -> do
      case parseURIReference $ Text.unpack s of
        Just u -> pure $ LogSrc u
        _      -> fail $ "Invalid URI: " <> show s

data GYLogScribeType = GYStdErrScribe | GYGCPScribe | GYCustomSourceScribe !LogSrc
    deriving (Show, Eq, Ord)

stdErrTag, gcpTag, typeTag, severityTag, verbosityTag, gySourceTag :: Key
stdErrTag    = "stderr"
gcpTag       = "gcp"
typeTag      = "type"
severityTag  = "severity"
verbosityTag = "verbosity"
gySourceTag  = "gySource"


-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode GYStdErrScribe
-- {"tag":"stderr"}
--
-- >>> LBS8.putStrLn $ Aeson.encode $ GYCustomSourceScribe "https://pub:priv@sentry.hostname.tld:8443/sentry/example_project"
-- {"tag":"gySource","source":"https://pub:...@sentry.hostname.tld:8443/sentry/example_project"}
--
-- >>> LBS8.putStrLn $ Aeson.encode $ GYCustomSourceScribe "log.txt"
-- {"tag":"gySource","source":"log.txt"}
--
instance Aeson.ToJSON GYLogScribeType where
    toJSON GYStdErrScribe                = Aeson.object ["tag" Aeson..= stdErrTag]
    toJSON GYGCPScribe                   = Aeson.object ["tag" Aeson..= gcpTag]
    toJSON (GYCustomSourceScribe source) = Aeson.object [ "tag" Aeson..= gySourceTag, "source" Aeson..= source]

    toEncoding GYStdErrScribe                = Aeson.pairs ("tag" Aeson..= stdErrTag)
    toEncoding GYGCPScribe                   = Aeson.pairs ("tag" Aeson..= gcpTag)
    toEncoding (GYCustomSourceScribe source) = Aeson.pairs ("tag" Aeson..= gySourceTag <> "source" Aeson..= source)

-- |
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"stderr\"}"
-- Right GYStdErrScribe
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"gySource\",\"source\":\"log.txt\"}"
-- Right (GYCustomSourceScribe (LogSrc log.txt))
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"gySource\",\"source\":\"https://pub:priv@sentry.hostname.tld:8443/sentry/example_project\"}"
-- Right (GYCustomSourceScribe (LogSrc https://pub:...@sentry.hostname.tld:8443/sentry/example_project))
--
-- >>> Aeson.eitherDecode @GYLogScribeType "{\"tag\":\"fancy-scribe\"}"
-- Left "Error in $: unknown GYLogScribe tag: fancy-scribe"
--
instance Aeson.FromJSON GYLogScribeType where
    parseJSON = Aeson.withObject "GYLogScribeType" $ \x -> do
        tag <- x Aeson..: "tag"
        if | tag == stdErrTag   -> pure GYStdErrScribe
           | tag == gcpTag      -> pure GYGCPScribe
           | tag == gySourceTag -> GYCustomSourceScribe <$> x Aeson..: "source"
           | otherwise          -> fail $ "unknown GYLogScribe tag: " <> Key.toString tag

data GYLogScribeConfig = GYLogScribeConfig
        { cfgLogType      :: !GYLogScribeType
        , cfgLogSeverity  :: !GYLogSeverity
        , cfgLogVerbosity :: !GYLogVerbosity
        } deriving (Show, Eq, Ord)

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode $ GYLogScribeConfig (GYCustomSourceScribe "log.txt") GYWarning (read "GYLogVerbosity V1")
-- {"type":{"tag":"gySource","source":"log.txt"},"severity":"Warning","verbosity":"V1"}
--
instance Aeson.ToJSON GYLogScribeConfig where
    toJSON GYLogScribeConfig {..} = Aeson.object
        [ typeTag      Aeson..= cfgLogType
        , severityTag  Aeson..= cfgLogSeverity
        , verbosityTag Aeson..= cfgLogVerbosity
        ]

    toEncoding GYLogScribeConfig {..} = Aeson.pairs
        ( typeTag      Aeson..= cfgLogType
        <> severityTag  Aeson..= cfgLogSeverity
        <> verbosityTag Aeson..= cfgLogVerbosity
        )


-- |
--
-- >>> Aeson.decode @GYLogScribeConfig "{\"severity\":\"Warning\",\"verbosity\":\"V1\",\"type\":{\"tag\":\"gySource\",\"source\":\"log.txt\"}}"
-- Just (GYLogScribeConfig {cfgLogType = GYCustomSourceScribe (LogSrc log.txt), cfgLogSeverity = GYWarning, cfgLogVerbosity = GYLogVerbosity V1})
--
instance Aeson.FromJSON GYLogScribeConfig where
    parseJSON = Aeson.withObject "GYLogScribeConfig" $ \x ->
        GYLogScribeConfig <$> (x Aeson..: typeTag)
                          <*> (x Aeson..: severityTag)
                          <*> (x Aeson..: verbosityTag)

mkScribe :: GYLogScribeConfig -> IO (K.Scribe, Text.Text)
mkScribe GYLogScribeConfig {..} = case cfgLogType of
    GYStdErrScribe -> do
        scribe <- K.mkHandleScribe K.ColorIfTerminal stderr permit verbosity
        pure (scribe, "stderr")

    GYGCPScribe -> do
        scribe <- K.mkHandleScribeWithFormatter gcpFormatter K.ColorIfTerminal stdout permit verbosity
        pure (scribe, "gcp-stdout")

    GYCustomSourceScribe source -> do
        scribe <- customSourceScribe source
        pure (scribe, Text.pack $ show source)

  where
    permit :: K.PermitFunc
    permit = K.permitItem $ logSeverityToKatip cfgLogSeverity

    verbosity :: K.Verbosity
    verbosity = logVerbosityToKatip cfgLogVerbosity

    customSourceScribe :: LogSrc -> IO K.Scribe
    customSourceScribe (LogSrc uri) = case uri of
      URI {uriScheme = "", uriPath = path} ->
        K.mkFileScribe path permit verbosity

      URI {uriScheme = s, uriAuthority = Just URIAuth{uriRegName = domainName}}
        | s `elem` ["http:", "https:"] && "sentry.io" `isSuffixOf` domainName ->
          Sentry.mkSentryScribe (Sentry.sentryService $ show uri) permit verbosity
      x ->
        fail $ "Unsupported LogSrc: " <> show x

mkLogEnv :: GYLogNamespace -> [GYLogScribeConfig] -> IO K.LogEnv
mkLogEnv ns cfgs = do
    logEnv <- K.initLogEnv (logNamespaceToKatip $ "GeniusYield" <> ns) ""
    foldM f logEnv cfgs
  where
    f :: K.LogEnv -> GYLogScribeConfig -> IO K.LogEnv
    f logEnv cfg = do
        (scribe, name) <- mkScribe cfg
        K.registerScribe name scribe K.defaultScribeSettings logEnv
