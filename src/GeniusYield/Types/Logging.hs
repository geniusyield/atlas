{-|
Module      : GeniusYield.Types.Logging
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Logging
    ( -- * Severity
      GYLogSeverity (..)
    , logSeverityToKatip
      -- * Verbosity
    , GYLogVerbosity (..)
    , logVerbosityToKatip
      -- * Namespace
    , GYLogNamespace
    , logNamespaceFromKatip
    , logNamespaceToKatip
      -- * Log contexts
    , GYLogContexts
    , logContextsFromKatip
    , logContextsToKatip
    , addContext
    , sl
    , logContextsToS
      -- * Log environment
    , GYLogEnv
    , logEnvFromKatip
    , logEnvToKatip
    , closeScribes
      -- * Log configuration
    , GYLogConfiguration (..)
    , GYRawLog (..)
    , RawLogger (..)
    , unitRawLogger
    , simpleRawLogger
    , cfgAddNamespace
    , cfgAddContext
    , logRun
      -- * Scribe Configuration
    , GYLogScribeType (..)
    , GYLogScribeConfig (..)
    , LogSrc (..)
      -- * Utilities
    , prettyNamespace
    , mkLogEnv
    ) where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Aeson                   (Key)
import qualified Data.Aeson                   as Aeson
import qualified Data.Aeson.Key               as Key
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Data.List                    (intercalate, isSuffixOf)
import           Data.Maybe                   (fromJust)
import           Data.String.Conv             (StringConv, toS)
import qualified Data.Text                    as Text
import           GeniusYield.Imports
import           GeniusYield.Providers.GCP    (gcpFormatter)
import qualified GeniusYield.Providers.Sentry as Sentry
import           GHC.Stack                    (withFrozenCallStack)
import qualified Katip                        as K
import qualified Katip.Core                   as KC
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
-- >>> import           Data.Text                  (Text)

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
data GYLogSeverity = GYDebug | GYInfo | GYWarning | GYError
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

instance Aeson.ToJSON GYLogSeverity where
    toJSON GYDebug   = "Debug"
    toJSON GYInfo    = "Info"
    toJSON GYWarning = "Warning"
    toJSON GYError   = "Error"

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
instance Aeson.FromJSON GYLogSeverity where
    parseJSON = Aeson.withText "GYLogSeverity" $ \t ->
        if | t == "Debug"   -> return GYDebug
           | t == "Info"    -> return GYInfo
           | t == "Warning" -> return GYWarning
           | t == "Error"   -> return GYError
           | otherwise      -> fail $ "unknown GYLogSeverity: " <> Text.unpack t

logSeverityToKatip :: GYLogSeverity -> K.Severity
logSeverityToKatip GYDebug   = K.DebugS
logSeverityToKatip GYInfo    = K.InfoS
logSeverityToKatip GYWarning = K.WarningS
logSeverityToKatip GYError   = K.ErrorS

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

logNamespaceFromKatip :: K.Namespace -> GYLogNamespace
logNamespaceFromKatip = coerce

prettyNamespace :: GYLogNamespace -> String
prettyNamespace ns = intercalate "." $ map Text.unpack $ K.unNamespace $ logNamespaceToKatip ns

-------------------------------------------------------------------------------
-- Log contexts
-------------------------------------------------------------------------------

newtype GYLogContexts = GYLogContexts K.LogContexts
  deriving newtype (Semigroup, Monoid)

logContextsFromKatip :: K.LogContexts -> GYLogContexts
logContextsFromKatip = coerce

logContextsToKatip :: GYLogContexts -> K.LogContexts
logContextsToKatip = coerce

-- | Add a context to the log contexts. See `sl`.
addContext :: KC.LogItem i => i -> GYLogContexts -> GYLogContexts
addContext i ctx = ctx <> logContextsFromKatip (K.liftPayload i)

-- | Construct a simple log payload.
--
-- >>> Aeson.encode $ logContextsToKatip $ addContext (sl "key" "value") mempty
-- "{\"key\":\"value\"}"
--
sl :: forall a. ToJSON a => Text -> a -> K.SimpleLogPayload
sl = K.sl

-- | Get textual representation of log contexts.
--
-- >>> logContextsToS @Text $ addContext (sl "key" "value") mempty
-- "{\"key\":\"value\"}"
--
logContextsToS :: StringConv LBS8.ByteString a => GYLogContexts -> a
logContextsToS = logContextsToKatip >>> Aeson.encode >>> toS

-------------------------------------------------------------------------------
-- Log environment
-------------------------------------------------------------------------------

newtype GYLogEnv = GYLogEnv K.LogEnv

logEnvFromKatip :: K.LogEnv -> GYLogEnv
logEnvFromKatip = coerce

logEnvToKatip :: GYLogEnv -> K.LogEnv
logEnvToKatip = coerce

-- | Calls @closeScribes@ from Katip.
closeScribes :: GYLogEnv -> IO GYLogEnv
closeScribes genv = genv & logEnvToKatip & K.closeScribes <&> logEnvFromKatip

-------------------------------------------------------------------------------
-- Log configuration
-------------------------------------------------------------------------------

newtype RawLogger = RawLogger { unRawLogger :: GYLogContexts -> GYLogNamespace -> GYLogSeverity -> Text -> IO () }

-- | A logger that does ignores the logs.
unitRawLogger :: RawLogger
unitRawLogger = RawLogger $ \_ _ _ _ -> pure ()

-- | A logger that ignores context and namespace and filters messages based on severity.
simpleRawLogger :: GYLogSeverity -> (Text -> IO ()) -> RawLogger
simpleRawLogger targetSev putLog = RawLogger $ \_ _ sev -> when (targetSev <= sev) . putLog

data GYRawLog = GYRawLog
  { rawLogRun     :: RawLogger
  , rawLogCleanUp :: IO ()
  }

data GYLogConfiguration = GYLogConfiguration
  { cfgLogNamespace :: !GYLogNamespace
  , cfgLogContexts  :: !GYLogContexts
  , cfgLogDirector  :: !(Either GYLogEnv GYRawLog)
  }

cfgAddNamespace :: GYLogNamespace -> GYLogConfiguration -> GYLogConfiguration
cfgAddNamespace ns cfg = cfg { cfgLogNamespace = cfgLogNamespace cfg <> ns }

cfgAddContext :: KC.LogItem i => i -> GYLogConfiguration -> GYLogConfiguration
cfgAddContext i cfg = cfg { cfgLogContexts = addContext i (cfgLogContexts cfg) }

logRun :: (HasCallStack, MonadIO m, StringConv a Text) => GYLogConfiguration -> GYLogSeverity -> a -> m ()
logRun GYLogConfiguration {..} sev msg = case cfgLogDirector of
  Left cfgLogEnv -> withFrozenCallStack $ K.runKatipT (logEnvToKatip cfgLogEnv) $ K.logLoc (logContextsToKatip cfgLogContexts) (logNamespaceToKatip cfgLogNamespace) (logSeverityToKatip sev) (K.logStr msg)
  Right GYRawLog {..} -> liftIO $ unRawLogger rawLogRun cfgLogContexts cfgLogNamespace sev $ toS msg

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

mkLogEnv :: GYLogNamespace -> [GYLogScribeConfig] -> IO GYLogEnv
mkLogEnv ns cfgs = do
    logEnv <- K.initLogEnv (logNamespaceToKatip $ "GeniusYield" <> ns) ""
    logEnvFromKatip <$> foldM f logEnv cfgs
  where
    f :: K.LogEnv -> GYLogScribeConfig -> IO K.LogEnv
    f logEnv cfg = do
        (scribe, name) <- mkScribe cfg
        K.registerScribe name scribe K.defaultScribeSettings logEnv
