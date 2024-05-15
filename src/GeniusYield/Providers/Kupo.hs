{- |
Module      : GeniusYield.Providers.Kupo
Description : Kupo API to query for UTxOs.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}

module GeniusYield.Providers.Kupo (
  KupoApiEnv,
  newKupoApiEnv,
  kupoLookupDatum,
  kupoLookupScript,
  kupoQueryUtxo,
  kupoAwaitTxConfirmed
) where

import qualified Cardano.Api                  as Api
import           Control.Concurrent           (threadDelay)
import           Control.Monad                ((<=<))
import           Data.Aeson                   (Value (Null), withObject, (.:))
import           Data.Char                    (toLower)
import           Data.Maybe                   (listToMaybe)
import qualified Data.Text                    as Text
import           Deriving.Aeson
import           GeniusYield.Imports
import           GeniusYield.Providers.Common (datumFromCBOR, extractAssetClass,
                                               newServantClientEnv)
import           GeniusYield.Types            (GYAddress, GYAddressBech32,
                                               GYAssetClass (..), GYAwaitTx,
                                               GYAwaitTxException (GYAwaitTxException),
                                               GYAwaitTxParameters (..),
                                               GYDatum, GYDatumHash,
                                               GYLookupDatum,
                                               GYOutDatum (GYOutDatumHash, GYOutDatumInline, GYOutDatumNone),
                                               GYPaymentCredential,
                                               GYQueryUTxO (..), GYScript,
                                               GYScriptHash, GYTxId, GYTxOutRef,
                                               GYUTxO (..), GYUTxOs, GYValue,
                                               addressFromBech32, addressToText,
                                               gyQueryUtxoAtAddressesDefault,
                                               gyQueryUtxoAtPaymentCredentialsDefault,
                                               gyQueryUtxoRefsAtAddressDefault,
                                               gyQueryUtxosAtTxOutRefsDefault,
                                               paymentCredentialToHexText,
                                               scriptFromCBOR, txIdToApi,
                                               txOutRefFromApiTxIdIx,
                                               txOutRefToTuple', utxosFromList,
                                               valueFromLovelace)
import qualified GeniusYield.Types            as GYTypes (PlutusVersion (..))
import           GeniusYield.Types.Slot       (GYSlot, unsafeAdvanceSlot)
import           Servant.API                  (Capture, Get, Header,
                                               Headers (getResponse), JSON,
                                               QueryFlag, QueryParam,
                                               ResponseHeader (Header),
                                               ToHttpApiData,
                                               lookupResponseHeader,
                                               type (:<|>) (..), (:>))
import           Servant.Client               (ClientEnv, ClientError, ClientM,
                                               client, runClientM)
import           Web.HttpApiData              (ToHttpApiData (..))

-- $setup
-- >>> import qualified Data.Aeson as Aeson

-- | Kupo api env.
newtype KupoApiEnv = KupoApiEnv ClientEnv

-- | Returns a new 'KupoApiEnv' given the base url to query from.
newKupoApiEnv :: String -> IO KupoApiEnv
newKupoApiEnv baseUrl = KupoApiEnv <$> newServantClientEnv baseUrl

-- | Exceptions.
data KupoProviderException
  = -- | Error from the Kupo API.
    KupoApiError !Text !ClientError
    -- | Received an absurd response from Kupo. This shouldn't ever happen.
  | KupoAbsurdResponse !Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

{-# INLINEABLE handleKupoError #-}
handleKupoError :: Text -> Either ClientError a -> IO a
handleKupoError locationInfo = either (throwIO . KupoApiError locationInfo) pure

{-# INLINEABLE handleKupoAbsurdResponse #-}
handleKupoAbsurdResponse :: Text -> Text -> IO a
handleKupoAbsurdResponse locationInfo e = throwIO . KupoAbsurdResponse $ "At location: " <> locationInfo <> ", encountered absurd: " <> e <> "."

{-# INLINEABLE runKupoClient #-}
runKupoClient :: KupoApiEnv -> ClientM a -> IO (Either ClientError a)
runKupoClient (KupoApiEnv cEnv) c = runClientM c cEnv

-- | Pattern to match kupo results against.
type Pattern = Text

-- Will lower the first character for your type.
data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier ""       = ""
  getStringModifier (c : cs) = toLower c : cs

newtype KupoDatum = KupoDatum (Maybe GYDatum)
  deriving stock (Eq, Ord, Show, Generic)

-- >>> Aeson.eitherDecode @KupoDatum "null"
-- Right (KupoDatum Nothing)
-- >>> Aeson.eitherDecode @KupoDatum "{\"datum\": \"d8799f03ff\"}"
-- Right (KupoDatum (Just (GYDatum Constr 0 [I 3])))
-- >>> Aeson.eitherDecode @KupoDatum "{\"datum\": \"willNotWork\"}"
-- Left "Error in $: DeserializeErrorHex \"willNotWork\""
instance FromJSON KupoDatum where
  parseJSON v =
    -- Kupo returns "null" (under 200 response code) if it doesn't find the preimage.
    if v == Null then pure $ KupoDatum Nothing
    else
      withObject "KupoDatum"
        (\datumObject -> do
            datumBytes <- datumObject .: "datum"
            case datumFromCBOR datumBytes of
              Left e  -> fail $ show e
              Right d -> pure $ KupoDatum (Just d)
        ) v

data KupoScriptLanguage = Native | PlutusV1 | PlutusV2
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON) via CustomJSON '[ConstructorTagModifier '[Rename "Native" "native", Rename "PlutusV1" "plutus:v1", Rename "PlutusV2" "plutus:v2"]] KupoScriptLanguage

newtype KupoScript = KupoScript (Maybe (Some GYScript))
  deriving stock (Eq, Show, Generic)

-- >>> Aeson.eitherDecode @KupoScript "null"
-- Right (KupoScript Nothing)
-- >>> Aeson.eitherDecode @KupoScript "{\"language\": \"wontWork\", \"script\": \"49480100002221200101\"}"
-- Left "Error in $.language: parsing GeniusYield.Providers.Kupo.KupoScriptLanguage failed, expected one of the tags [\"native\",\"plutus:v1\",\"plutus:v2\"], but found tag \"wontWork\""
-- >>> Aeson.eitherDecode @KupoScript "{\"language\": \"plutus:v2\", \"script\": \"a\"}"
-- Right (KupoScript Nothing)
-- >>> Aeson.eitherDecode @KupoScript "{\"language\": \"plutus:v2\", \"script\": \"49480100002221200101\"}"
-- Right (KupoScript (Just (Some (GYScript "f14241393964259a53ca546af364e7f5688ca5aaa35f1e0da0f951b2"))))
instance FromJSON KupoScript where
  parseJSON v =
    -- Kupo returns "null" (under 200 response code) if it doesn't find the preimage.
    if v == Null then pure $ KupoScript Nothing
    else
      withObject "KupoScript"
        (\scriptObject -> do
            scriptHex <- scriptObject .: "script"
            scriptLanguage <- scriptObject .: "language"
            case scriptLanguage of
              Native -> pure $ KupoScript Nothing  -- native scripts are not supported.
              PlutusV1 -> pure $ KupoScript $ Some <$> scriptFromCBOR @'GYTypes.PlutusV1 scriptHex
              PlutusV2 -> pure $ KupoScript $ Some <$> scriptFromCBOR @'GYTypes.PlutusV2 scriptHex
        ) v

data KupoValue = KupoValue
  { coins  :: !Natural
  , assets :: !GYValue
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

data KupoDatumType = Hash | Inline
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] KupoDatumType

newtype KupoCreatedAt = KupoCreatedAt
  { slotNo     :: GYSlot
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] KupoCreatedAt

data KupoUtxo = KupoUtxo
  { transactionId :: !GYTxId
  , outputIndex   :: !Api.TxIx
  , address       :: !GYAddressBech32
  , value         :: !KupoValue
  , datumHash     :: !(Maybe GYDatumHash)
  , datumType     :: !(Maybe KupoDatumType)
  , scriptHash    :: !(Maybe GYScriptHash)
  , createdAt     :: !KupoCreatedAt
  , spentAt       :: !KupoCreatedAt
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] KupoUtxo

type MostRecentCheckpointHeader = Header "X-Most-Recent-Checkpoint" GYSlot

findDatumByHash :: GYDatumHash -> ClientM KupoDatum
findScriptByHash :: GYScriptHash -> ClientM KupoScript
fetchUtxosByPattern :: Pattern -> Bool -> Maybe KupoOrder -> Maybe Text -> Maybe Text -> ClientM (Headers '[MostRecentCheckpointHeader] [KupoUtxo])

data KupoOrder = KOMostRecentFirst | KOOldestFirst
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ToHttpApiData KupoOrder where
  toUrlPiece KOMostRecentFirst = "most-recent-first"
  toUrlPiece KOOldestFirst     = "oldest-first"

type KupoApi =
         "datums"
      :> Capture "datumHash" GYDatumHash
      :> Get '[JSON] KupoDatum
    :<|> "scripts"
      :> Capture "scriptHash" GYScriptHash
      :> Get '[JSON] KupoScript
    :<|> "matches"
      :> Capture "pattern" Pattern
      :> QueryFlag "unspent"
      :> QueryParam "order" KupoOrder
      :> QueryParam "policy_id" Text
      :> QueryParam "asset_name" Text
      :> Get '[JSON] (Headers '[MostRecentCheckpointHeader] [KupoUtxo])

findDatumByHash :<|> findScriptByHash :<|> fetchUtxosByPattern = client @KupoApi Proxy

-- | Given a 'GYDatumHash' returns the corresponding 'GYDatum' if found.
kupoLookupDatum :: KupoApiEnv -> GYLookupDatum
kupoLookupDatum env dh = do
  KupoDatum md <-
    handleKupoError "LookupDatum"
      <=< runKupoClient env $ findDatumByHash dh
  pure md

-- | Given a 'GYScriptHash' returns the corresponding 'GYScript' if found.
kupoLookupScript :: KupoApiEnv -> GYScriptHash -> IO (Maybe (Some GYScript))
kupoLookupScript env sh = do
  KupoScript ms <-
    handleKupoError "LookupScript"
      <=< runKupoClient env $ findScriptByHash sh
  pure ms

-- | Find UTxOs at a given address.
kupoUtxosAtAddress :: KupoApiEnv -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
kupoUtxosAtAddress env addr mAssetClass = do
  let extractedAssetClass = extractAssetClass mAssetClass
      commonRequestPart = fetchUtxosByPattern (addressToText addr) True Nothing
  addrUtxos <-
    handleKupoError locationIdent <=< runKupoClient env $
      case extractedAssetClass of
        Nothing       -> commonRequestPart Nothing Nothing
        Just (mp, tn) -> commonRequestPart (Just mp) (Just tn)
  utxosFromList <$> traverse (transformUtxo env) (getResponse addrUtxos)
  where
    locationIdent = "AddressesUtxo"

kupoUtxoAtTxOutRef :: KupoApiEnv -> GYTxOutRef -> IO (Maybe GYUTxO)
kupoUtxoAtTxOutRef env oref = do
  let (txId, utxoIdx) = txOutRefToTuple' oref
  utxo <-
    handleKupoError locationIdent <=< runKupoClient env $
      fetchUtxosByPattern (Text.pack (show utxoIdx) <> "@" <> txId) True Nothing Nothing Nothing
  listToMaybe <$> traverse (transformUtxo env) (getResponse utxo)
 where
  locationIdent = "UtxoByRef"

kupoUtxosAtPaymentCredential :: KupoApiEnv -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
kupoUtxosAtPaymentCredential env cred mAssetClass = do
  let extractedAssetClass = extractAssetClass mAssetClass
      commonRequestPart = fetchUtxosByPattern (paymentCredentialToHexText cred <> "/*") True Nothing
  credUtxos <-
    handleKupoError locationIdent <=< runKupoClient env $
      case extractedAssetClass of
        Nothing       -> commonRequestPart Nothing Nothing
        Just (mp, tn) -> commonRequestPart (Just mp) (Just tn)
  utxosFromList <$> traverse (transformUtxo env) (getResponse credUtxos)
 where
  locationIdent = "PaymentCredentialUtxos"

transformUtxo :: KupoApiEnv -> KupoUtxo -> IO GYUTxO
transformUtxo env KupoUtxo {..} = do
  let ref = txOutRefFromApiTxIdIx (txIdToApi transactionId) outputIndex
  dat <- case datumType of
    Nothing     -> pure GYOutDatumNone
    Just Hash   -> do
      dh <- maybe (handleKupoAbsurdResponse locationIdent $ commonDatumHashError <> "'hash'") pure datumHash
      pure $ GYOutDatumHash dh
    Just Inline -> do
      dh <- maybe (handleKupoAbsurdResponse locationIdent $ commonDatumHashError <> "'inline'") pure datumHash
      dat' <- kupoLookupDatum env dh
      maybe (handleKupoAbsurdResponse locationIdent $ Text.pack $ "UTxO: " <> show ref <> " has inline datum, but no datum preimage found for given hash " <> show dh) (pure . GYOutDatumInline) dat'
  sc <- case scriptHash of
    Nothing -> pure Nothing
    Just sh -> kupoLookupScript env sh
  pure $ GYUTxO
      { utxoRef = ref
      , utxoAddress = addressFromBech32 address
      , utxoValue = assets value <> valueFromLovelace (toInteger $ coins value)
      , utxoOutDatum = dat
      , utxoRefScript = sc
      }
  where
    locationIdent = "transformUtxo"
    commonDatumHashError = "No 'datum_hash' present in response whereas 'datum_type' mentions "

-- | Definition of 'GYQueryUTxO' for the Kupo provider.
kupoQueryUtxo :: KupoApiEnv -> GYQueryUTxO
kupoQueryUtxo env =
  GYQueryUTxO
    { gyQueryUtxosAtAddress' = kupoUtxosAtAddress env
    , gyQueryUtxosAtAddressWithDatums' = Nothing
    , gyQueryUtxosAtAddresses' = gyQueryUtxoAtAddressesDefault $ kupoUtxosAtAddress env
    , gyQueryUtxosAtTxOutRefs' = gyQueryUtxosAtTxOutRefsDefault $ kupoUtxoAtTxOutRef env
    , gyQueryUtxosAtTxOutRefsWithDatums' = Nothing
    , gyQueryUtxoAtTxOutRef' = kupoUtxoAtTxOutRef env
    , gyQueryUtxoRefsAtAddress' = gyQueryUtxoRefsAtAddressDefault $ kupoUtxosAtAddress env
    , gyQueryUtxosAtAddressesWithDatums' = Nothing
    , gyQueryUtxosAtPaymentCredential' = kupoUtxosAtPaymentCredential env
    , gyQueryUtxosAtPaymentCredWithDatums' = Nothing
    , gyQueryUtxosAtPaymentCredentials' = gyQueryUtxoAtPaymentCredentialsDefault $ kupoUtxosAtPaymentCredential env
    , gyQueryUtxosAtPaymentCredsWithDatums' = Nothing
    }

kupoAwaitTxConfirmed :: KupoApiEnv -> GYAwaitTx
kupoAwaitTxConfirmed env p@GYAwaitTxParameters{..} txId = go 0
  where
    go attempt
      | attempt >= maxAttempts = throwIO $ GYAwaitTxException p
      | otherwise = do
          utxos <-
            handleKupoError locationIdent <=< runKupoClient env $
              fetchUtxosByPattern (Text.pack $ "*@" <> show txId) False Nothing Nothing Nothing  -- We don't require for only @unspent@. Kupo with @--prune-utxo@ option would still keep spent UTxOs until their spent record is truly immutable (see Kupo docs for more details).
          case listToMaybe (getResponse utxos) of
            Nothing -> threadDelay checkInterval >> go (attempt + 1)
            Just u  -> do
              let slotsToWait :: Natural = 3 * fromIntegral confirmations * 20  -- Ouroboros Praos guarantees that there are at least @k@ blocks in a window of @3k / f@ slots where @f@ is the active slot coefficient, which is @0.05@ for Mainnet, Preprod & Preview.
              case (lookupResponseHeader utxos :: ResponseHeader "X-Most-Recent-Checkpoint" GYSlot) of
                Header slotOfCurrentBlock -> unless (let s = unsafeAdvanceSlot (slotNo (createdAt u)) slotsToWait in s <= slotOfCurrentBlock) $ threadDelay checkInterval >> go (attempt + 1)  -- @Word64@ wraps back to zero in case of overflow, so it's safe.
                _ -> handleKupoAbsurdResponse locationIdent "Header 'X-Most-Recent-Checkpoint' isn't seen in response"

          where
            locationIdent = "AwaitTx"
