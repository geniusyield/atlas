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
  kupoAwaitTxConfirmed,
) where

import Cardano.Api qualified as Api
import Control.Concurrent (threadDelay)
import Control.Monad ((<=<))
import Data.Aeson (Value (Null), withObject, (.:))
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Deriving.Aeson
import GeniusYield.Imports
import GeniusYield.Providers.Common (
  datumFromCBOR,
  extractAssetClass,
  extractNonAdaToken,
  newServantClientEnv,
 )
import GeniusYield.Types (
  GYAddress,
  GYAddressBech32,
  GYAssetClass (..),
  GYAwaitTx,
  GYAwaitTxException (GYAwaitTxException),
  GYAwaitTxParameters (..),
  GYDatum,
  GYDatumHash,
  GYLookupDatum,
  GYNonAdaToken,
  GYOutDatum (GYOutDatumHash, GYOutDatumInline, GYOutDatumNone),
  GYPaymentCredential,
  GYQueryUTxO (..),
  GYScriptHash,
  GYTxId,
  GYTxOutRef,
  GYUTxO (..),
  GYUTxOs,
  GYValue,
  LowerFirst,
  addressFromBech32,
  addressToText,
  gyQueryUtxoAtAddressesDefault,
  gyQueryUtxoAtPaymentCredentialsDefault,
  gyQueryUtxoRefsAtAddressDefault,
  gyQueryUtxosAtTxOutRefsDefault,
  parseValueKM,
  paymentCredentialToHexText,
  scriptFromCBOR,
  simpleScriptFromCBOR,
  txIdToApi,
  txOutRefFromApiTxIdIx,
  txOutRefToTuple',
  utxosFromList,
  valueFromLovelace,
 )
import GeniusYield.Types qualified as GYTypes (PlutusVersion (..))
import GeniusYield.Types.Script (GYAnyScript (..))
import Servant.API (
  Capture,
  Get,
  Header,
  Headers (getResponse),
  JSON,
  QueryFlag,
  QueryParam,
  ResponseHeader (Header),
  lookupResponseHeader,
  (:>),
  type (:<|>) (..),
 )
import Servant.Client (
  ClientEnv,
  ClientError,
  ClientM,
  client,
  runClientM,
 )

{- $setup
>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson as Aeson
-}

-- | Kupo api env.
newtype KupoApiEnv = KupoApiEnv ClientEnv

-- | Returns a new 'KupoApiEnv' given the base url to query from.
newKupoApiEnv :: String -> IO KupoApiEnv
newKupoApiEnv baseUrl = KupoApiEnv <$> newServantClientEnv baseUrl

-- | Exceptions.
data KupoProviderException
  = -- | Error from the Kupo API.
    KupoApiError !Text !ClientError
  | -- | Received an absurd response from Kupo. This shouldn't ever happen.
    KupoAbsurdResponse !Text
  deriving stock (Eq, Show)
  deriving anyclass Exception

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
    if v == Null
      then pure $ KupoDatum Nothing
      else
        withObject
          "KupoDatum"
          ( \datumObject -> do
              datumBytes <- datumObject .: "datum"
              case datumFromCBOR datumBytes of
                Left e -> fail $ show e
                Right d -> pure $ KupoDatum (Just d)
          )
          v

data KupoScriptLanguage = Native | PlutusV1 | PlutusV2 | PlutusV3
  deriving stock (Eq, Ord, Show, Generic)
  deriving FromJSON via CustomJSON '[ConstructorTagModifier '[Rename "Native" "native", Rename "PlutusV1" "plutus:v1", Rename "PlutusV2" "plutus:v2", Rename "PlutusV3" "plutus:v3"]] KupoScriptLanguage

newtype KupoScript = KupoScript (Maybe GYAnyScript)
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
    if v == Null
      then pure $ KupoScript Nothing
      else
        withObject
          "KupoScript"
          ( \scriptObject -> do
              scriptHex <- scriptObject .: "script"
              scriptLanguage <- scriptObject .: "language"
              case scriptLanguage of
                Native -> pure $ KupoScript $ GYSimpleScript <$> simpleScriptFromCBOR scriptHex
                PlutusV1 -> pure $ KupoScript $ GYPlutusScript <$> scriptFromCBOR @'GYTypes.PlutusV1 scriptHex
                PlutusV2 -> pure $ KupoScript $ GYPlutusScript <$> scriptFromCBOR @'GYTypes.PlutusV2 scriptHex
                PlutusV3 -> pure $ KupoScript $ GYPlutusScript <$> scriptFromCBOR @'GYTypes.PlutusV3 scriptHex
          )
          v

data KupoValue = KupoValue
  { coins :: !Natural
  , assets :: !GYValue
  }
  deriving stock (Show, Eq, Ord, Generic)

-- >>> Aeson.decode @KupoValue "{\"coins\":1762790,\"assets\":{\"604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b\":1}}"
-- Just (KupoValue {coins = 1762790, assets = valueFromList [(GYToken "604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b" "",1)]})
--
-- >>> Aeson.decode @KupoValue "{\"coins\":1762790,\"assets\":{\"604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b.\":1}}"
-- Just (KupoValue {coins = 1762790, assets = valueFromList [(GYToken "604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b" "",1)]})
--
-- >>> Aeson.decode @KupoValue "{\"coins\":1762790,\"assets\":{\"604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b.474f4c44\":1}}"
-- Just (KupoValue {coins = 1762790, assets = valueFromList [(GYToken "604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b" "GOLD",1)]})
--
-- >>> Aeson.decode @KupoValue "{\"coins\":1762790,\"assets\":{\"604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b474f4c44\":1, \"604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8c.474f4c44\":2}}"
-- Just (KupoValue {coins = 1762790, assets = valueFromList [(GYToken "604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8b" "GOLD",1),(GYToken "604eed076ac858d58278d943f3ae79f9a0ea958712cd50dda49c0a8c" "GOLD",2)]})
instance FromJSON KupoValue where
  parseJSON = withObject "KupoValue" $ \v -> do
    coins <- v .: "coins"
    assets' <- v .: "assets"
    assets <- parseValueKM True assets'
    return KupoValue {coins = coins, assets = assets}

data KupoDatumType = Hash | Inline
  deriving stock (Show, Eq, Ord, Generic)
  deriving FromJSON via CustomJSON '[ConstructorTagModifier '[LowerFirst]] KupoDatumType

newtype KupoCreatedAt = KupoCreatedAt
  { slotNo :: Word64
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[CamelToSnake]] KupoCreatedAt

data KupoUtxo = KupoUtxo
  { transactionId :: !GYTxId
  , outputIndex :: !Api.TxIx
  , address :: !GYAddressBech32
  , value :: !KupoValue
  , datumHash :: !(Maybe GYDatumHash)
  , datumType :: !(Maybe KupoDatumType)
  , scriptHash :: !(Maybe GYScriptHash)
  , createdAt :: !KupoCreatedAt
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving FromJSON via CustomJSON '[FieldLabelModifier '[CamelToSnake]] KupoUtxo

findDatumByHash :: GYDatumHash -> ClientM KupoDatum
findScriptByHash :: GYScriptHash -> ClientM KupoScript
fetchUtxosByPattern :: Pattern -> Bool -> Maybe Text -> Maybe Text -> ClientM (Headers '[Header "X-Most-Recent-Checkpoint" Word64] [KupoUtxo])

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
      :> QueryParam "policy_id" Text
      :> QueryParam "asset_name" Text
      :> Get '[JSON] (Headers '[Header "X-Most-Recent-Checkpoint" Word64] [KupoUtxo])

findDatumByHash :<|> findScriptByHash :<|> fetchUtxosByPattern = client @KupoApi Proxy

-- | Given a 'GYDatumHash' returns the corresponding 'GYDatum' if found.
kupoLookupDatum :: KupoApiEnv -> GYLookupDatum
kupoLookupDatum env dh = do
  KupoDatum md <-
    handleKupoError "LookupDatum"
      <=< runKupoClient env
      $ findDatumByHash dh
  pure md

-- | Given a 'GYScriptHash' returns the corresponding 'GYScript' if found.
kupoLookupScript :: KupoApiEnv -> GYScriptHash -> IO (Maybe GYAnyScript)
kupoLookupScript env sh = do
  KupoScript ms <-
    handleKupoError "LookupScript"
      <=< runKupoClient env
      $ findScriptByHash sh
  pure ms

-- | Find UTxOs at a given address.
kupoUtxosAtAddress :: KupoApiEnv -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
kupoUtxosAtAddress env addr mAssetClass = do
  let extractedAssetClass = extractAssetClass mAssetClass
      commonRequestPart = fetchUtxosByPattern (addressToText addr) True
  addrUtxos <-
    handleKupoError locationIdent <=< runKupoClient env $
      case extractedAssetClass of
        Nothing -> commonRequestPart Nothing Nothing
        Just (mp, tn) -> commonRequestPart (Just mp) (Just tn)
  utxosFromList <$> traverse (transformUtxo env) (getResponse addrUtxos)
 where
  locationIdent = "AddressesUtxo"

kupoUtxosWithAsset :: KupoApiEnv -> GYNonAdaToken -> IO GYUTxOs
kupoUtxosWithAsset env ac = do
  let (pid, tn) = extractNonAdaToken ac
  utxos <-
    handleKupoError locationIdent <=< runKupoClient env $
      fetchUtxosByPattern (pid <> "." <> tn) True Nothing Nothing
  utxosFromList <$> traverse (transformUtxo env) (getResponse utxos)
 where
  locationIdent = "UtxosWithAsset"

kupoUtxoAtTxOutRef :: KupoApiEnv -> GYTxOutRef -> IO (Maybe GYUTxO)
kupoUtxoAtTxOutRef env oref = do
  let (txId, utxoIdx) = txOutRefToTuple' oref
  utxo <-
    handleKupoError locationIdent <=< runKupoClient env $
      fetchUtxosByPattern (Text.pack (show utxoIdx) <> "@" <> txId) True Nothing Nothing
  listToMaybe <$> traverse (transformUtxo env) (getResponse utxo)
 where
  locationIdent = "UtxoByRef"

kupoUtxosAtPaymentCredential :: KupoApiEnv -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
kupoUtxosAtPaymentCredential env cred mAssetClass = do
  let extractedAssetClass = extractAssetClass mAssetClass
      commonRequestPart = fetchUtxosByPattern (paymentCredentialToHexText cred <> "/*") True
  credUtxos <-
    handleKupoError locationIdent <=< runKupoClient env $
      case extractedAssetClass of
        Nothing -> commonRequestPart Nothing Nothing
        Just (mp, tn) -> commonRequestPart (Just mp) (Just tn)
  utxosFromList <$> traverse (transformUtxo env) (getResponse credUtxos)
 where
  locationIdent = "PaymentCredentialUtxos"

transformUtxo :: KupoApiEnv -> KupoUtxo -> IO GYUTxO
transformUtxo env KupoUtxo {..} = do
  let ref = txOutRefFromApiTxIdIx (txIdToApi transactionId) outputIndex
  dat <- case datumType of
    Nothing -> pure GYOutDatumNone
    Just Hash -> do
      dh <- maybe (handleKupoAbsurdResponse locationIdent $ commonDatumHashError <> "'hash'") pure datumHash
      pure $ GYOutDatumHash dh
    Just Inline -> do
      dh <- maybe (handleKupoAbsurdResponse locationIdent $ commonDatumHashError <> "'inline'") pure datumHash
      dat' <- kupoLookupDatum env dh
      maybe (handleKupoAbsurdResponse locationIdent $ Text.pack $ "UTxO: " <> show ref <> " has inline datum, but no datum preimage found for given hash " <> show dh) (pure . GYOutDatumInline) dat'
  sc <- case scriptHash of
    Nothing -> pure Nothing
    Just sh -> kupoLookupScript env sh
  pure $
    GYUTxO
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
    , gyQueryUtxosWithAsset' = kupoUtxosWithAsset env
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
kupoAwaitTxConfirmed env p@GYAwaitTxParameters {..} txId = go 0
 where
  go attempt
    | attempt >= maxAttempts = throwIO $ GYAwaitTxException p
    | otherwise = do
        utxos <-
          handleKupoError locationIdent <=< runKupoClient env $
            fetchUtxosByPattern (Text.pack $ "*@" <> show txId) False Nothing Nothing -- We don't require for only @unspent@. Kupo with @--prune-utxo@ option would still keep spent UTxOs until their spent record is truly immutable (see Kupo docs for more details).
        case listToMaybe (getResponse utxos) of
          Nothing -> threadDelay checkInterval >> go (attempt + 1)
          Just u -> do
            let slotsToWait = 3 * confirmations * 20 -- Ouroboros Praos guarantees that there are at least @k@ blocks in a window of @3k / f@ slots where @f@ is the active slot coefficient, which is @0.05@ for Mainnet, Preprod & Preview.
            case (lookupResponseHeader utxos :: ResponseHeader "X-Most-Recent-Checkpoint" Word64) of
              Header slotOfCurrentBlock -> unless (slotNo (createdAt u) + slotsToWait <= slotOfCurrentBlock) $ threadDelay checkInterval >> go (attempt + 1)
              _ -> handleKupoAbsurdResponse locationIdent "Header 'X-Most-Recent-Checkpoint' isn't seen in response"
   where
    locationIdent = "AwaitTx"
