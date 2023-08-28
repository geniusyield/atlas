{-|
Module      : GeniusYield.Types.Script
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Script (
    -- * Validator
    GYValidator,
    validatorFromPlutus,
    validatorFromSerialisedScript,
    validatorToSerialisedScript,
    validatorToApi,
    validatorFromApi,
    validatorToApiPlutusScriptWitness,

    -- ** File operations
    writeValidator,
    readValidator,

    -- ** Selectors
    validatorHash,
    validatorPlutusHash,
    validatorApiHash,
    validatorVersion,

    -- * ValidatorHash
    GYValidatorHash,
    validatorHashToApi,
    validatorHashToPlutus,
    validatorHashFromApi,
    validatorHashFromPlutus,

    -- * MintingPolicy
    GYMintingPolicy,
    mintingPolicyId,
    mintingPolicyVersion,
    mintingPolicyVersionFromWitness,
    mintingPolicyFromPlutus,
    mintingPolicyFromSerialisedScript,
    mintingPolicyToSerialisedScript,
    mintingPolicyToApi,
    mintingPolicyIdToText,
    mintingPolicyIdFromText,
    mintingPolicyFromApi,
    mintingPolicyToApiPlutusScriptWitness,

    -- * Witness for Minting Policy
    GYMintScript (..),
    mintingPolicyIdFromWitness,
    gyMintScriptToSerialisedScript,
    gyMintingScriptWitnessToApiPlutusSW,

    -- ** File operations
    writeMintingPolicy,
    readMintingPolicy,

    -- ** Selectors
    mintingPolicyCurrencySymbol,
    mintingPolicyApiId,
    mintingPolicyApiIdFromWitness,

    -- * MintingPolicyId
    GYMintingPolicyId,
    mintingPolicyIdToApi,
    mintingPolicyIdFromApi,
    mintingPolicyIdCurrencySymbol,

    -- * Script
    GYScript,
    scriptVersion,
    validatorToScript,
    mintingPolicyToScript,
    scriptToApi,
    scriptFromCBOR,
    scriptFromCBOR',
    scriptFromPlutus,
    scriptFromSerialisedScript,
    scriptToSerialisedScript,
    scriptApiHash,
    scriptPlutusHash,
    someScriptPlutusHash,
    someScriptToReferenceApi,
    someScriptFromReferenceApi,
    referenceScriptToApiPlutusScriptWitness,

    -- ** File operations
    writeScript,
    readScript,
) where

import           Data.GADT.Compare
import           Data.GADT.Show

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S
import           Control.Lens                     ((?~))
import           Data.Aeson.Types                 (FromJSONKey (fromJSONKey),
                                                   FromJSONKeyFunction (FromJSONKeyTextParser),
                                                   ToJSONKey (toJSONKey),
                                                   toJSONKeyText)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16           as BS16
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal.Schema     as Swagger
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as TE
import qualified PlutusLedgerApi.Common           as Plutus
import qualified PlutusLedgerApi.V1               as PlutusV1
import qualified PlutusTx
import qualified PlutusTx.Builtins                as PlutusTx
import qualified Text.Printf                      as Printf
import qualified Web.HttpApiData                  as Web

import           Data.ByteString                  (ByteString)
import           GeniusYield.Imports
import           GeniusYield.Types.Ledger         (PlutusToCardanoError (..))
import           GeniusYield.Types.PlutusVersion
import           GeniusYield.Types.TxOutRef       (GYTxOutRef, txOutRefToApi)
import           Data.ByteString.Short            (ShortByteString)

-- $setup
--
-- >>> import GeniusYield.Imports

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------

newtype GYValidator v = GYValidator (GYScript v)
  deriving (Eq, Ord, Show)

deriving newtype instance GEq GYValidator
deriving newtype instance GCompare GYValidator

instance GShow GYValidator where
  gshowsPrec = showsPrec

validatorFromPlutus :: forall v. SingPlutusVersionI v => PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()) -> GYValidator v
validatorFromPlutus = coerce (scriptFromPlutus @v)

validatorFromSerialisedScript :: forall v. SingPlutusVersionI v => Plutus.SerialisedScript -> GYValidator v
validatorFromSerialisedScript = coerce . scriptFromSerialisedScript

validatorToSerialisedScript :: GYValidator v -> Plutus.SerialisedScript
validatorToSerialisedScript = coerce >>> scriptToSerialisedScript >>> coerce

validatorToScript :: GYValidator v -> GYScript v
validatorToScript = coerce

validatorToApi :: GYValidator v -> Api.PlutusScript (PlutusVersionToApi v)
validatorToApi = coerce scriptToApi

validatorFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYValidator v
validatorFromApi = coerce (scriptFromApi @v)

validatorHash :: GYValidator v -> GYValidatorHash
validatorHash = coerce scriptApiHash

validatorPlutusHash :: GYValidator v -> PlutusV1.ScriptHash
validatorPlutusHash = coerce scriptPlutusHash

validatorApiHash :: GYValidator v -> Api.ScriptHash
validatorApiHash = coerce scriptApiHash

validatorVersion :: GYValidator v -> SingPlutusVersion v
validatorVersion = coerce scriptVersion

validatorToApiPlutusScriptWitness
    :: GYValidator v
    -> Api.ScriptDatum Api.WitCtxTxIn
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness Api.WitCtxTxIn Api.BabbageEra
validatorToApiPlutusScriptWitness (GYValidator s) =
    scriptToApiPlutusScriptWitness s

-- | Writes a validator to a file.
--
writeValidator :: FilePath -> GYValidator v -> IO ()
writeValidator file = writeScriptCore "Validator" file . coerce

-- | Reads a validator from a file.
--
readValidator :: SingPlutusVersionI v => FilePath -> IO (GYValidator v)
readValidator = coerce readScript

newtype GYValidatorHash = GYValidatorHash Api.ScriptHash
  deriving stock (Show, Eq, Ord)

-- |
--
-- >>> "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYValidatorHash
-- GYValidatorHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
--
instance IsString GYValidatorHash where
    fromString = GYValidatorHash . fromString

-- |
--
-- >>> printf "%s" ("cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYValidatorHash)
-- cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0
--
instance Printf.PrintfArg GYValidatorHash where
    formatArg (GYValidatorHash h) = formatArg $ init $ tail $ show h

validatorHashToPlutus :: GYValidatorHash -> PlutusV1.ScriptHash
validatorHashToPlutus = apiHashToPlutus . validatorHashToApi

validatorHashToApi :: GYValidatorHash -> Api.ScriptHash
validatorHashToApi = coerce

validatorHashFromApi :: Api.ScriptHash -> GYValidatorHash
validatorHashFromApi = coerce

-- |
--
-- >>> validatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
-- Right (GYValidatorHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0")
--
-- >>> validatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7"
-- Left (DeserialiseRawBytesError {ptceTag = "validatorHashFromPlutus: cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7"})
--
validatorHashFromPlutus :: PlutusV1.ScriptHash -> Either PlutusToCardanoError GYValidatorHash
validatorHashFromPlutus vh@(PlutusV1.ScriptHash ibs) =
    bimap
        (\e -> DeserialiseRawBytesError $ Text.pack $ "validatorHashFromPlutus: " <> show vh <> ", error: " <> show e)
        validatorHashFromApi
    $ Api.deserialiseFromRawBytes Api.AsScriptHash $ PlutusTx.fromBuiltin ibs

-------------------------------------------------------------------------------
-- Minting Policy
-------------------------------------------------------------------------------

newtype GYMintingPolicy v = GYMintingPolicy (GYScript v)
  deriving stock (Eq, Ord, Show)

deriving newtype instance GEq GYMintingPolicy
deriving newtype instance GCompare GYMintingPolicy

instance GShow GYMintingPolicy where
  gshowsPrec = showsPrec

mintingPolicyVersion :: GYMintingPolicy v -> SingPlutusVersion v
mintingPolicyVersion = coerce scriptVersion

mintingPolicyVersionFromWitness :: GYMintScript v -> PlutusVersion
mintingPolicyVersionFromWitness (GYMintScript mp) = fromSingPlutusVersion $ mintingPolicyVersion mp
mintingPolicyVersionFromWitness (GYMintReference _ s) = fromSingPlutusVersion $ mintingPolicyVersion $ coerce s

mintingPolicyId :: GYMintingPolicy v -> GYMintingPolicyId
mintingPolicyId = coerce scriptApiHash

mintingPolicyIdFromWitness :: GYMintScript v -> GYMintingPolicyId
mintingPolicyIdFromWitness (GYMintScript p)      = mintingPolicyId p
mintingPolicyIdFromWitness (GYMintReference _ s) = mintingPolicyId $ coerce s

mintingPolicyFromPlutus :: forall v. SingPlutusVersionI v => PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()) -> GYMintingPolicy v
mintingPolicyFromPlutus = coerce (scriptFromPlutus @v)

mintingPolicyFromSerialisedScript :: forall v. SingPlutusVersionI v => Plutus.SerialisedScript -> GYMintingPolicy v
mintingPolicyFromSerialisedScript = coerce . scriptFromSerialisedScript

mintingPolicyToSerialisedScript :: GYMintingPolicy v -> Plutus.SerialisedScript
mintingPolicyToSerialisedScript = coerce >>> scriptToSerialisedScript >>> coerce

mintingPolicyToScript :: GYMintingPolicy v -> GYScript v
mintingPolicyToScript = coerce

mintingPolicyToApi :: GYMintingPolicy v -> Api.PlutusScript (PlutusVersionToApi v)
mintingPolicyToApi = coerce scriptToApi

mintingPolicyFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYMintingPolicy v
mintingPolicyFromApi = coerce (scriptFromApi @v)

mintingPolicyCurrencySymbol :: GYMintingPolicy v -> PlutusV1.CurrencySymbol
mintingPolicyCurrencySymbol = coerce scriptPlutusHash

mintingPolicyApiId :: GYMintingPolicy v -> Api.PolicyId
mintingPolicyApiId = coerce . mintingPolicyId

mintingPolicyApiIdFromWitness :: GYMintScript v -> Api.PolicyId
mintingPolicyApiIdFromWitness = coerce . mintingPolicyIdFromWitness

mintingPolicyToApiPlutusScriptWitness
    :: GYMintingPolicy v
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness Api.WitCtxMint Api.BabbageEra
mintingPolicyToApiPlutusScriptWitness (GYMintingPolicy s) =
    scriptToApiPlutusScriptWitness s Api.NoScriptDatumForMint

data GYMintScript (u :: PlutusVersion) where
    -- | 'VersionIsGreaterOrEqual' restricts which version scripts can be used in this transaction.
    GYMintScript    :: v `VersionIsGreaterOrEqual` u => GYMintingPolicy v -> GYMintScript u

    -- | Reference inputs can be only used in V2 transactions.
    GYMintReference :: !GYTxOutRef -> !(GYScript 'PlutusV2) -> GYMintScript 'PlutusV2

deriving instance Show (GYMintScript v)

instance Eq (GYMintScript v) where
    GYMintReference r s == GYMintReference r' s' = r == r' && s == s'
    GYMintScript p == GYMintScript p'            = defaultEq p p'
    _ == _                                       = False

instance Ord (GYMintScript v) where
    GYMintReference r s `compare` GYMintReference r' s' = compare r r' <> compare s s'
    GYMintReference _ _ `compare` _ = LT
    GYMintScript p `compare` GYMintScript p' = defaultCompare p p'
    GYMintScript _ `compare` _ = GT

gyMintScriptToSerialisedScript :: GYMintScript u -> Plutus.SerialisedScript
gyMintScriptToSerialisedScript (GYMintScript mp) = coerce mp & scriptToSerialisedScript & coerce
gyMintScriptToSerialisedScript (GYMintReference _ s) = scriptToSerialisedScript s & coerce

gyMintingScriptWitnessToApiPlutusSW
  :: GYMintScript u
  -> Api.S.ScriptRedeemer
  -> Api.S.ExecutionUnits
  -> Api.S.ScriptWitness Api.S.WitCtxMint Api.S.BabbageEra
gyMintingScriptWitnessToApiPlutusSW (GYMintScript p) = mintingPolicyToApiPlutusScriptWitness p
gyMintingScriptWitnessToApiPlutusSW (GYMintReference r s) =
    referenceScriptToApiPlutusScriptWitness r s
    Api.NoScriptDatumForMint

-- | Writes a minting policy to a file.
--
writeMintingPolicy :: FilePath -> GYMintingPolicy v -> IO ()
writeMintingPolicy file = writeScriptCore "Minting Policy" file . coerce

-- | Reads a minting policy from a file.
--
readMintingPolicy :: SingPlutusVersionI v => FilePath -> IO (GYMintingPolicy v)
readMintingPolicy = coerce readScript

-- | Minting policy identifier, also a currency symbol.
newtype GYMintingPolicyId = GYMintingPolicyId Api.PolicyId
  deriving stock   (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance ToJSONKey GYMintingPolicyId where
    toJSONKey = toJSONKeyText mintingPolicyIdToText

instance FromJSONKey GYMintingPolicyId where
    fromJSONKey = FromJSONKeyTextParser (either fail pure . mintingPolicyIdFromText)

-- |
--
-- >>> fromString "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" :: GYMintingPolicyId
-- "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
--
instance IsString GYMintingPolicyId where
    fromString = GYMintingPolicyId . fromString

instance Show GYMintingPolicyId where
    showsPrec d (GYMintingPolicyId s) = showsPrec d s

instance Web.FromHttpApiData GYMintingPolicyId where
  parseUrlPiece = first Text.pack . Atto.parseOnly parser . TE.encodeUtf8
    where
      parser :: Atto.Parser GYMintingPolicyId
      parser  = do
        cs <- Atto.takeWhile1 isHexDigit

        case Api.deserialiseFromRawBytesHex Api.AsPolicyId cs of
          Left x    -> fail $ "Invalid currency symbol: " ++ show cs ++ "; Reason: " ++ show x
          Right cs' -> return $ mintingPolicyIdFromApi cs'

instance Swagger.ToParamSchema GYMintingPolicyId where
  toParamSchema _ = mempty
      & Swagger.type_           ?~ Swagger.SwaggerString
      & Swagger.format          ?~ "hex"
      & Swagger.maxLength       ?~ 56
      & Swagger.minLength       ?~ 56

instance Swagger.ToSchema GYMintingPolicyId where
  declareNamedSchema _ = pure $ Swagger.named "GYMintingPolicyId" $ Swagger.paramSchemaToSchema (Proxy @GYMintingPolicyId)
                       & Swagger.description     ?~ "This is the hash of a minting policy script."
                       & Swagger.example         ?~ toJSON ("ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" :: Text)

mintingPolicyIdToApi :: GYMintingPolicyId -> Api.PolicyId
mintingPolicyIdToApi = coerce

mintingPolicyIdFromApi :: Api.PolicyId -> GYMintingPolicyId
mintingPolicyIdFromApi = coerce

mintingPolicyIdCurrencySymbol :: GYMintingPolicyId -> PlutusV1.CurrencySymbol
mintingPolicyIdCurrencySymbol = coerce $ PlutusTx.toBuiltin . Api.serialiseToRawBytes @Api.PolicyId

mintingPolicyIdToText :: GYMintingPolicyId -> Text
mintingPolicyIdToText = Api.serialiseToRawBytesHexText . Api.unPolicyId . mintingPolicyIdToApi

mintingPolicyIdFromText :: Text -> Either String GYMintingPolicyId
mintingPolicyIdFromText policyid = bimap customError mintingPolicyIdFromApi
    . Api.deserialiseFromRawBytesHex Api.S.AsPolicyId
    $ TE.encodeUtf8 policyid
  where
    customError err = "Invalid minting policy: " ++ show policyid ++ "; Reason: " ++ show err

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

-- | Plutus script
data GYScript (v :: PlutusVersion) = GYScript
    !(SingPlutusVersion v)
    !(Api.PlutusScript (PlutusVersionToApi v))
    !Api.ScriptHash

-- | Equality and comparison are on script hash.
--
-- As hash is cryptographicly strong, and 'GYScript' constructor is not
-- exposed, this works great.
--
instance Eq (GYScript v) where
    (==) = defaultEq

instance Ord (GYScript v) where
    compare = defaultCompare

instance Show (GYScript v) where
    showsPrec d (GYScript _ _ h) = showParen (d > 10)
        $ showString "GYScript "
        . showsPrec 11 h

instance GEq GYScript where
    geq (GYScript v1 _ h1) (GYScript v2 _ h2) = do
        Refl <- geq v1 v2
        guard (h1 == h2)
        return Refl

instance GCompare GYScript where
    gcompare (GYScript v1 _ h1) (GYScript v2 _ h2) = case gcompare v1 v2 of
        GEQ -> case compare h1 h2 of
          EQ -> GEQ
          LT -> GLT
          GT -> GGT
        GLT -> GLT
        GGT -> GGT

instance GShow GYScript where
    gshowsPrec = showsPrec

-- In implementation we cache the api representation and hashes.

scriptFromPlutus :: forall v a. SingPlutusVersionI v => PlutusTx.CompiledCode a -> GYScript v
scriptFromPlutus script = scriptFromApi $ Api.S.PlutusScriptSerialised $ Plutus.serialiseCompiledCode script

scriptFromSerialisedScript :: forall v. SingPlutusVersionI v => Plutus.SerialisedScript -> GYScript v
scriptFromSerialisedScript serialisedScript =
  scriptFromApi $ Api.S.PlutusScriptSerialised @(PlutusVersionToApi v) serialisedScript

scriptToSerialisedScript :: GYScript v -> ShortByteString
scriptToSerialisedScript script = scriptToApi script & \case
  (Api.S.PlutusScriptSerialised s) -> s

scriptVersion :: GYScript v -> SingPlutusVersion v
scriptVersion (GYScript v _ _) = v

scriptToApi :: GYScript v -> Api.PlutusScript (PlutusVersionToApi v)
scriptToApi (GYScript _ api _) = api

someScriptToReferenceApi :: Some GYScript -> Api.S.ReferenceScript Api.S.BabbageEra
someScriptToReferenceApi (Some (GYScript v apiScript _)) =
    Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra $
    Api.ScriptInAnyLang (Api.PlutusScriptLanguage v') $
    Api.PlutusScript v' apiScript
  where
    v' = singPlutusVersionToApi v

-- |
--
-- /Note/: Simple scripts are converted to 'Nothing'.
someScriptFromReferenceApi :: Api.S.ReferenceScript era -> Maybe (Some GYScript)
someScriptFromReferenceApi Api.S.ReferenceScriptNone = Nothing
someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang Api.SimpleScriptLanguage _)) = Nothing
someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang (Api.PlutusScriptLanguage Api.PlutusScriptV1) (Api.PlutusScript _ x))) = Just (Some y)
  where
    y :: GYScript 'PlutusV1
    y = scriptFromApi x

someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang (Api.PlutusScriptLanguage Api.PlutusScriptV2) (Api.PlutusScript _ x))) = Just (Some y)
  where
    y :: GYScript 'PlutusV2
    y = scriptFromApi x

scriptFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYScript v
scriptFromApi script = GYScript v script apiHash
  where
    v = singPlutusVersion @v
    apiScript :: Api.S.Script (PlutusVersionToApi v)
    apiScript = Api.PlutusScript (singPlutusVersionToApi v) script
    apiHash = Api.hashScript apiScript

scriptFromCBOR :: forall v. SingPlutusVersionI v => Text -> Maybe (GYScript v)
scriptFromCBOR = scriptFromCBOR' . encodeUtf8

scriptFromCBOR' :: forall v. SingPlutusVersionI v => ByteString -> Maybe (GYScript v)
scriptFromCBOR' b = do
    bs <- rightToMaybe (BS16.decode b)
    case singPlutusVersion @v of
      SingPlutusV1 -> fmap scriptFromApi $ rightToMaybe $ flip Api.deserialiseFromRawBytes bs $ Api.AsPlutusScript $ Api.proxyToAsType $ Proxy @(PlutusVersionToApi v)
      SingPlutusV2 -> fmap scriptFromApi $ rightToMaybe $ flip Api.deserialiseFromRawBytes bs $ Api.AsPlutusScript $ Api.proxyToAsType $ Proxy @(PlutusVersionToApi v)

scriptPlutusHash :: GYScript v -> PlutusV1.ScriptHash
scriptPlutusHash = apiHashToPlutus . scriptApiHash

apiHashToPlutus :: Api.ScriptHash -> PlutusV1.ScriptHash
apiHashToPlutus h = PlutusV1.ScriptHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes h

someScriptPlutusHash :: Some GYScript -> PlutusV1.ScriptHash
someScriptPlutusHash (Some s) = scriptPlutusHash s

scriptApiHash :: GYScript v -> Api.ScriptHash
scriptApiHash (GYScript _ _ ah) = ah

scriptToApiPlutusScriptWitness
    :: GYScript v
    -> Api.S.ScriptDatum ctx
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness ctx Api.BabbageEra
scriptToApiPlutusScriptWitness (GYScript v api _) = case v of
    SingPlutusV1 -> Api.PlutusScriptWitness
        Api.PlutusScriptV1InBabbage
        Api.PlutusScriptV1
        (Api.S.PScript api)
    SingPlutusV2 -> Api.PlutusScriptWitness
        Api.PlutusScriptV2InBabbage
        Api.PlutusScriptV2
        (Api.S.PScript api)

referenceScriptToApiPlutusScriptWitness
  :: GYTxOutRef
  -> GYScript 'PlutusV2
  -> Api.S.ScriptDatum witctx
  -> Api.S.ScriptRedeemer
  -> Api.S.ExecutionUnits
  -> Api.S.ScriptWitness witctx Api.S.BabbageEra
referenceScriptToApiPlutusScriptWitness r s =
    Api.PlutusScriptWitness
    Api.PlutusScriptV2InBabbage
    Api.PlutusScriptV2
    (Api.S.PReferenceScript (txOutRefToApi r) (Just (scriptApiHash s)))

-- | Writes a script to a file.
--
writeScript :: forall v. FilePath -> GYScript v -> IO ()
writeScript = writeScriptCore "Script"

-- | Reads a script from a file.
--
readScript :: forall v. SingPlutusVersionI v => FilePath -> IO (GYScript v)
readScript file = case singPlutusVersion @v of
    SingPlutusV1 -> do
        e <- Api.readFileTextEnvelope (Api.AsPlutusScript Api.AsPlutusScriptV1) (Api.File file)
        case e of
            Left (err :: Api.FileError Api.TextEnvelopeError) -> throwIO $ userError $ show err
            Right s                                           -> return $ scriptFromApi s

    SingPlutusV2 -> do
        e <- Api.readFileTextEnvelope (Api.AsPlutusScript Api.AsPlutusScriptV2) (Api.File file)
        case e of
            Left (err :: Api.FileError Api.TextEnvelopeError) -> throwIO $ userError $ show err
            Right s                                           -> return $ scriptFromApi s

writeScriptCore :: forall v. Api.S.TextEnvelopeDescr -> FilePath -> GYScript v -> IO ()
writeScriptCore desc file s = do
    e <- case scriptVersion @v s of
        SingPlutusV1 -> Api.writeFileTextEnvelope (Api.File file) (Just desc) $ scriptToApi s
        SingPlutusV2 -> Api.writeFileTextEnvelope (Api.File file) (Just desc) $ scriptToApi s
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()
