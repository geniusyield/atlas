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
    validatorToPlutus,
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
    mintingPolicyFromPlutus,
    mintingPolicyToPlutus,
    mintingPolicyToApi,
    mintingPolicyIdToText,
    mintingPolicyIdFromText,
    mintingPolicyFromApi,
    mintingPolicyToApiPlutusScriptWitness,

    -- ** File operations
    writeMintingPolicy,
    readMintingPolicy,

    -- ** Selectors
    mintingPolicyCurrencySymbol,
    mintingPolicyApiId,

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
    scriptToPlutus,
    scriptFromPlutus,
    someScriptToPlutus,
    scriptApiHash,
    scriptPlutusHash,
    someScriptPlutusHash,
    someScriptToReferenceApi,
    someScriptFromReferenceApi,

    -- ** File operations
    writeScript,
    readScript,
) where

import           Data.GADT.Compare
import           Data.GADT.Show

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S
import qualified Codec.Serialise
import           Control.Lens                     ((?~))
import           Data.Aeson.Types                 (ToJSONKey (toJSONKey), FromJSONKey (fromJSONKey), FromJSONKeyFunction (FromJSONKeyTextParser), toJSONKeyText)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Base16           as BS16
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Short            as SBS
import qualified Data.Swagger                     as Swagger
import qualified Data.Swagger.Internal.Schema     as Swagger
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as TE
import qualified Plutus.V1.Ledger.Api             as Plutus
import qualified Plutus.V1.Ledger.Scripts         as Plutus
import qualified PlutusTx.Builtins.Internal       as Plutus
import qualified Text.Printf                      as Printf
import qualified Web.HttpApiData                  as Web

import           Data.ByteString                  (ByteString)
import           GeniusYield.Imports
import           GeniusYield.Types.Ledger         (PlutusToCardanoError (..))
import           GeniusYield.Types.PlutusVersion

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

validatorFromPlutus :: forall v. SingPlutusVersionI v => Plutus.Validator -> GYValidator v
validatorFromPlutus = coerce (scriptFromPlutus @v) -- TODO: generalise #28
                                                   --       (https://github.com/geniusyield/atlas/issues/28)
validatorToPlutus :: GYValidator v  -> Plutus.Validator
validatorToPlutus = coerce scriptToPlutus

validatorToScript :: GYValidator v -> GYScript v
validatorToScript = coerce

validatorToApi :: GYValidator v -> Api.PlutusScript (PlutusVersionToApi v)
validatorToApi = coerce scriptToApi

validatorFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYValidator v
validatorFromApi = coerce (scriptFromApi @v)

validatorHash :: GYValidator v -> GYValidatorHash
validatorHash = coerce scriptApiHash

validatorPlutusHash :: GYValidator v -> Plutus.ValidatorHash
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

validatorHashToPlutus :: GYValidatorHash -> Plutus.ValidatorHash
validatorHashToPlutus (GYValidatorHash h) =
    Plutus.ValidatorHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes h

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
validatorHashFromPlutus :: Plutus.ValidatorHash -> Either PlutusToCardanoError GYValidatorHash
validatorHashFromPlutus vh@(Plutus.ValidatorHash (Plutus.BuiltinByteString bs)) =
    maybe
        (Left $ DeserialiseRawBytesError $ Text.pack $ "validatorHashFromPlutus: " ++ show vh)
        (Right . coerce)
    $ Api.deserialiseFromRawBytes Api.AsScriptHash bs

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


mintingPolicyId :: GYMintingPolicy v -> GYMintingPolicyId
mintingPolicyId = coerce scriptApiHash

mintingPolicyFromPlutus :: forall v. SingPlutusVersionI v => Plutus.MintingPolicy -> GYMintingPolicy v
mintingPolicyFromPlutus = coerce (scriptFromPlutus @v)

mintingPolicyToPlutus :: GYMintingPolicy v -> Plutus.MintingPolicy
mintingPolicyToPlutus = coerce scriptToPlutus

mintingPolicyToScript :: GYMintingPolicy v -> GYScript v
mintingPolicyToScript = coerce

mintingPolicyToApi :: GYMintingPolicy v -> Api.PlutusScript (PlutusVersionToApi v)
mintingPolicyToApi = coerce scriptToApi

mintingPolicyFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYMintingPolicy v
mintingPolicyFromApi = coerce (scriptFromApi @v)

mintingPolicyCurrencySymbol :: GYMintingPolicy v -> Plutus.CurrencySymbol
mintingPolicyCurrencySymbol = coerce scriptPlutusHash

mintingPolicyApiId :: GYMintingPolicy v -> Api.PolicyId
mintingPolicyApiId = coerce scriptApiHash

mintingPolicyToApiPlutusScriptWitness
    :: GYMintingPolicy v
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness Api.WitCtxMint Api.BabbageEra
mintingPolicyToApiPlutusScriptWitness (GYMintingPolicy s) =
    scriptToApiPlutusScriptWitness s Api.NoScriptDatumForMint

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

mintingPolicyIdCurrencySymbol :: GYMintingPolicyId -> Plutus.CurrencySymbol
mintingPolicyIdCurrencySymbol = coerce $ Plutus.BuiltinByteString . Api.serialiseToRawBytes @Api.PolicyId

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
    !Plutus.Script
    !(Api.PlutusScript (PlutusVersionToApi v))
    !Plutus.ScriptHash
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
    showsPrec d (GYScript _ _ _ _ h) = showParen (d > 10)
        $ showString "GYScript "
        . showsPrec 11 h

instance GEq GYScript where
    geq (GYScript v1 _ _ _ h1) (GYScript v2 _ _ _ h2) = do
        Refl <- geq v1 v2
        guard (h1 == h2)
        return Refl

instance GCompare GYScript where
    gcompare (GYScript v1 _ _ _ h1) (GYScript v2 _ _ _ h2) = case gcompare v1 v2 of
        GEQ -> case compare h1 h2 of
          EQ -> GEQ
          LT -> GLT
          GT -> GGT
        GLT -> GLT
        GGT -> GGT

instance GShow GYScript where
    gshowsPrec = showsPrec

-- In implementation we cache the api representation and hashes.

scriptFromPlutus :: forall v. SingPlutusVersionI v => Plutus.Script -> GYScript v
scriptFromPlutus script =
    GYScript v script apiPlutusScript plutusHash apiHash
  where
    -- implementation: Ledger.Scripts.toCardanoApiScript script
    apiPlutusScript
        = Api.S.PlutusScriptSerialised
        $ SBS.toShort
        $ BSL.toStrict
        $ Codec.Serialise.serialise script

    v = singPlutusVersion @v

    apiScript :: Api.S.Script (PlutusVersionToApi v)
    apiScript  = Api.PlutusScript (singPlutusVersionToApi v) apiPlutusScript
    apiHash    = Api.hashScript apiScript
    plutusHash = coerce $ Plutus.BuiltinByteString $ Api.serialiseToRawBytes apiHash

scriptVersion :: GYScript v -> SingPlutusVersion v
scriptVersion (GYScript v _ _ _ _) = v

scriptToPlutus :: GYScript v -> Plutus.Script
scriptToPlutus (GYScript _ p _ _ _) = p

someScriptToPlutus :: Some GYScript -> Plutus.Script
someScriptToPlutus (Some s) = scriptToPlutus s

scriptToApi :: GYScript v -> Api.PlutusScript (PlutusVersionToApi v)
scriptToApi (GYScript _ _ api _ _) = api

someScriptToReferenceApi :: Some GYScript -> Api.S.ReferenceScript Api.S.BabbageEra
someScriptToReferenceApi (Some (GYScript v _ apiScript _ _)) =
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
someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang (Api.SimpleScriptLanguage _) _)) = Nothing
someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang (Api.PlutusScriptLanguage Api.PlutusScriptV1) (Api.PlutusScript _ x))) = Just (Some y)
  where
    y :: GYScript 'PlutusV1
    y = scriptFromApi x

someScriptFromReferenceApi (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Api.ScriptInAnyLang (Api.PlutusScriptLanguage Api.PlutusScriptV2) (Api.PlutusScript _ x))) = Just (Some y)
  where
    y :: GYScript 'PlutusV2
    y = scriptFromApi x

scriptFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYScript v
scriptFromApi = scriptFromPlutus . Codec.Serialise.deserialise . BSL.fromStrict . with Api.serialiseToRawBytes
  where
    with :: (Api.S.HasTypeProxy (PlutusVersionToApi v) => r) -> r
    with k = case singPlutusVersion @v of
        SingPlutusV1 -> k
        SingPlutusV2 -> k

-- TODO: Can be defined in terms of `scriptFromCBOR'`.
scriptFromCBOR :: forall v. SingPlutusVersionI v => Text -> Maybe (GYScript v)
scriptFromCBOR t = do
    bs <- BSL.fromStrict <$> rightToMaybe (BS16.decode $ encodeUtf8 t)
    scriptFromPlutus @v <$> rightToMaybe (Codec.Serialise.deserialiseOrFail @Plutus.Script bs)

scriptFromCBOR' :: forall v. SingPlutusVersionI v => ByteString -> Maybe (GYScript v)
scriptFromCBOR' b = do
    bs <- BSL.fromStrict <$> rightToMaybe (BS16.decode b)
    scriptFromPlutus @v <$> rightToMaybe (Codec.Serialise.deserialiseOrFail @Plutus.Script bs)

scriptPlutusHash :: GYScript v -> Plutus.ScriptHash
scriptPlutusHash (GYScript _ _ _ ph _) = ph

someScriptPlutusHash :: Some GYScript -> Plutus.ScriptHash
someScriptPlutusHash (Some s) = scriptPlutusHash s

scriptApiHash :: GYScript v -> Api.ScriptHash
scriptApiHash (GYScript _ _ _ _ ah) = ah

scriptToApiPlutusScriptWitness
    :: GYScript v
    -> Api.S.ScriptDatum ctx
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness ctx Api.BabbageEra
scriptToApiPlutusScriptWitness (GYScript v _ api _ _) = case v of
    SingPlutusV1 -> Api.PlutusScriptWitness
        Api.PlutusScriptV1InBabbage
        Api.PlutusScriptV1
        (Api.S.PScript api)
    SingPlutusV2 -> Api.PlutusScriptWitness
        Api.PlutusScriptV2InBabbage
        Api.PlutusScriptV2
        (Api.S.PScript api)

-- | Writes a script to a file.
--
writeScript :: forall v. FilePath -> GYScript v -> IO ()
writeScript = writeScriptCore "Script"

-- | Reads a script from a file.
--
readScript :: forall v. SingPlutusVersionI v => FilePath -> IO (GYScript v)
readScript file = case singPlutusVersion @v of
    SingPlutusV1 -> do
        e <- Api.readFileTextEnvelope (Api.AsPlutusScript Api.AsPlutusScriptV1) file
        case e of
            Left (err :: Api.FileError Api.TextEnvelopeError) -> throwIO $ userError $ show err
            Right s                                           -> return $ scriptFromApi s

    SingPlutusV2 -> do
        e <- Api.readFileTextEnvelope (Api.AsPlutusScript Api.AsPlutusScriptV2) file
        case e of
            Left (err :: Api.FileError Api.TextEnvelopeError) -> throwIO $ userError $ show err
            Right s                                           -> return $ scriptFromApi s

writeScriptCore :: forall v. Api.S.TextEnvelopeDescr -> FilePath -> GYScript v -> IO ()
writeScriptCore desc file s = do
    e <- case scriptVersion @v s of
        SingPlutusV1 -> Api.writeFileTextEnvelope file (Just desc) $ scriptToApi s
        SingPlutusV2 -> Api.writeFileTextEnvelope file (Just desc) $ scriptToApi s
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()
