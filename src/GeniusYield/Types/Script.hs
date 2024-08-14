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

    -- * ScriptHash
    GYScriptHash,
    scriptHashFromApi,
    scriptHashToApi,
    scriptHashFromLedger,
    scriptHashToLedger,
    scriptHashToPlutus,

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
    mintingPolicyIdToCurrencySymbol,
    mintingPolicyIdFromCurrencySymbol,
    mintingPolicyIdCurrencySymbol,

    -- * StakeValidator
    GYStakeValidator,
    stakeValidatorVersion,
    stakeValidatorVersionFromWitness,
    stakeValidatorFromPlutus,
    stakeValidatorFromSerialisedScript,
    stakeValidatorToSerialisedScript,
    stakeValidatorToApi,
    stakeValidatorFromApi,
    stakeValidatorToApiPlutusScriptWitness,

    -- * Witness for stake validator
    GYStakeValScript (..),
    gyStakeValScriptToSerialisedScript,
    gyStakeValScriptWitnessToApiPlutusSW,

    -- ** Stake validator selectors
    stakeValidatorHash,
    stakeValidatorPlutusHash,
    stakeValidatorApiHash,

    -- * StakeValidatorHash
    GYStakeValidatorHash,
    stakeValidatorHashToApi,
    stakeValidatorHashToPlutus,
    stakeValidatorHashFromApi,
    stakeValidatorHashFromPlutus,

    -- ** File operations
    writeStakeValidator,
    readStakeValidator,

    -- * Script
    GYScript,
    scriptVersion,
    validatorToScript,
    mintingPolicyToScript,
    stakeValidatorToScript,
    scriptToApi,
    scriptToApiScript,
    scriptToApiScriptInEra,
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
    apiHashToPlutus,
    scriptSize,

    -- ** File operations
    writeScript,
    readScript,

    -- * Any Script
    GYAnyScript (..),

    -- * Simple Script
    module SimpleScript
) where

import qualified Cardano.Api                           as Api
import qualified Cardano.Api.Script                    as Api
import qualified Cardano.Api.Shelley                   as Api.S
import           Cardano.Ledger.SafeHash               (SafeToHash (originalBytesSize))
import           Control.Lens                          ((?~))
import           Data.Aeson.Types                      (FromJSONKey (fromJSONKey),
                                                        FromJSONKeyFunction (FromJSONKeyTextParser),
                                                        ToJSONKey (toJSONKey),
                                                        toJSONKeyText)
import qualified Data.Attoparsec.ByteString.Char8      as Atto
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Base16                as BS16
import           Data.ByteString.Short                 (ShortByteString)
import           Data.GADT.Compare
import           Data.GADT.Show
import qualified Data.Swagger                          as Swagger
import qualified Data.Swagger.Internal.Schema          as Swagger
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as TE
import           GeniusYield.Imports
import           GeniusYield.Types.Era                 (ApiEra)
import           GeniusYield.Types.Ledger              (PlutusToCardanoError (..))
import           GeniusYield.Types.PlutusVersion
import           GeniusYield.Types.Script.ScriptHash
import           GeniusYield.Types.Script.SimpleScript as SimpleScript
import           GeniusYield.Types.TxOutRef            (GYTxOutRef,
                                                        txOutRefToApi)
import qualified PlutusLedgerApi.Common                as Plutus
import qualified PlutusLedgerApi.V1                    as PlutusV1
import qualified PlutusTx
import qualified PlutusTx.Builtins                     as PlutusTx
import qualified Text.Printf                           as Printf
import qualified Web.HttpApiData                       as Web

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
    -> Api.ScriptWitness Api.WitCtxTxIn ApiEra
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
-- Left (DeserialiseRawBytesError {ptceTag = "validatorHashFromPlutus: cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7, error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise ScriptHash\"}"})
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
    -> Api.ScriptWitness Api.WitCtxMint ApiEra
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
  -> Api.S.ScriptWitness Api.S.WitCtxMint Api.S.ConwayEra
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

instance Web.ToHttpApiData GYMintingPolicyId where
  toUrlPiece = mintingPolicyIdToText

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

{-# DEPRECATED mintingPolicyIdCurrencySymbol "Use mintingPolicyIdToCurrencySymbol." #-}
mintingPolicyIdCurrencySymbol :: GYMintingPolicyId -> PlutusV1.CurrencySymbol
mintingPolicyIdCurrencySymbol = coerce $ PlutusTx.toBuiltin . Api.serialiseToRawBytes @Api.PolicyId

mintingPolicyIdToCurrencySymbol :: GYMintingPolicyId -> PlutusV1.CurrencySymbol
mintingPolicyIdToCurrencySymbol = mintingPolicyIdCurrencySymbol

-- |
--
-- >>> mintingPolicyIdFromCurrencySymbol $ mintingPolicyIdToCurrencySymbol "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
-- Right "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
--
mintingPolicyIdFromCurrencySymbol :: PlutusV1.CurrencySymbol -> Either PlutusToCardanoError GYMintingPolicyId
mintingPolicyIdFromCurrencySymbol cs =
    bimap
        (\e -> DeserialiseRawBytesError $ Text.pack $ "validatorHashFromPlutus: " <> show cs <> ", error: " <> show e)
        mintingPolicyIdFromApi
    $ Api.deserialiseFromRawBytes Api.AsPolicyId $ PlutusTx.fromBuiltin $ PlutusV1.unCurrencySymbol cs

mintingPolicyIdToText :: GYMintingPolicyId -> Text
mintingPolicyIdToText = Api.serialiseToRawBytesHexText . Api.unPolicyId . mintingPolicyIdToApi

mintingPolicyIdFromText :: Text -> Either String GYMintingPolicyId
mintingPolicyIdFromText policyid = bimap customError mintingPolicyIdFromApi
    . Api.deserialiseFromRawBytesHex Api.S.AsPolicyId
    $ TE.encodeUtf8 policyid
  where
    customError err = "Invalid minting policy: " ++ show policyid ++ "; Reason: " ++ show err

-------------------------------------------------------------------------------
-- Stake validator
-------------------------------------------------------------------------------

newtype GYStakeValidator v = GYStakeValidator (GYScript v)
  deriving stock (Eq, Ord, Show)

deriving newtype instance GEq GYStakeValidator
deriving newtype instance GCompare GYStakeValidator

instance GShow GYStakeValidator where
  gshowsPrec = showsPrec

stakeValidatorVersion :: GYStakeValidator v -> SingPlutusVersion v
stakeValidatorVersion = coerce scriptVersion

stakeValidatorVersionFromWitness :: GYStakeValScript v -> PlutusVersion
stakeValidatorVersionFromWitness (GYStakeValScript mp) = fromSingPlutusVersion $ stakeValidatorVersion mp
stakeValidatorVersionFromWitness (GYStakeValReference _ s) = fromSingPlutusVersion $ stakeValidatorVersion $ coerce s

stakeValidatorFromPlutus :: forall v. SingPlutusVersionI v => PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()) -> GYStakeValidator v
stakeValidatorFromPlutus = coerce (scriptFromPlutus @v)

stakeValidatorFromSerialisedScript :: forall v. SingPlutusVersionI v => Plutus.SerialisedScript -> GYStakeValidator v
stakeValidatorFromSerialisedScript = coerce . scriptFromSerialisedScript

stakeValidatorToSerialisedScript :: GYStakeValidator v -> Plutus.SerialisedScript
stakeValidatorToSerialisedScript = coerce >>> scriptToSerialisedScript >>> coerce

stakeValidatorToScript :: GYStakeValidator v -> GYScript v
stakeValidatorToScript = coerce

stakeValidatorToApi :: GYStakeValidator v -> Api.PlutusScript (PlutusVersionToApi v)
stakeValidatorToApi = coerce scriptToApi

stakeValidatorFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYStakeValidator v
stakeValidatorFromApi = coerce (scriptFromApi @v)

stakeValidatorToApiPlutusScriptWitness
    :: GYStakeValidator v
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness Api.WitCtxStake ApiEra
stakeValidatorToApiPlutusScriptWitness (GYStakeValidator s) =
    scriptToApiPlutusScriptWitness s Api.NoScriptDatumForStake

data GYStakeValScript (u :: PlutusVersion) where
    -- | 'VersionIsGreaterOrEqual' restricts which version scripts can be used in this transaction.
    GYStakeValScript    :: v `VersionIsGreaterOrEqual` u => GYStakeValidator v -> GYStakeValScript u

    -- | Reference inputs can be only used in V2 transactions.
    GYStakeValReference :: !GYTxOutRef -> !(GYScript 'PlutusV2) -> GYStakeValScript 'PlutusV2

deriving instance Show (GYStakeValScript v)

instance Eq (GYStakeValScript v) where
    GYStakeValReference r s == GYStakeValReference r' s' = r == r' && s == s'
    GYStakeValScript p == GYStakeValScript p'            = defaultEq p p'
    _ == _                                               = False

instance Ord (GYStakeValScript v) where
    GYStakeValReference r s `compare` GYStakeValReference r' s' = compare r r' <> compare s s'
    GYStakeValReference _ _ `compare` _ = LT
    GYStakeValScript p `compare` GYStakeValScript p' = defaultCompare p p'
    GYStakeValScript _ `compare` _ = GT

gyStakeValScriptToSerialisedScript :: GYStakeValScript u -> Plutus.SerialisedScript
gyStakeValScriptToSerialisedScript (GYStakeValScript mp) = coerce mp & scriptToSerialisedScript & coerce
gyStakeValScriptToSerialisedScript (GYStakeValReference _ s) = scriptToSerialisedScript s & coerce

gyStakeValScriptWitnessToApiPlutusSW
  :: GYStakeValScript u
  -> Api.S.ScriptRedeemer
  -> Api.S.ExecutionUnits
  -> Api.S.ScriptWitness Api.S.WitCtxStake Api.S.ConwayEra
gyStakeValScriptWitnessToApiPlutusSW (GYStakeValScript p) = stakeValidatorToApiPlutusScriptWitness p
gyStakeValScriptWitnessToApiPlutusSW (GYStakeValReference r s) =
    referenceScriptToApiPlutusScriptWitness r s
    Api.NoScriptDatumForStake

stakeValidatorHash :: GYStakeValidator v -> GYStakeValidatorHash
stakeValidatorHash = coerce scriptApiHash

stakeValidatorPlutusHash :: GYStakeValidator v -> PlutusV1.ScriptHash
stakeValidatorPlutusHash = coerce scriptPlutusHash

stakeValidatorApiHash :: GYStakeValidator v -> Api.ScriptHash
stakeValidatorApiHash = coerce scriptApiHash

newtype GYStakeValidatorHash = GYStakeValidatorHash Api.ScriptHash
  deriving stock (Show, Eq, Ord)

-- |
--
-- >>> "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYStakeValidatorHash
-- GYStakeValidatorHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
--
instance IsString GYStakeValidatorHash where
    fromString = GYStakeValidatorHash . fromString

-- |
--
-- >>> printf "%s" ("cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0" :: GYStakeValidatorHash)
-- cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0
--
instance Printf.PrintfArg GYStakeValidatorHash where
    formatArg (GYStakeValidatorHash h) = formatArg $ init $ tail $ show h

stakeValidatorHashToPlutus :: GYStakeValidatorHash -> PlutusV1.ScriptHash
stakeValidatorHashToPlutus = apiHashToPlutus . stakeValidatorHashToApi

stakeValidatorHashToApi :: GYStakeValidatorHash -> Api.ScriptHash
stakeValidatorHashToApi = coerce

stakeValidatorHashFromApi :: Api.ScriptHash -> GYStakeValidatorHash
stakeValidatorHashFromApi = coerce

-- |
--
-- >>> stakeValidatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0"
-- Right (GYStakeValidatorHash "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7d0")
--
-- >>> stakeValidatorHashFromPlutus "cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7"
-- Left (DeserialiseRawBytesError {ptceTag = "stakeValidatorHashFromPlutus: cabdd19b58d4299fde05b53c2c0baf978bf9ade734b490fc0cc8b7, error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise ScriptHash\"}"})
--
stakeValidatorHashFromPlutus :: PlutusV1.ScriptHash -> Either PlutusToCardanoError GYStakeValidatorHash
stakeValidatorHashFromPlutus vh@(PlutusV1.ScriptHash ibs) =
    bimap
        (\e -> DeserialiseRawBytesError $ Text.pack $ "stakeValidatorHashFromPlutus: " <> show vh <> ", error: " <> show e)
        stakeValidatorHashFromApi
    $ Api.deserialiseFromRawBytes Api.AsScriptHash $ PlutusTx.fromBuiltin ibs

-- | Writes a stake validator to a file.
--
writeStakeValidator :: FilePath -> GYStakeValidator v -> IO ()
writeStakeValidator file = writeScriptCore "Stake Validator" file . coerce

-- | Reads a stake validator from a file.
--
readStakeValidator :: SingPlutusVersionI v => FilePath -> IO (GYStakeValidator v)
readStakeValidator = coerce readScript

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

scriptToApiScript :: GYScript v -> Api.Script (PlutusVersionToApi v)
scriptToApiScript (GYScript v api _) = Api.PlutusScript (singPlutusVersionToApi v) api

scriptToApiScriptInEra :: GYScript v -> Api.ScriptInEra Api.S.ConwayEra
scriptToApiScriptInEra s@(GYScript v _ _) = Api.ScriptInEra scriptInLanguageEra (scriptToApiScript s)
  where
    scriptInLanguageEra = case singPlutusVersionToApi v of
      Api.PlutusScriptV1 -> Api.PlutusScriptV1InConway
      Api.PlutusScriptV2 -> Api.PlutusScriptV2InConway
      Api.PlutusScriptV3 -> Api.PlutusScriptV3InConway

-- FIXME: Should we use Conway here?
someScriptToReferenceApi :: Some GYScript -> Api.S.ReferenceScript Api.S.ConwayEra
someScriptToReferenceApi (Some (GYScript v apiScript _)) =
    Api.S.ReferenceScript
      Api.S.BabbageEraOnwardsConway $
      Api.ScriptInAnyLang (Api.PlutusScriptLanguage v') $
        Api.PlutusScript v' apiScript
  where
    v' = singPlutusVersionToApi v

-- |
--
-- /Note/: Simple scripts are converted to 'Nothing'.
someScriptFromReferenceApi :: Api.S.ReferenceScript era -> Maybe (Some GYScript)
someScriptFromReferenceApi Api.S.ReferenceScriptNone = Nothing
someScriptFromReferenceApi
  (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway
    (Api.ScriptInAnyLang Api.SimpleScriptLanguage _)) = Nothing
someScriptFromReferenceApi
  (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway
    (Api.ScriptInAnyLang
      (Api.PlutusScriptLanguage Api.PlutusScriptV1)
      (Api.PlutusScript _ x)
    )
  ) = Just (Some y)
  where
    y :: GYScript 'PlutusV1
    y = scriptFromApi x

someScriptFromReferenceApi
  (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway
    (Api.ScriptInAnyLang
      (Api.PlutusScriptLanguage Api.PlutusScriptV2)
      (Api.PlutusScript _ x)
    )
  ) = Just (Some y)
  where
    y :: GYScript 'PlutusV2
    y = scriptFromApi x

-- FIXME: V3 is not possible in Babbage, shold we indicate it?
someScriptFromReferenceApi
  (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway
    (Api.ScriptInAnyLang
      (Api.PlutusScriptLanguage Api.PlutusScriptV3)
      (Api.PlutusScript _ _))) = Nothing

-- TODO: Add definitions for Conway
someScriptFromReferenceApi
  (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway _) = Nothing

scriptFromApi :: forall v. SingPlutusVersionI v => Api.PlutusScript (PlutusVersionToApi v) -> GYScript v
scriptFromApi script = GYScript v script apiHash
  where
    v = singPlutusVersion @v
    apiScript :: Api.S.Script (PlutusVersionToApi v)
    apiScript = Api.PlutusScript (singPlutusVersionToApi v) script
    apiHash = Api.hashScript apiScript

-- >>> scriptFromCBOR @'PlutusV2 "59212d010000323232323322332233223232323322323232323232323332223332223232323232332232323232323232323233322232323232323233223232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323223232323223232322323253353232323232323232323232323304e3301c4912054687265616420746f6b656e206d697373696e672066726f6d20696e7075742e00330553304d301a50043035500b480094cd4c0e0030854cd4c100034854ccd400454cccccd4028418041808418441804cc140cc0792411c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355072301b50083355072077303b500d3301e4901124e4654206d757374206265206275726e742e005007221062106015333333500a1330503301e4911c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355072301b50083355072077303b500d3301e4901124e4654206d757374206265206275726e742e00500710602106110601060221062153333335009105f13304f3301d49011c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355071301a50073355071076303a500c3304f3301d4914043616e6e6f7420636c61696d206265666f72652074696d65206475726174696f6e20676976656e20666f7220666972737420706c617965722773206d6f76652e0033350455052350443332001505948009402cc071401ccc075241124e4654206d757374206265206275726e742e00500621060105f105f221330513301f49011b4e6f74207369676e656420627920666972737420706c617965722e003355073301c50093355073078303d500e330513301f49110436f6d6d6974206d69736d617463682e0033036372466e28008c178004c10c03ccc144cc07d241104d697373656420646561646c696e652e003335047505433502f3039500e500d301e50095333553353332001505d001003106d1533553335001153335003106110621061153335003106110611062153335003106210611061106c106b1330513301f49011357726f6e67206f757470757420646174756d2e00330513303630435005305e001330513332001505c303b50053507b0033332001505a500406d330513301f4911a546f6b656e206d697373696e672066726f6d206f75747075742e003305833050301d50063038500e48008cc07d24012d5365636f6e6420706c617965722773207374616b65206d697373696e6720696e2064726177206f75747075742e00330583505f301d5006303a500e13301f4901124e4654206d757374206265206275726e742e0050081330513301f4911357726f6e67206f757470757420646174756d2e00330513303630435005305e001330513332001505c303b50053507b0033332001505a500406b330513301f4911a546f6b656e206d697373696e672066726f6d206f75747075742e003305833050301d50063038500e48008cc07d240126596f75206c6f73742c2063616e6e6f742074616b65207374616b652066726f6d2067616d652e00330583505f301d50063332001505948010c0e9403854cd4c0fc0308417c54cccccd40204178417884cc13ccc0752411c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355071301a50073355071076303a500c3304f3301d49120466972737420706c617965722773207374616b65206973206d697373696e672e00330563505d301b50053038500c3304f3301d4901215365636f6e6420706c617965722773207374616b65206973206d697373696e672e00330563505d301b50043332001505748010c0e14030cc13ccc07524011357726f6e67206f757470757420646174756d2e003304f3303430415003304100d3304f3332001505a3039500335079001325335001210611061304050033304f3301d491104d697373656420646561646c696e652e003335045505233502d3037500c500a301c50073301d49011a546f6b656e206d697373696e672066726f6d206f75747075742e00330563304e301b50043036500c480084cc138cc07124011b4e6f74207369676e656420627920666972737420706c617965722e003355070301950063075303a500b3304e3301c4914143616e6e6f7420636c61696d206265666f72652074696d65206475726174696f6e20676976656e20666f72207365636f6e6420706c617965722773206d6f76652e00333504450513504333320015058480094024c06d4018cc071241124e4654206d757374206265206275726e742e005005105e22106015335303e50012100113507c4901334d6174636820726573756c7420657870656374656420627574206f757470757420646174756d20686173206e6f7468696e672e001335506e05f306f500115335335506d3355302a1200122533532323304f33033303b002303b0013304f33033303a002303a0013304f33056303800230380013304f33056303700230370013304f33056303f002303f0013304f3232350022235003225335333573466e3c0100081981944cc0e800c0044194c0dc008c0d8008cc13ccc0c8c0d4008c0d4004cc13ccc0d0c0f0008c0f0004cc13ccc158c0f4008c0f4004cc13ccc0d0c108008c108004cc0d0c10c008c10c004c0f4030c0f0cd541bc180c1c00084cd41c4008004400541c14cd4c0ac01084d400488d40048888d402c88d4008888888888888ccd54c0fc4800488d400888894cd4cc1280600104cd42280401801440154214040284c98c81f0cd5ce2481024c660007c13507a491384e6f206f7574707574206174207468697320736372697074206164647265737320686176696e672073616d6520706172616d65746572732e00221533500110022213507e49012145787065637465642065786163746c79206f6e652067616d65206f75747075742e0015335302a00321350012200113507949011347616d6520696e707574206d697373696e672e00133050330483235001222222222222008500130305006480044d400488008cccd5cd19b8735573aa00e9000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502502635742a01866a04a04c6ae85402ccd409409cd5d0a805199aa814bae502835742a012666aa052eb940a0d5d0a80419a8128179aba150073335502903075a6ae854018c8c8c8cccd5cd19b8735573aa0049000119a8289919191999ab9a3370e6aae7540092000233505633503a75a6ae854008c0ecd5d09aba2500223263208f013357380c011e0211a0226aae7940044dd50009aba150023232323333573466e1cd55cea80124000466a0b066a074eb4d5d0a801181d9aba135744a004464c6411e0266ae7018023c04234044d55cf280089baa001357426ae8940088c98c822c04cd5ce02e045808448089aab9e5001137540026ae854014cd4095d71aba150043335502902c200135742a006666aa052eb88004d5d0a80118171aba135744a004464c6410e0266ae7016021c04214044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a803980f1aba135744a00e464c640f266ae701281e41dccccd5cd19b87500848028848888880188cccd5cd19b87500948020848888880088cccd5cd19b87500a48018848888880148cccd5cd19b87500b480108488888800c8cccd5cd19b87500c480088c8c84888888cc00402001cc134d5d09aba2500f375c6ae8540388cccd5cd19b87500d480008c84888888c01001cc134d5d09aab9e501023263207d33573809c0fa0f60f40f20f00ee0ec26664002a09e605aa00464002606aa00426664002a09c6664002a09c6058a002640026068a002640026068a002260640026666ae68cdc39aab9d500b480008cccc160c8c8c8c8c8c8c8c8c8c8c8c8cccd5cd19b8735573aa016900011999999999983218121aba1500b302435742a0146eb4d5d0a8049bad35742a0106eb4d5d0a8039919191999ab9a3370e6aae75400920002335507a375c6ae854008dd71aba135744a004464c6410a0266ae701582140420c044d55cf280089baa00135742a00c604e6ae854014dd71aba15004375a6ae85400cdd71aba15002375c6ae84d5d128011193190408099ab9c0520810107f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba1500b375c6ae854028cd4060110d5d0a80499a80c1191999ab9a3370ea0029002103111999ab9a3370ea0049001103091999ab9a3370ea0069000103191931903c99ab9c04a079077076075135573a6ea8004d5d09aba2500923263207433573808a0e80e420e626a0e292010350543500135573ca00226ea80044d55cea80109aab9e50011375400226ae8940044d5d1280089aab9e500113754002446a004444444444444a66a666aa604a24002a0484a66a666ae68cdc780700082a82a09a8370008a8368021082a882991a800911100191a80091111111111100291299a8008822899ab9c0020441232230023758002640026aa0c8446666aae7c004941688cd4164c010d5d080118019aba2002065232323333573466e1cd55cea8012400046644246600200600460166ae854008c014d5d09aba2500223263206533573806c0ca0c626aae7940044dd50009191919191999ab9a3370e6aae7540112000233332222123333001005004003002300935742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263206c33573807a0d80d40d20d026aae7540044dd50009aba1500233500a75c6ae84d5d1280111931903319ab9c037066064135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa83091191999aab9f0022505823350573355059300635573aa004600a6aae794008c010d5d100183189aba1001232323333573466e1cd55cea801240004660b060166ae854008cd4014028d5d09aba250022326320613357380640c20be26aae7940044dd500089119191999ab9a3370ea002900011a82d18029aba135573ca00646666ae68cdc3a801240044a0b4464c640c466ae700cc18818017c4d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c640c466ae700cc18818017c1781744d55cea80089baa001232323333573466e1cd55cea80124000466086600a6ae854008dd69aba135744a004464c640bc66ae700bc1781704d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263205c33573805a0b80b426ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263206533573806c0ca0c60c40c20c00be0bc0ba26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc15cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000460b260106ae84d55cf280311931902f19ab9c02f05e05c05b135573aa00626ae8940044d55cf280089baa001232323333573466e1d4005200223056375c6ae84d55cf280191999ab9a3370ea00490001182c1bae357426aae7940108c98c816ccd5ce01602d82c82c09aab9d50011375400224464646666ae68cdc3a800a40084a04a46666ae68cdc3a8012400446a04e600c6ae84d55cf280211999ab9a3370ea00690001091100111931902e19ab9c02d05c05a059058135573aa00226ea80048c8cccd5cd19b8750014800880dc8cccd5cd19b8750024800080dc8c98c8160cd5ce01482c02b02a89aab9d375400224466a03666a0386a04200406a66a03c6a04200206a640026aa0a64422444a66a00220044426600a004666aa600e2400200a00800246a002446a0044444444444446666a01a4a0b24a0b24a0b24666aa602424002a02246a00244a66a6602c00400826a0ba0062a0b801a264246600244a66a004420062002004a090640026aa0a04422444a66a00226a00644002442666a00a440046008004666aa600e2400200a00800244a66a666ae68cdc79a801110011a800910010180178999ab9a3370e6a004440026a0024400206005e205e446a004446a006446466a00a466a0084a66a666ae68cdc780100081b01a8a801881a901a919a802101a9299a999ab9a3371e00400206c06a2a006206a2a66a00642a66a0044266a004466a004466a004466a00446601a0040024070466a004407046601a00400244407044466a0084070444a66a666ae68cdc380300181d81d0a99a999ab9a3370e00a0040760742660620080022074207420662a66a00242066206644666ae68cdc780100081701691a8009111111111100291a8009111111111100311a8009111111111100411a8009111111111100491a800911100111a8009111111111100511a8009111111111100591a8009111111111100211a8009111111111100191a800911100211a8009111111111100391a800911100091a800911100191a8009111111111100111a800911111111110008919a80199a8021a80480080e99a803280400e89111a801111a801111a802911a801112999a999a8080058030010a99a8008a99a8028999a80700580180388128999a80700580180388128999a8070058018038910919800801801091091980080180109111a801111a801912999a999a8048038020010a99a8018800880f880f080f89109198008018010911191919192999a80310a999a80310a999a80410980224c26006930a999a80390980224c2600693080a08090a999a80390980224c26006930a999a80310980224c260069308098a999a80290808880908080a999a80290a999a803909802a4c26008930a999a803109802a4c2600893080988088a999a803109802a4c26008930a999a802909802a4c2600893080912999a80290a999a80390a999a80390999a8068050010008b0b0b08090a999a80310a999a80310999a8060048010008b0b0b0808880812999a80210a999a80310a999a80310999a8060048010008b0b0b08088a999a80290a999a80290999a8058040010008b0b0b0808080792999a80190a999a80290a999a80290999a8058040010008b0b0b08080a999a80210a999a80210999a8050038010008b0b0b0807880712999a80110a999a80210a999a80210999a8050038010008b0b0b08078a999a80190a999a80190999a8048030010008b0b0b08070806890911180180208911000891a80091111111003911a8009119980a00200100091299a801080088091191999ab9a3370ea0029002100c91999ab9a3370ea0049001100e11999ab9a3370ea0069000100e11931901a99ab9c006035033032031135573a6ea8005241035054310011233333333001005225335333573466e1c008004044040401854cd4ccd5cd19b890020010110101004100522333573466e2000800404404088ccd5cd19b8900200101101022333573466e2400800404004488ccd5cd19b88002001010011225335333573466e2400800404404040044008894cd4ccd5cd19b890020010110101002100112220031222002122200122333573466e1c00800403002c488cdc10010008912999a8010a999a8008805080488048a999a8008804880508048a999a80088048804880509119b8000200113222533500221533500221330050020011009153350012100910095001122533350021533350011007100610061533350011006100710061533350011006100610072333500148905506170657200488104526f636b0048810853636973736f72730012320013330020010050052223232300100532001355027223350014800088d4008894cd4ccd5cd19b8f00200900c00b130070011300600332001355026223350014800088d4008894cd4ccd5cd19b8f00200700b00a100113006003122002122001488100253353355010232323232323333333574800c46666ae68cdc39aab9d5006480008cccd55cfa8031281011999aab9f500625021233335573ea00c4a04446666aae7d40189408c8cccd55cf9aba2500725335533553355335323232323232323232323232323333333574801a46666ae68cdc39aab9d500d480008cccd55cfa8069281a11999aab9f500d25035233335573ea01a4a06c46666aae7d4034940dc8cccd55cfa8069281c11999aab9f500d25039233335573ea01a4a07446666aae7d4034940ec8cccd55cfa8069281e11999aab9f500d2503d233335573ea01a4a07c46666aae7cd5d128071299aa99aa99aa99aa99aa99aa99aa99aa99aa99aa99a98199aba150192135041302b0011503f215335303435742a032426a08460040022a0802a07e42a66a606a6ae85406084d4108c00800454100540fc854cd4c0d4d5d0a80b909a82118010008a8200a81f90a99a981a9aba1501621350423002001150401503f215335323232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282391999aab9f500425048233335573e6ae89401494cd4c100d5d0a80390a99a98209aba15007213504c33550480020011504a150492504905004f04e2504604c2504525045250452504504c135744a00226aae7940044dd50009aba1501521350423002001150401503f215335323232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282391999aab9f500425048233335573e6ae89401494cd4c8c8c8ccccccd5d200191999ab9a3370e6aae75400d2000233335573ea0064a09e46666aae7cd5d128021299a98239aba15005213505200115050250500570562504e0542504d2504d2504d2504d054135573ca00226ea8004d5d0a80390a99a981f9aba15007213504c330380020011504a150492504905004f04e2504604c2504525045250452504504c135744a00226aae7940044dd50009aba1501421350423002001150401503f215335303735742a026426a08460040022a0802a07e42a66a606a6ae85404884d4108c00800454100540fc854cd4c0dcd5d0a808909a82118010008a8200a81f90a99a981b9aba1501021350423002001150401503f2503f04604504404304204104003f03e03d03c03b2503303925032250322503225032039135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a016426a04c60220022a04842a66a60386ae85402c84d409cc0080045409454090854cd4cd40748c8c8c8ccccccd5d200211999ab9a3370ea004900211999aab9f500423502d01a2502c03323333573466e1d400d2002233335573ea00a46a05c03a4a05a06846666ae68cdc3a8022400046666aae7d40188d40bc074940b80d4940b40cc0c80c4940a8940a8940a8940a80c44d55cea80109aab9e5001137540026ae85402884d409cc0080045409454090854cd4cd40748c8c8c8ccccccd5d200211999ab9a3370ea004900211999aab9f500423502d01f2502c03323333573466e1d400d2002233335573ea00a46a05c03c4a05a06846666ae68cdc3a8022400046666aae7d40188d40bc080940b80d4940b40cc0c80c4940a8940a8940a8940a80c44d55cea80109aab9e5001137540026ae85402484d409cc0080045409454090940900ac0a80a40a009c9407c094940789407894078940780944d5d1280089aba25001135744a00226aae7940044dd50008009080089a80ea491e446174756d20636f756c646e277420626520646573657269616c697365640022222222222123333333333300100c00b00a009008007006005004003002222212333300100500400300222123300100300212220031222002122200112220031222002122200123232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021280991999aab9f500425014233335573e6ae89401494cd4c02cd5d0a80390a99a99a807119191919191999999aba400623333573466e1d40092002233335573ea00c4a03e46666aae7d4018940808cccd55cfa8031281091999aab9f35744a00e4a66a602e6ae854028854cd4c060d5d0a80510a99a980c9aba1500a21350263330270030020011502415023150222502202902802702623333573466e1d400d2000233335573ea00e4a04046666aae7cd5d128041299a980b9aba150092135023302500115021250210280272501f0250242501d2501d2501d2501d024135573aa00826ae8940044d5d1280089aab9e5001137540026ae85401c84d4060cc05800800454058540549405407006c06894048060940449404494044940440604d5d1280089aab9e50011375400246666666ae900049403494034940348d4038dd68011280680a11919191999999aba400423333573466e1d40092002233335573ea0084a02246666aae7cd5d128029299a98049aba1500621350143017001150122501201901823333573466e1d400d2000233335573ea00a4a02446666aae7cd5d128031299a98051aba1500721350153019001150132501301a019250110170162500f2500f2500f2500f016135573aa00426aae7940044dd500091999999aba40012500b2500b2500b2500b23500c375c0040242446464646666666ae900108cccd5cd19b875002480008cccd55cfa8021280811999aab9f35744a00a4a66a60126ae85401884d404cd404c004540449404406005c8cccd5cd19b875003480088cccd55cfa80291a80928089280880c1280800b00a9280712807128071280700a89aab9d5002135573ca00226ea80044488c00800494ccd4d400488880084d40352411e47616d65206f757470757420646f65736e2774206861766520646174756d0021001213500e49012647616d65206f757470757420646f65736e2774206861766520646174756d20696e6c696e65640011220021221223300100400311221233001003002253353500122335002235007001250062100113500949012d4e6f205075624b65792063726564656e7469616c7320657869737420666f72207468697320616464726573732e002212330010030021212230020031122001212230020032221223330010050040032122300200321223001003123263200333573800200693090008891918008009119801980100100081"
-- Just (GYScript "5a2f01c4186061b8197e6a4646d34ec8fd1f3cbdeb67fbb8ab831b25")

scriptFromCBOR :: forall v. SingPlutusVersionI v => Text -> Maybe (GYScript v)
scriptFromCBOR = scriptFromCBOR' . encodeUtf8

-- >>> scriptFromCBOR' @'PlutusV2 "59212d010000323232323322332233223232323322323232323232323332223332223232323232332232323232323232323233322232323232323233223232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323223232323223232322323253353232323232323232323232323304e3301c4912054687265616420746f6b656e206d697373696e672066726f6d20696e7075742e00330553304d301a50043035500b480094cd4c0e0030854cd4c100034854ccd400454cccccd4028418041808418441804cc140cc0792411c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355072301b50083355072077303b500d3301e4901124e4654206d757374206265206275726e742e005007221062106015333333500a1330503301e4911c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355072301b50083355072077303b500d3301e4901124e4654206d757374206265206275726e742e00500710602106110601060221062153333335009105f13304f3301d49011c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355071301a50073355071076303a500c3304f3301d4914043616e6e6f7420636c61696d206265666f72652074696d65206475726174696f6e20676976656e20666f7220666972737420706c617965722773206d6f76652e0033350455052350443332001505948009402cc071401ccc075241124e4654206d757374206265206275726e742e00500621060105f105f221330513301f49011b4e6f74207369676e656420627920666972737420706c617965722e003355073301c50093355073078303d500e330513301f49110436f6d6d6974206d69736d617463682e0033036372466e28008c178004c10c03ccc144cc07d241104d697373656420646561646c696e652e003335047505433502f3039500e500d301e50095333553353332001505d001003106d1533553335001153335003106110621061153335003106110611062153335003106210611061106c106b1330513301f49011357726f6e67206f757470757420646174756d2e00330513303630435005305e001330513332001505c303b50053507b0033332001505a500406d330513301f4911a546f6b656e206d697373696e672066726f6d206f75747075742e003305833050301d50063038500e48008cc07d24012d5365636f6e6420706c617965722773207374616b65206d697373696e6720696e2064726177206f75747075742e00330583505f301d5006303a500e13301f4901124e4654206d757374206265206275726e742e0050081330513301f4911357726f6e67206f757470757420646174756d2e00330513303630435005305e001330513332001505c303b50053507b0033332001505a500406b330513301f4911a546f6b656e206d697373696e672066726f6d206f75747075742e003305833050301d50063038500e48008cc07d240126596f75206c6f73742c2063616e6e6f742074616b65207374616b652066726f6d2067616d652e00330583505f301d50063332001505948010c0e9403854cd4c0fc0308417c54cccccd40204178417884cc13ccc0752411c4e6f74207369676e6564206279207365636f6e6420706c617965722e003355071301a50073355071076303a500c3304f3301d49120466972737420706c617965722773207374616b65206973206d697373696e672e00330563505d301b50053038500c3304f3301d4901215365636f6e6420706c617965722773207374616b65206973206d697373696e672e00330563505d301b50043332001505748010c0e14030cc13ccc07524011357726f6e67206f757470757420646174756d2e003304f3303430415003304100d3304f3332001505a3039500335079001325335001210611061304050033304f3301d491104d697373656420646561646c696e652e003335045505233502d3037500c500a301c50073301d49011a546f6b656e206d697373696e672066726f6d206f75747075742e00330563304e301b50043036500c480084cc138cc07124011b4e6f74207369676e656420627920666972737420706c617965722e003355070301950063075303a500b3304e3301c4914143616e6e6f7420636c61696d206265666f72652074696d65206475726174696f6e20676976656e20666f72207365636f6e6420706c617965722773206d6f76652e00333504450513504333320015058480094024c06d4018cc071241124e4654206d757374206265206275726e742e005005105e22106015335303e50012100113507c4901334d6174636820726573756c7420657870656374656420627574206f757470757420646174756d20686173206e6f7468696e672e001335506e05f306f500115335335506d3355302a1200122533532323304f33033303b002303b0013304f33033303a002303a0013304f33056303800230380013304f33056303700230370013304f33056303f002303f0013304f3232350022235003225335333573466e3c0100081981944cc0e800c0044194c0dc008c0d8008cc13ccc0c8c0d4008c0d4004cc13ccc0d0c0f0008c0f0004cc13ccc158c0f4008c0f4004cc13ccc0d0c108008c108004cc0d0c10c008c10c004c0f4030c0f0cd541bc180c1c00084cd41c4008004400541c14cd4c0ac01084d400488d40048888d402c88d4008888888888888ccd54c0fc4800488d400888894cd4cc1280600104cd42280401801440154214040284c98c81f0cd5ce2481024c660007c13507a491384e6f206f7574707574206174207468697320736372697074206164647265737320686176696e672073616d6520706172616d65746572732e00221533500110022213507e49012145787065637465642065786163746c79206f6e652067616d65206f75747075742e0015335302a00321350012200113507949011347616d6520696e707574206d697373696e672e00133050330483235001222222222222008500130305006480044d400488008cccd5cd19b8735573aa00e9000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233502502635742a01866a04a04c6ae85402ccd409409cd5d0a805199aa814bae502835742a012666aa052eb940a0d5d0a80419a8128179aba150073335502903075a6ae854018c8c8c8cccd5cd19b8735573aa0049000119a8289919191999ab9a3370e6aae7540092000233505633503a75a6ae854008c0ecd5d09aba2500223263208f013357380c011e0211a0226aae7940044dd50009aba150023232323333573466e1cd55cea80124000466a0b066a074eb4d5d0a801181d9aba135744a004464c6411e0266ae7018023c04234044d55cf280089baa001357426ae8940088c98c822c04cd5ce02e045808448089aab9e5001137540026ae854014cd4095d71aba150043335502902c200135742a006666aa052eb88004d5d0a80118171aba135744a004464c6410e0266ae7016021c04214044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a803980f1aba135744a00e464c640f266ae701281e41dccccd5cd19b87500848028848888880188cccd5cd19b87500948020848888880088cccd5cd19b87500a48018848888880148cccd5cd19b87500b480108488888800c8cccd5cd19b87500c480088c8c84888888cc00402001cc134d5d09aba2500f375c6ae8540388cccd5cd19b87500d480008c84888888c01001cc134d5d09aab9e501023263207d33573809c0fa0f60f40f20f00ee0ec26664002a09e605aa00464002606aa00426664002a09c6664002a09c6058a002640026068a002640026068a002260640026666ae68cdc39aab9d500b480008cccc160c8c8c8c8c8c8c8c8c8c8c8c8cccd5cd19b8735573aa016900011999999999983218121aba1500b302435742a0146eb4d5d0a8049bad35742a0106eb4d5d0a8039919191999ab9a3370e6aae75400920002335507a375c6ae854008dd71aba135744a004464c6410a0266ae701582140420c044d55cf280089baa00135742a00c604e6ae854014dd71aba15004375a6ae85400cdd71aba15002375c6ae84d5d128011193190408099ab9c0520810107f135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba1500b375c6ae854028cd4060110d5d0a80499a80c1191999ab9a3370ea0029002103111999ab9a3370ea0049001103091999ab9a3370ea0069000103191931903c99ab9c04a079077076075135573a6ea8004d5d09aba2500923263207433573808a0e80e420e626a0e292010350543500135573ca00226ea80044d55cea80109aab9e50011375400226ae8940044d5d1280089aab9e500113754002446a004444444444444a66a666aa604a24002a0484a66a666ae68cdc780700082a82a09a8370008a8368021082a882991a800911100191a80091111111111100291299a8008822899ab9c0020441232230023758002640026aa0c8446666aae7c004941688cd4164c010d5d080118019aba2002065232323333573466e1cd55cea8012400046644246600200600460166ae854008c014d5d09aba2500223263206533573806c0ca0c626aae7940044dd50009191919191999ab9a3370e6aae7540112000233332222123333001005004003002300935742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263206c33573807a0d80d40d20d026aae7540044dd50009aba1500233500a75c6ae84d5d1280111931903319ab9c037066064135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa83091191999aab9f0022505823350573355059300635573aa004600a6aae794008c010d5d100183189aba1001232323333573466e1cd55cea801240004660b060166ae854008cd4014028d5d09aba250022326320613357380640c20be26aae7940044dd500089119191999ab9a3370ea002900011a82d18029aba135573ca00646666ae68cdc3a801240044a0b4464c640c466ae700cc18818017c4d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c640c466ae700cc18818017c1781744d55cea80089baa001232323333573466e1cd55cea80124000466086600a6ae854008dd69aba135744a004464c640bc66ae700bc1781704d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263205c33573805a0b80b426ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263206533573806c0ca0c60c40c20c00be0bc0ba26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc15cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000460b260106ae84d55cf280311931902f19ab9c02f05e05c05b135573aa00626ae8940044d55cf280089baa001232323333573466e1d4005200223056375c6ae84d55cf280191999ab9a3370ea00490001182c1bae357426aae7940108c98c816ccd5ce01602d82c82c09aab9d50011375400224464646666ae68cdc3a800a40084a04a46666ae68cdc3a8012400446a04e600c6ae84d55cf280211999ab9a3370ea00690001091100111931902e19ab9c02d05c05a059058135573aa00226ea80048c8cccd5cd19b8750014800880dc8cccd5cd19b8750024800080dc8c98c8160cd5ce01482c02b02a89aab9d375400224466a03666a0386a04200406a66a03c6a04200206a640026aa0a64422444a66a00220044426600a004666aa600e2400200a00800246a002446a0044444444444446666a01a4a0b24a0b24a0b24666aa602424002a02246a00244a66a6602c00400826a0ba0062a0b801a264246600244a66a004420062002004a090640026aa0a04422444a66a00226a00644002442666a00a440046008004666aa600e2400200a00800244a66a666ae68cdc79a801110011a800910010180178999ab9a3370e6a004440026a0024400206005e205e446a004446a006446466a00a466a0084a66a666ae68cdc780100081b01a8a801881a901a919a802101a9299a999ab9a3371e00400206c06a2a006206a2a66a00642a66a0044266a004466a004466a004466a00446601a0040024070466a004407046601a00400244407044466a0084070444a66a666ae68cdc380300181d81d0a99a999ab9a3370e00a0040760742660620080022074207420662a66a00242066206644666ae68cdc780100081701691a8009111111111100291a8009111111111100311a8009111111111100411a8009111111111100491a800911100111a8009111111111100511a8009111111111100591a8009111111111100211a8009111111111100191a800911100211a8009111111111100391a800911100091a800911100191a8009111111111100111a800911111111110008919a80199a8021a80480080e99a803280400e89111a801111a801111a802911a801112999a999a8080058030010a99a8008a99a8028999a80700580180388128999a80700580180388128999a8070058018038910919800801801091091980080180109111a801111a801912999a999a8048038020010a99a8018800880f880f080f89109198008018010911191919192999a80310a999a80310a999a80410980224c26006930a999a80390980224c2600693080a08090a999a80390980224c26006930a999a80310980224c260069308098a999a80290808880908080a999a80290a999a803909802a4c26008930a999a803109802a4c2600893080988088a999a803109802a4c26008930a999a802909802a4c2600893080912999a80290a999a80390a999a80390999a8068050010008b0b0b08090a999a80310a999a80310999a8060048010008b0b0b0808880812999a80210a999a80310a999a80310999a8060048010008b0b0b08088a999a80290a999a80290999a8058040010008b0b0b0808080792999a80190a999a80290a999a80290999a8058040010008b0b0b08080a999a80210a999a80210999a8050038010008b0b0b0807880712999a80110a999a80210a999a80210999a8050038010008b0b0b08078a999a80190a999a80190999a8048030010008b0b0b08070806890911180180208911000891a80091111111003911a8009119980a00200100091299a801080088091191999ab9a3370ea0029002100c91999ab9a3370ea0049001100e11999ab9a3370ea0069000100e11931901a99ab9c006035033032031135573a6ea8005241035054310011233333333001005225335333573466e1c008004044040401854cd4ccd5cd19b890020010110101004100522333573466e2000800404404088ccd5cd19b8900200101101022333573466e2400800404004488ccd5cd19b88002001010011225335333573466e2400800404404040044008894cd4ccd5cd19b890020010110101002100112220031222002122200122333573466e1c00800403002c488cdc10010008912999a8010a999a8008805080488048a999a8008804880508048a999a80088048804880509119b8000200113222533500221533500221330050020011009153350012100910095001122533350021533350011007100610061533350011006100710061533350011006100610072333500148905506170657200488104526f636b0048810853636973736f72730012320013330020010050052223232300100532001355027223350014800088d4008894cd4ccd5cd19b8f00200900c00b130070011300600332001355026223350014800088d4008894cd4ccd5cd19b8f00200700b00a100113006003122002122001488100253353355010232323232323333333574800c46666ae68cdc39aab9d5006480008cccd55cfa8031281011999aab9f500625021233335573ea00c4a04446666aae7d40189408c8cccd55cf9aba2500725335533553355335323232323232323232323232323333333574801a46666ae68cdc39aab9d500d480008cccd55cfa8069281a11999aab9f500d25035233335573ea01a4a06c46666aae7d4034940dc8cccd55cfa8069281c11999aab9f500d25039233335573ea01a4a07446666aae7d4034940ec8cccd55cfa8069281e11999aab9f500d2503d233335573ea01a4a07c46666aae7cd5d128071299aa99aa99aa99aa99aa99aa99aa99aa99aa99aa99a98199aba150192135041302b0011503f215335303435742a032426a08460040022a0802a07e42a66a606a6ae85406084d4108c00800454100540fc854cd4c0d4d5d0a80b909a82118010008a8200a81f90a99a981a9aba1501621350423002001150401503f215335323232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282391999aab9f500425048233335573e6ae89401494cd4c100d5d0a80390a99a98209aba15007213504c33550480020011504a150492504905004f04e2504604c2504525045250452504504c135744a00226aae7940044dd50009aba1501521350423002001150401503f215335323232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021282391999aab9f500425048233335573e6ae89401494cd4c8c8c8ccccccd5d200191999ab9a3370e6aae75400d2000233335573ea0064a09e46666aae7cd5d128021299a98239aba15005213505200115050250500570562504e0542504d2504d2504d2504d054135573ca00226ea8004d5d0a80390a99a981f9aba15007213504c330380020011504a150492504905004f04e2504604c2504525045250452504504c135744a00226aae7940044dd50009aba1501421350423002001150401503f215335303735742a026426a08460040022a0802a07e42a66a606a6ae85404884d4108c00800454100540fc854cd4c0dcd5d0a808909a82118010008a8200a81f90a99a981b9aba1501021350423002001150401503f2503f04604504404304204104003f03e03d03c03b2503303925032250322503225032039135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a016426a04c60220022a04842a66a60386ae85402c84d409cc0080045409454090854cd4cd40748c8c8c8ccccccd5d200211999ab9a3370ea004900211999aab9f500423502d01a2502c03323333573466e1d400d2002233335573ea00a46a05c03a4a05a06846666ae68cdc3a8022400046666aae7d40188d40bc074940b80d4940b40cc0c80c4940a8940a8940a8940a80c44d55cea80109aab9e5001137540026ae85402884d409cc0080045409454090854cd4cd40748c8c8c8ccccccd5d200211999ab9a3370ea004900211999aab9f500423502d01f2502c03323333573466e1d400d2002233335573ea00a46a05c03c4a05a06846666ae68cdc3a8022400046666aae7d40188d40bc080940b80d4940b40cc0c80c4940a8940a8940a8940a80c44d55cea80109aab9e5001137540026ae85402484d409cc0080045409454090940900ac0a80a40a009c9407c094940789407894078940780944d5d1280089aba25001135744a00226aae7940044dd50008009080089a80ea491e446174756d20636f756c646e277420626520646573657269616c697365640022222222222123333333333300100c00b00a009008007006005004003002222212333300100500400300222123300100300212220031222002122200112220031222002122200123232323333333574800846666ae68cdc39aab9d5004480008cccd55cfa8021280991999aab9f500425014233335573e6ae89401494cd4c02cd5d0a80390a99a99a807119191919191999999aba400623333573466e1d40092002233335573ea00c4a03e46666aae7d4018940808cccd55cfa8031281091999aab9f35744a00e4a66a602e6ae854028854cd4c060d5d0a80510a99a980c9aba1500a21350263330270030020011502415023150222502202902802702623333573466e1d400d2000233335573ea00e4a04046666aae7cd5d128041299a980b9aba150092135023302500115021250210280272501f0250242501d2501d2501d2501d024135573aa00826ae8940044d5d1280089aab9e5001137540026ae85401c84d4060cc05800800454058540549405407006c06894048060940449404494044940440604d5d1280089aab9e50011375400246666666ae900049403494034940348d4038dd68011280680a11919191999999aba400423333573466e1d40092002233335573ea0084a02246666aae7cd5d128029299a98049aba1500621350143017001150122501201901823333573466e1d400d2000233335573ea00a4a02446666aae7cd5d128031299a98051aba1500721350153019001150132501301a019250110170162500f2500f2500f2500f016135573aa00426aae7940044dd500091999999aba40012500b2500b2500b2500b23500c375c0040242446464646666666ae900108cccd5cd19b875002480008cccd55cfa8021280811999aab9f35744a00a4a66a60126ae85401884d404cd404c004540449404406005c8cccd5cd19b875003480088cccd55cfa80291a80928089280880c1280800b00a9280712807128071280700a89aab9d5002135573ca00226ea80044488c00800494ccd4d400488880084d40352411e47616d65206f757470757420646f65736e2774206861766520646174756d0021001213500e49012647616d65206f757470757420646f65736e2774206861766520646174756d20696e6c696e65640011220021221223300100400311221233001003002253353500122335002235007001250062100113500949012d4e6f205075624b65792063726564656e7469616c7320657869737420666f72207468697320616464726573732e002212330010030021212230020031122001212230020032221223330010050040032122300200321223001003123263200333573800200693090008891918008009119801980100100081"
-- Just (GYScript "5a2f01c4186061b8197e6a4646d34ec8fd1f3cbdeb67fbb8ab831b25")

scriptFromCBOR' :: forall v. SingPlutusVersionI v => ByteString -> Maybe (GYScript v)
scriptFromCBOR' b = do
    bs <- rightToMaybe (BS16.decode b)
    case singPlutusVersion @v of
      SingPlutusV1 -> fmap scriptFromApi $ rightToMaybe $ flip Api.deserialiseFromRawBytes bs $ Api.AsPlutusScript $ Api.proxyToAsType $ Proxy @(PlutusVersionToApi v)
      SingPlutusV2 -> fmap scriptFromApi $ rightToMaybe $ flip Api.deserialiseFromRawBytes bs $ Api.AsPlutusScript $ Api.proxyToAsType $ Proxy @(PlutusVersionToApi v)

scriptPlutusHash :: GYScript v -> PlutusV1.ScriptHash
scriptPlutusHash = apiHashToPlutus . scriptApiHash

someScriptPlutusHash :: Some GYScript -> PlutusV1.ScriptHash
someScriptPlutusHash (Some s) = scriptPlutusHash s

scriptApiHash :: GYScript v -> Api.ScriptHash
scriptApiHash (GYScript _ _ ah) = ah

scriptToApiPlutusScriptWitness
    :: GYScript v
    -> Api.S.ScriptDatum ctx
    -> Api.ScriptRedeemer
    -> Api.ExecutionUnits
    -> Api.ScriptWitness ctx ApiEra
scriptToApiPlutusScriptWitness (GYScript v api _) = case v of
    SingPlutusV1 -> Api.PlutusScriptWitness
        Api.PlutusScriptV1InConway
        Api.PlutusScriptV1
        (Api.S.PScript api)
    SingPlutusV2 -> Api.PlutusScriptWitness
        Api.PlutusScriptV2InConway
        Api.PlutusScriptV2
        (Api.S.PScript api)

referenceScriptToApiPlutusScriptWitness
  :: GYTxOutRef
  -> GYScript 'PlutusV2
  -> Api.S.ScriptDatum witctx
  -> Api.S.ScriptRedeemer
  -> Api.S.ExecutionUnits
  -> Api.S.ScriptWitness witctx Api.S.ConwayEra
referenceScriptToApiPlutusScriptWitness r s =
    Api.PlutusScriptWitness
    Api.PlutusScriptV2InConway
    Api.PlutusScriptV2
    (Api.S.PReferenceScript (txOutRefToApi r) (Just (scriptApiHash s)))

scriptSize :: Some GYScript -> Int
scriptSize (Some s) = scriptToApiScriptInEra s & Api.toShelleyScript & originalBytesSize  -- Maybe we could have simply obtained size from serialised bytestring but this is how it script size is actually determined inside ledger codebase.

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

-- | Type encapsulating both simple and plutus scripts.
data GYAnyScript where
    GYSimpleScript :: !GYSimpleScript -> GYAnyScript
    GYPlutusScript :: forall v. !(GYScript v) -> GYAnyScript

deriving instance Show GYAnyScript

instance Eq GYAnyScript where
  GYSimpleScript s1 == GYSimpleScript s2 = s1 == s2
  GYPlutusScript s1 == GYPlutusScript s2 = defaultEq s1 s2
  _ == _                                 = False
