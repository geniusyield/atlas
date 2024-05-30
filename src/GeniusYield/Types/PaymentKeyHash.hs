{-|
Module      : GeniusYield.Types.PaymentKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.PaymentKeyHash (
    GYPaymentKeyHash,
    paymentKeyHashFromPlutus,
    paymentKeyHashToPlutus,
    paymentKeyHashToApi,
    paymentKeyHashFromApi,
) where

import qualified Cardano.Api                  as Api
import           Control.Lens                 ((?~))
import qualified Data.Aeson.Types             as Aeson
import qualified Data.Csv                     as Csv
import qualified Data.OpenApi                 as OpenApi
import qualified Data.Swagger                 as Swagger
import qualified Data.Swagger.Internal.Schema as Swagger
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           GeniusYield.Imports
import           GeniusYield.Types.Ledger
import           GeniusYield.Types.PubKeyHash (AsPubKeyHash (..), CanSignTx)
import           GeniusYield.Utils            (printOpenApiSchema, swaggerToOpenApiSchema)
import qualified PlutusLedgerApi.V1.Crypto    as Plutus
import qualified PlutusTx.Builtins            as Plutus
import qualified PlutusTx.Builtins.Internal   as Plutus
import qualified Text.Printf                  as Printf
import           Unsafe.Coerce                (unsafeCoerce)



-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import qualified Data.Csv                   as Csv
-- >>> import           Data.Proxy
-- >>> import qualified Text.Printf                as Printf

newtype GYPaymentKeyHash = GYPaymentKeyHash (Api.Hash Api.PaymentKey)
    deriving stock Show
    deriving newtype (Eq, Ord, IsString)

instance AsPubKeyHash GYPaymentKeyHash where
  toPubKeyHash = unsafeCoerce  -- We could have exported `GYPubKeyHash` from an internal module but `GYPubKeyHash` needs an overhaul anyways.
  fromPubKeyHash = unsafeCoerce

instance CanSignTx GYPaymentKeyHash

-- |
--
-- >>> paymentKeyHashFromPlutus "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- >>> paymentKeyHashFromPlutus "abcd"
-- Left (DeserialiseRawBytesError {ptceTag = "paymentKeyHashFromPlutus \"\\171\\205\", error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise Hash PaymentKey\"}"})
--
paymentKeyHashFromPlutus :: Plutus.PubKeyHash -> Either PlutusToCardanoError GYPaymentKeyHash
paymentKeyHashFromPlutus (Plutus.PubKeyHash (Plutus.BuiltinByteString h)) =
    bimap
        (\e -> DeserialiseRawBytesError $ Text.pack $ "paymentKeyHashFromPlutus " ++ show h ++ ", error: " ++ show e)
        GYPaymentKeyHash
    $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsPaymentKey) h

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> paymentKeyHashToPlutus pkh
-- e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
--
paymentKeyHashToPlutus :: GYPaymentKeyHash -> Plutus.PubKeyHash
paymentKeyHashToPlutus = coerce fromCardanoPaymentKeyHash where
    -- this is not exported from plutus-ledger
    fromCardanoPaymentKeyHash :: Api.Hash Api.PaymentKey -> Plutus.PubKeyHash
    fromCardanoPaymentKeyHash paymentKeyHash = Plutus.PubKeyHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes paymentKeyHash

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> paymentKeyHashToApi pkh
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
paymentKeyHashToApi :: GYPaymentKeyHash -> Api.Hash Api.PaymentKey
paymentKeyHashToApi = coerce

-- |
--
-- >>> paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
paymentKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPaymentKeyHash
paymentKeyHashFromApi = coerce

-- |
--
-- >>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- >>> LBS8.putStrLn $ Aeson.encode pkh
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
instance Aeson.ToJSON GYPaymentKeyHash where
    toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . paymentKeyHashToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
-- Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- Invalid characters:
--
-- >>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
-- Left "Error in $: RawBytesHexErrorBase16DecodeFail \"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\" \"invalid character at offset: 53\""
--
instance Aeson.FromJSON GYPaymentKeyHash where
    parseJSON = Aeson.withText "GYPaymentKeyHash" $
        either
            (fail . show)
            (return . GYPaymentKeyHash)
        . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)
        . Text.encodeUtf8

-- |
--
-- >>> Printf.printf "%s\n" $ paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
--
instance Printf.PrintfArg GYPaymentKeyHash where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentKeyHashToApi

-- |
--
-- >>> Csv.toField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
--
instance Csv.ToField GYPaymentKeyHash where
    toField = Api.serialiseToRawBytesHex . paymentKeyHashToApi

-- |
--
-- >>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")
--
-- >>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "not a payment key hash"
-- Left "RawBytesHexErrorBase16DecodeFail \"not a payment key hash\" \"invalid character at offset: 0\""
--
instance Csv.FromField GYPaymentKeyHash where
    parseField = either (fail . show) (return . paymentKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)

-------------------------------------------------------------------------------
-- openapi & swagger schema
-------------------------------------------------------------------------------

-- |
--
-- >>> printOpenApiSchema (Proxy @GYPaymentKeyHash)
-- (fromList [],NamedSchema {_namedSchemaName = Just "GYPaymentKeyHash", _namedSchemaSchema = Schema {_schemaTitle = Nothing, _schemaDescription = Just "The hash of a payment public key.", _schemaRequired = [], _schemaNullable = Nothing, _schemaAllOf = Nothing, _schemaOneOf = Nothing, _schemaNot = Nothing, _schemaAnyOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaWriteOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Just (String "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"), _schemaDeprecated = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaDefault = Nothing, _schemaType = Just OpenApiString, _schemaFormat = Just "hex", _schemaItems = Nothing, _schemaMaximum = Nothing, _schemaExclusiveMaximum = Nothing, _schemaMinimum = Nothing, _schemaExclusiveMinimum = Nothing, _schemaMaxLength = Just 56, _schemaMinLength = Just 56, _schemaPattern = Nothing, _schemaMaxItems = Nothing, _schemaMinItems = Nothing, _schemaUniqueItems = Nothing, _schemaEnum = Nothing, _schemaMultipleOf = Nothing}})
--
instance OpenApi.ToSchema GYPaymentKeyHash where
  declareNamedSchema _ = pure $ swaggerToOpenApiSchema (Proxy @GYPaymentKeyHash)

instance Swagger.ToSchema GYPaymentKeyHash where
  declareNamedSchema _ = pure $ Swagger.named "GYPaymentKeyHash" $ mempty
                       & Swagger.type_       ?~ Swagger.SwaggerString
                       & Swagger.format      ?~ "hex"
                       & Swagger.description ?~ "The hash of a payment public key."
                       & Swagger.example     ?~ toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
                       & Swagger.maxLength   ?~ 56
                       & Swagger.minLength   ?~ 56
