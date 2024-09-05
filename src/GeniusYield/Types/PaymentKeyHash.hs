{- |
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
  paymentKeyHashFromLedger,
  paymentKeyHashToLedger,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Keys.Shelley qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Control.Lens ((?~))
import Data.Aeson.Types qualified as Aeson
import Data.Csv qualified as Csv
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports
import GeniusYield.Types.Ledger
import GeniusYield.Types.PubKeyHash (
  AsPubKeyHash (..),
  CanSignTx,
  pubKeyHashFromApi,
  pubKeyHashToApi,
 )
import PlutusLedgerApi.V1.Crypto qualified as Plutus
import PlutusTx.Builtins qualified as Plutus
import PlutusTx.Builtins.Internal qualified as Plutus
import Text.Printf qualified as Printf

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import qualified Text.Printf                as Printf
-}

newtype GYPaymentKeyHash = GYPaymentKeyHash (Api.Hash Api.PaymentKey)
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance AsPubKeyHash GYPaymentKeyHash where
  toPubKeyHash = paymentKeyHashToApi >>> pubKeyHashFromApi
  fromPubKeyHash = pubKeyHashToApi >>> paymentKeyHashFromApi

instance CanSignTx GYPaymentKeyHash

{- |

>>> paymentKeyHashFromPlutus "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

>>> paymentKeyHashFromPlutus "abcd"
Left (DeserialiseRawBytesError {ptceTag = "paymentKeyHashFromPlutus \"\\171\\205\", error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise Hash PaymentKey\"}"})
-}
paymentKeyHashFromPlutus :: Plutus.PubKeyHash -> Either PlutusToCardanoError GYPaymentKeyHash
paymentKeyHashFromPlutus (Plutus.PubKeyHash (Plutus.BuiltinByteString h)) =
  bimap
    (\e -> DeserialiseRawBytesError $ Text.pack $ "paymentKeyHashFromPlutus " ++ show h ++ ", error: " ++ show e)
    GYPaymentKeyHash
    $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsPaymentKey) h

{- |

>>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> paymentKeyHashToPlutus pkh
e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
-}
paymentKeyHashToPlutus :: GYPaymentKeyHash -> Plutus.PubKeyHash
paymentKeyHashToPlutus = coerce fromCardanoPaymentKeyHash
  where
    -- this is not exported from plutus-ledger
    fromCardanoPaymentKeyHash :: Api.Hash Api.PaymentKey -> Plutus.PubKeyHash
    fromCardanoPaymentKeyHash paymentKeyHash = Plutus.PubKeyHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes paymentKeyHash

{- |

>>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> paymentKeyHashToApi pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
paymentKeyHashToApi :: GYPaymentKeyHash -> Api.Hash Api.PaymentKey
paymentKeyHashToApi = coerce

{- |

>>> paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
paymentKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPaymentKeyHash
paymentKeyHashFromApi = coerce

-- | Convert to corresponding ledger representation.
paymentKeyHashToLedger :: GYPaymentKeyHash -> Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto
paymentKeyHashToLedger = paymentKeyHashToApi >>> Api.unPaymentKeyHash

-- | Convert from corresponding ledger representation.
paymentKeyHashFromLedger :: Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto -> GYPaymentKeyHash
paymentKeyHashFromLedger = Api.PaymentKeyHash >>> paymentKeyHashFromApi

{- |

>>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> LBS8.putStrLn $ Aeson.encode pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Aeson.ToJSON GYPaymentKeyHash where
  toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . paymentKeyHashToApi

{- |

>>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

Invalid characters:

>>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
Left "Error in $: RawBytesHexErrorBase16DecodeFail \"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\" \"invalid character at offset: 53\""
-}
instance Aeson.FromJSON GYPaymentKeyHash where
  parseJSON =
    Aeson.withText "GYPaymentKeyHash" $
      either
        (fail . show)
        (return . GYPaymentKeyHash)
        . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)
        . Text.encodeUtf8

{- |

>>> Printf.printf "%s\n" $ paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
-}
instance Printf.PrintfArg GYPaymentKeyHash where
  formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentKeyHashToApi

{- |

>>> Csv.toField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Csv.ToField GYPaymentKeyHash where
  toField = Api.serialiseToRawBytesHex . paymentKeyHashToApi

{- |

>>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right (GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

>>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "not a payment key hash"
Left "RawBytesHexErrorBase16DecodeFail \"not a payment key hash\" \"invalid character at offset: 0\""
-}
instance Csv.FromField GYPaymentKeyHash where
  parseField = either (fail . show) (return . paymentKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToSchema GYPaymentKeyHash where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYPaymentKeyHash" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "The hash of a payment public key."
            & Swagger.example
          ?~ toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
            & Swagger.maxLength
          ?~ 56
            & Swagger.minLength
          ?~ 56
