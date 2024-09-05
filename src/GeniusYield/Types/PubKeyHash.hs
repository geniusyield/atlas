{- |
Module      : GeniusYield.Types.PubKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.PubKeyHash (
  GYPubKeyHash (..),
  AsPubKeyHash (..),
  CanSignTx,
  pubKeyHashFromPlutus,
  pubKeyHashToPlutus,
  pubKeyHashToApi,
  pubKeyHashFromApi,
  pubKeyHashToLedger,
  pubKeyHashFromLedger,
) where

import Control.Lens ((?~))
import GeniusYield.Imports

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Keys qualified as Ledger
import Data.Aeson.Types qualified as Aeson
import Data.Csv qualified as Csv
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Types.Ledger
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

newtype GYPubKeyHash = GYPubKeyHash (Api.Hash Api.PaymentKey)
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

class AsPubKeyHash a where
  toPubKeyHash :: a -> GYPubKeyHash
  fromPubKeyHash :: GYPubKeyHash -> a

class (AsPubKeyHash a) => CanSignTx a

instance AsPubKeyHash GYPubKeyHash where
  toPubKeyHash = id
  fromPubKeyHash = id

instance CanSignTx GYPubKeyHash

{- |

>>> pubKeyHashFromPlutus "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

>>> pubKeyHashFromPlutus "abcd"
Left (DeserialiseRawBytesError {ptceTag = "pubKeyHashFromPlutus \"\\171\\205\", error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise Hash PaymentKey\"}"})
-}
pubKeyHashFromPlutus :: Plutus.PubKeyHash -> Either PlutusToCardanoError GYPubKeyHash
pubKeyHashFromPlutus (Plutus.PubKeyHash (Plutus.BuiltinByteString h)) =
  bimap
    (\e -> DeserialiseRawBytesError $ Text.pack $ "pubKeyHashFromPlutus " ++ show h ++ ", error: " ++ show e)
    GYPubKeyHash
    $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsPaymentKey) h

{- |

>>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> pubKeyHashToPlutus pkh
e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
-}
pubKeyHashToPlutus :: GYPubKeyHash -> Plutus.PubKeyHash
pubKeyHashToPlutus = coerce fromCardanoPaymentKeyHash
  where
    -- this is not exported from plutus-ledger
    fromCardanoPaymentKeyHash :: Api.Hash Api.PaymentKey -> Plutus.PubKeyHash
    fromCardanoPaymentKeyHash paymentKeyHash = Plutus.PubKeyHash $ Plutus.toBuiltin $ Api.serialiseToRawBytes paymentKeyHash

{- |

>>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> pubKeyHashToApi pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
pubKeyHashToApi :: GYPubKeyHash -> Api.Hash Api.PaymentKey
pubKeyHashToApi = coerce

{- |

>>> pubKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
pubKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPubKeyHash
pubKeyHashFromApi = coerce

-- | Convert to corresponding ledger representation.
pubKeyHashToLedger :: GYPubKeyHash -> Ledger.KeyHash (a :: Ledger.KeyRole) Ledger.StandardCrypto
pubKeyHashToLedger = pubKeyHashToApi >>> Api.unPaymentKeyHash >>> Ledger.coerceKeyRole

-- | Convert from corresponding ledger representation.
pubKeyHashFromLedger :: Ledger.KeyHash (a :: Ledger.KeyRole) Ledger.StandardCrypto -> GYPubKeyHash
pubKeyHashFromLedger = Ledger.coerceKeyRole >>> Api.PaymentKeyHash >>> pubKeyHashFromApi

{- |

>>> let Just pkh = Aeson.decode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> LBS8.putStrLn $ Aeson.encode pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Aeson.ToJSON GYPubKeyHash where
  toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . pubKeyHashToApi

{- |

>>> Aeson.eitherDecode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

Invalid characters:

>>> Aeson.eitherDecode @GYPubKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
Left "Error in $: RawBytesHexErrorBase16DecodeFail \"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\" \"invalid character at offset: 53\""
-}
instance Aeson.FromJSON GYPubKeyHash where
  parseJSON =
    Aeson.withText "GYPubKeyHash" $
      either
        (fail . show)
        (return . GYPubKeyHash)
        . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)
        . Text.encodeUtf8

{- |

>>> Printf.printf "%s\n" $ pubKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
-}
instance Printf.PrintfArg GYPubKeyHash where
  formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . pubKeyHashToApi

{- |

>>> Csv.toField @GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Csv.ToField GYPubKeyHash where
  toField = Api.serialiseToRawBytesHex . pubKeyHashToApi

{- |

>>> Csv.runParser $ Csv.parseField @GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right (GYPubKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

>>> Csv.runParser $ Csv.parseField @GYPubKeyHash "not a pubkey hash"
Left "RawBytesHexErrorBase16DecodeFail \"not a pubkey hash\" \"invalid bytestring size\""
-}
instance Csv.FromField GYPubKeyHash where
  parseField = either (fail . show) (return . pubKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey)

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToSchema GYPubKeyHash where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYPubKeyHash" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "The hash of a public key."
            & Swagger.example
          ?~ toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
            & Swagger.maxLength
          ?~ 56
            & Swagger.minLength
          ?~ 56
