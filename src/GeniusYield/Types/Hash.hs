{- |
Module      : GeniusYield.Types.Hash
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Hash (
  GYKeyHash,
  keyHashToLedger,
  keyHashFromLedger,
  keyHashToApi,
  keyHashFromApi,
  keyHashToRawBytes,
  keyHashToRawBytesHex,
  keyHashToRawBytesHexText,
  keyHashFromRawBytes,
  keyHashFromRawBytesHex,
  GYPaymentKeyHash,
  paymentKeyHashToApi,
  paymentKeyHashFromApi,
  paymentKeyHashToLedger,
  paymentKeyHashFromLedger,
  paymentKeyHashFromPlutus,
  paymentKeyHashToPlutus,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Crypto.DSIGN.Class qualified as Crypto
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Seed qualified as Crypto
import Cardano.Ledger.Keys qualified as Ledger
import Control.Lens ((?~))
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS
import Data.Csv qualified as Csv
import Data.Either.Combinators (maybeToRight)
import Data.Hashable (Hashable (..))
import Data.String (IsString (..))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports (bimap, coerce, (&), (>>>))
import GeniusYield.Types.KeyRole (GYKeyRole (..), GYKeyRoleToLedger, SingGYKeyRole (..), SingGYKeyRoleI (..), fromSingGYKeyRole)
import GeniusYield.Types.Ledger
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (fromPubKeyHash, toPubKeyHash), CanSignTx, pubKeyHashFromApi, pubKeyHashFromPlutus, pubKeyHashToApi, pubKeyHashToPlutus)
import GeniusYield.Types.StakeKeyHash (
  GYStakeKeyHash,
  stakeKeyHashFromApi,
  stakeKeyHashToApi,
 )
import GeniusYield.Utils (serialiseToBech32WithPrefix)
import PlutusLedgerApi.V1 qualified as Plutus (Credential (..))
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

newtype GYKeyHash (kr :: GYKeyRole) = GYKeyHash (Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto)
  deriving newtype (Eq, Ord)

keyHashToLedger :: GYKeyHash kr -> Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto
keyHashToLedger = coerce

keyHashFromLedger :: Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYKeyHash kr
keyHashFromLedger = coerce

-- >>> "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81" :: (GYKeyHash 'GYKeyRolePayment)
-- GYKeyHash (GYKeyRolePayment) "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
instance IsString (GYKeyHash kr) where
  fromString = BS.pack >>> keyHashFromRawBytesHex >>> either error id

instance SingGYKeyRoleI kr => Show (GYKeyHash kr) where
  show kh = "GYKeyHash (" <> show (fromSingGYKeyRole (singGYKeyRole @kr)) <> ") " <> show (keyHashToRawBytesHex kh)

keyHashToRawBytes :: GYKeyHash kr -> BS.ByteString
keyHashToRawBytes kh = Crypto.hashToBytes $ Ledger.unKeyHash $ keyHashToLedger kh

keyHashToRawBytesHex :: GYKeyHash kr -> BS.ByteString
keyHashToRawBytesHex = keyHashToRawBytes >>> Base16.encode

keyHashToRawBytesHexText :: GYKeyHash kr -> Text
keyHashToRawBytesHexText = keyHashToRawBytesHex >>> Text.decodeUtf8

keyHashFromRawBytes :: BS.ByteString -> Maybe (GYKeyHash kr)
keyHashFromRawBytes bs = keyHashFromLedger . Ledger.KeyHash <$> Crypto.hashFromBytes bs

keyHashFromRawBytesHex :: BS.ByteString -> Either String (GYKeyHash kr)
keyHashFromRawBytesHex bs =
  case Base16.decode bs of
    Left e -> Left $ "GeniusYield.Types.Hash.keyHashFromRawBytesHex: unable to decode hash from hex string: " <> BS.unpack bs <> ", error: " <> e
    Right bs' -> case keyHashFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.Hash.keyHashFromRawBytesHex: unable to decode hash from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just kh -> Right kh

type family GYHashToApi (kr :: GYKeyRole) where
  GYHashToApi 'GYKeyRolePayment = Api.Hash Api.PaymentKey
  GYHashToApi 'GYKeyRoleStaking = Api.Hash Api.StakeKey
  GYHashToApi 'GYKeyRoleDRep = Api.Hash Api.DRepKey

keyHashToApi :: forall kr. SingGYKeyRoleI kr => GYKeyHash kr -> GYHashToApi kr
keyHashToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce

keyHashFromApi :: forall kr. SingGYKeyRoleI kr => GYHashToApi kr -> GYKeyHash kr
keyHashFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce

instance AsPubKeyHash (GYKeyHash kr) where
  toPubKeyHash (GYKeyHash kh) = pubKeyHashFromApi $ coerce $ Ledger.coerceKeyRole kh
  fromPubKeyHash = pubKeyHashToApi >>> coerce

instance CanSignTx (GYKeyHash kr)

{- |

>>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> LBS8.putStrLn $ Aeson.encode pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Aeson.ToJSON (GYKeyHash kr) where
  toJSON = Aeson.toJSON . keyHashToRawBytesHexText

{- |

>>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
Right GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"

Invalid characters:

>>> Aeson.eitherDecode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
Left "Error in $: \"GeniusYield.Types.Hash.keyHashFromRawBytesHex: unable to decode hash from hex string: e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz, error: invalid character at offset: 53\""
-}
instance SingGYKeyRoleI kr => Aeson.FromJSON (GYKeyHash kr) where
  parseJSON =
    Aeson.withText ("GYKeyHash (" <> show (fromSingGYKeyRole $ singGYKeyRole @kr) <> ")") $
      either
        (fail . show)
        return
        . keyHashFromRawBytesHex
        . Text.encodeUtf8

{- |

>>> Printf.printf "%s\n" $ paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d
-}
instance Printf.PrintfArg (GYKeyHash kr) where
  formatArg = Printf.formatArg . keyHashToRawBytesHexText

{- |

>>> Csv.toField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
instance Csv.ToField (GYKeyHash kr) where
  toField = keyHashToRawBytesHex

{- |

>>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"

>>> Csv.runParser $ Csv.parseField @GYPaymentKeyHash "not a payment key hash"
Left "\"GeniusYield.Types.Hash.keyHashFromRawBytesHex: unable to decode hash from hex string: not a payment key hash, error: invalid character at offset: 0\""
-}
instance Csv.FromField (GYKeyHash kr) where
  parseField = either (fail . show) return . keyHashFromRawBytesHex

instance SingGYKeyRoleI kr => Swagger.ToSchema (GYKeyHash kr) where
  declareNamedSchema _ =
    pure $
      Swagger.named ("GYKeyHash (" <> Text.pack (show (fromSingGYKeyRole $ singGYKeyRole @kr) <> ")")) $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "The hash of a key."
            & Swagger.example
          ?~ Aeson.toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
            & Swagger.maxLength
          ?~ 56
            & Swagger.minLength
          ?~ 56

--------------------------------------------------------------------------------

type GYPaymentKeyHash = GYKeyHash 'GYKeyRolePayment

paymentKeyHashFromPlutus :: Plutus.PubKeyHash -> Either PlutusToCardanoError GYPaymentKeyHash
paymentKeyHashFromPlutus = fmap fromPubKeyHash . pubKeyHashFromPlutus

paymentKeyHashToPlutus :: GYPaymentKeyHash -> Plutus.PubKeyHash
paymentKeyHashToPlutus = toPubKeyHash >>> pubKeyHashToPlutus

{- |

>>> let Just pkh = Aeson.decode @GYPaymentKeyHash "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
>>> paymentKeyHashToApi pkh
"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
paymentKeyHashToApi :: GYPaymentKeyHash -> Api.Hash Api.PaymentKey
paymentKeyHashToApi = keyHashToApi

{- |

>>> paymentKeyHashFromApi "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
GYPaymentKeyHash "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
paymentKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPaymentKeyHash
paymentKeyHashFromApi = keyHashFromApi

-- | Convert to corresponding ledger representation.
paymentKeyHashToLedger :: GYPaymentKeyHash -> Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto
paymentKeyHashToLedger = keyHashToLedger

-- | Convert from corresponding ledger representation.
paymentKeyHashFromLedger :: Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto -> GYPaymentKeyHash
paymentKeyHashFromLedger = keyHashFromLedger
