{- |
Module      : GeniusYield.Types.KeyHash
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.KeyHash (
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
  GYVRFVerKeyHash,
  vrfVerKeyHashToLedger,
  vrfVerKeyHashFromLedger,
  vrfVerKeyHashToRawBytes,
  vrfVerKeyHashToRawBytesHex,
  vrfVerKeyHashToRawBytesHexText,
  vrfVerKeyHashFromRawBytes,
  vrfVerKeyHashFromRawBytesHex,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Keys qualified as Ledger
import Control.Lens ((?~))
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS
import Data.Csv qualified as Csv
import Data.String (IsString (..))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GeniusYield.Imports (coerce, (&), (>>>))
import GeniusYield.Types.KeyRole (GYKeyRole (..), GYKeyRoleToLedger, GYKeyRoleVRF, SingGYKeyRole (..), SingGYKeyRoleI (..), fromSingGYKeyRole)
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (fromPubKeyHash, toPubKeyHash), CanSignTx, pubKeyHashFromApi, pubKeyHashToApi)
import Text.Printf qualified as Printf

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications -XDataKinds
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import qualified Text.Printf                as Printf
>>> import           Data.Proxy
>>> import qualified Data.Swagger               as Swagger
>>> import GeniusYield.Types.KeyRole
>>> let pkh :: GYKeyHash 'GYKeyRolePayment = "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
-}

-- | Hash of a public key corresponding to a given `GYKeyRole`.
newtype GYKeyHash (kr :: GYKeyRole) = GYKeyHash (Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto)
  deriving newtype (Eq, Ord)

-- | Convert to corresponding ledger representation.
keyHashToLedger :: GYKeyHash kr -> Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto
keyHashToLedger = coerce

-- | Convert from corresponding ledger representation.
keyHashFromLedger :: Ledger.KeyHash (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYKeyHash kr
keyHashFromLedger = coerce

-- >>> "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81" :: (GYKeyHash 'GYKeyRolePayment)
-- GYKeyHash (GYKeyRolePayment) "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
instance IsString (GYKeyHash kr) where
  fromString = BS.pack >>> keyHashFromRawBytesHex >>> either error id

-- >>> pkh
-- GYKeyHash (GYKeyRolePayment) "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
instance SingGYKeyRoleI kr => Show (GYKeyHash kr) where
  showsPrec d kh = showParen (d > 10) $ showString "GYKeyHash (" . shows (fromSingGYKeyRole (singGYKeyRole @kr)) . showString ") " . shows (keyHashToRawBytesHex kh)

-- | Get corresponding raw bytes.
keyHashToRawBytes :: GYKeyHash kr -> BS.ByteString
keyHashToRawBytes kh = Crypto.hashToBytes $ Ledger.unKeyHash $ keyHashToLedger kh

-- | Get corresponding raw bytes represented as hex.
keyHashToRawBytesHex :: GYKeyHash kr -> BS.ByteString
keyHashToRawBytesHex = keyHashToRawBytes >>> Base16.encode

-- | Get corresponding raw bytes represented as hex text.
keyHashToRawBytesHexText :: GYKeyHash kr -> Text
keyHashToRawBytesHexText = keyHashToRawBytesHex >>> Text.decodeUtf8

-- | Decode from raw bytes.
keyHashFromRawBytes :: BS.ByteString -> Maybe (GYKeyHash kr)
keyHashFromRawBytes bs = keyHashFromLedger . Ledger.KeyHash <$> Crypto.hashFromBytes bs

-- | Decode from raw bytes represented as hex.
keyHashFromRawBytesHex :: BS.ByteString -> Either String (GYKeyHash kr)
keyHashFromRawBytesHex bs =
  case Base16.decode bs of
    Left e -> Left $ "GeniusYield.Types.KeyHash.keyHashFromRawBytesHex: unable to decode hash from hex string: " <> BS.unpack bs <> ", error: " <> e
    Right bs' -> case keyHashFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.KeyHash.keyHashFromRawBytesHex: unable to decode hash from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just kh -> Right kh

-- | Convert to corresponding API representation.
type family GYHashToApi (kr :: GYKeyRole) where
  GYHashToApi 'GYKeyRolePayment = Api.Hash Api.PaymentKey
  GYHashToApi 'GYKeyRoleStaking = Api.Hash Api.StakeKey
  GYHashToApi 'GYKeyRoleDRep = Api.Hash Api.DRepKey
  GYHashToApi 'GYKeyRoleStakePool = Api.Hash Api.StakePoolKey
  GYHashToApi 'GYKeyRoleHotCommittee = Api.Hash Api.CommitteeHotKey
  GYHashToApi 'GYKeyRoleColdCommittee = Api.Hash Api.CommitteeColdKey

{- |

>>> keyHashToApi pkh
"ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
-}
keyHashToApi :: forall kr. SingGYKeyRoleI kr => GYKeyHash kr -> GYHashToApi kr
keyHashToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

{- |

>>> keyHashFromApi @'GYKeyRolePayment "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-}
keyHashFromApi :: forall kr. SingGYKeyRoleI kr => GYHashToApi kr -> GYKeyHash kr
keyHashFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

instance AsPubKeyHash (GYKeyHash kr) where
  toPubKeyHash (GYKeyHash kh) = pubKeyHashFromApi $ coerce $ Ledger.coerceKeyRole kh
  fromPubKeyHash = pubKeyHashToApi >>> coerce

instance CanSignTx (GYKeyHash kr)

{- |

>>> LBS8.putStrLn $ Aeson.encode pkh
"ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
-}
instance Aeson.ToJSON (GYKeyHash kr) where
  toJSON = Aeson.toJSON . keyHashToRawBytesHexText

{- |

>>> Aeson.eitherDecode @(GYKeyHash 'GYKeyRolePayment) "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\""
Right (GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

Invalid characters:

>>> Aeson.eitherDecode @(GYKeyHash 'GYKeyRolePayment) "\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz\""
Left "Error in $: \"GeniusYield.Types.KeyHash.keyHashFromRawBytesHex: unable to decode hash from hex string: e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6azzz, error: invalid character at offset: 53\""
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

>>> Printf.printf "%s\n" $ pkh
ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81
-}
instance Printf.PrintfArg (GYKeyHash kr) where
  formatArg = Printf.formatArg . keyHashToRawBytesHexText

{- |

>>> Csv.toField pkh
"ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
-}
instance Csv.ToField (GYKeyHash kr) where
  toField = keyHashToRawBytesHex

{- |

>>> Csv.runParser $ Csv.parseField @(GYKeyHash 'GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
Right (GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d")

>>> Csv.runParser $ Csv.parseField @(GYKeyHash 'GYKeyRolePayment) "not a payment key hash"
Left "\"GeniusYield.Types.KeyHash.keyHashFromRawBytesHex: unable to decode hash from hex string: not a payment key hash, error: invalid character at offset: 0\""
-}
instance Csv.FromField (GYKeyHash kr) where
  parseField = either (fail . show) return . keyHashFromRawBytesHex

{- |

>>> Aeson.encode (Swagger.toSchema (Proxy :: Proxy (GYKeyHash 'GYKeyRolePayment)))
"{\"description\":\"The hash of a key with role as GYKeyRolePayment\",\"example\":\"e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d\",\"format\":\"hex\",\"maxLength\":56,\"minLength\":56,\"type\":\"string\"}"
-}
instance SingGYKeyRoleI kr => Swagger.ToSchema (GYKeyHash kr) where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYKeyHash" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ ("The hash of a key with role as " <> Text.pack (show (fromSingGYKeyRole $ singGYKeyRole @kr)))
            & Swagger.example
          ?~ Aeson.toJSON ("e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d" :: Text)
            & Swagger.maxLength
          ?~ 56
            & Swagger.minLength
          ?~ 56

-- | Hash of a public key corresponding to a given `GYKeyRoleVRF`.
newtype GYVRFVerKeyHash (kr :: GYKeyRoleVRF) = GYVRFVerKeyHash (Ledger.Hash Ledger.StandardCrypto (Ledger.VerKeyVRF Ledger.StandardCrypto))
  deriving newtype (Eq, Ord)

vrfVerKeyHashToLedger :: GYVRFVerKeyHash kr -> Ledger.Hash Ledger.StandardCrypto (Ledger.VerKeyVRF Ledger.StandardCrypto)
vrfVerKeyHashToLedger = coerce

vrfVerKeyHashFromLedger :: Ledger.Hash Ledger.StandardCrypto (Ledger.VerKeyVRF Ledger.StandardCrypto) -> GYVRFVerKeyHash kr
vrfVerKeyHashFromLedger = coerce

-- >>> "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81" :: (GYVRFVerKeyHash 'GYKeyRoleVRFStakePool)
-- GYVRFVerKeyHash (GYKeyRoleVRFStakePool) "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
instance IsString (GYVRFVerKeyHash kr) where
  fromString = BS.pack >>> vrfVerKeyHashFromRawBytesHex >>> either error id

-- TODO: This (Show instance) is not specialised using singletons.
-- >>> pkh
-- GYVRFVerKeyHash (GYKeyRoleVRFStakePool) "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
instance Show (GYVRFVerKeyHash kr) where
  showsPrec d kh = showParen (d > 10) $ showString "GYVRFVerKeyHash (GYKeyRoleVRFStakePool) " . shows (vrfVerKeyHashToRawBytesHex kh)

-- | Get corresponding raw bytes.
vrfVerKeyHashToRawBytes :: GYVRFVerKeyHash kr -> BS.ByteString
vrfVerKeyHashToRawBytes kh = Crypto.hashToBytes $ vrfVerKeyHashToLedger kh

-- | Get corresponding raw bytes represented as hex.
vrfVerKeyHashToRawBytesHex :: GYVRFVerKeyHash kr -> BS.ByteString
vrfVerKeyHashToRawBytesHex = vrfVerKeyHashToRawBytes >>> Base16.encode

-- | Get corresponding raw bytes represented as hex text.
vrfVerKeyHashToRawBytesHexText :: GYVRFVerKeyHash kr -> Text
vrfVerKeyHashToRawBytesHexText = vrfVerKeyHashToRawBytesHex >>> Text.decodeUtf8

-- | Decode from raw bytes.
vrfVerKeyHashFromRawBytes :: BS.ByteString -> Maybe (GYVRFVerKeyHash kr)
vrfVerKeyHashFromRawBytes bs = vrfVerKeyHashFromLedger <$> Crypto.hashFromBytes bs

-- | Decode from raw bytes represented as hex.
vrfVerKeyHashFromRawBytesHex :: BS.ByteString -> Either String (GYVRFVerKeyHash kr)
vrfVerKeyHashFromRawBytesHex bs =
  case Base16.decode bs of
    Left e -> Left $ "GeniusYield.Types.KeyHash.vrfVerKeyHashFromRawBytesHex: unable to decode hash from hex string: " <> BS.unpack bs <> ", error: " <> e
    Right bs' -> case vrfVerKeyHashFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.KeyHash.vrfVerKeyHashFromRawBytesHex: unable to decode hash from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just kh -> Right kh
