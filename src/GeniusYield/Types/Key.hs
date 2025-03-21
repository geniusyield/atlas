{- |
Module      : GeniusYield.Types.Key
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Key (
  ToShelleyWitnessSigningKey (..),

  -- * Signing key
  GYSigningKey,
  signingKeyToLedger,
  signingKeyFromLedger,
  signingKeyToRawBytes,
  signingKeyToRawBytesHex,
  signingKeyToRawBytesHexText,
  signingKeyFromRawBytes,
  signingKeyFromRawBytesHex,
  GYSigningKeyToApi,
  signingKeyToApi,
  signingKeyFromApi,
  generateSigningKey,
  writeSigningKey,
  readSigningKey,
  signingKeyToLedgerKeyPair,
  signingKeyFromLedgerKeyPair,

  -- * Verification key
  GYVerificationKey,
  verificationKeyToLedger,
  verificationKeyFromLedger,
  getVerificationKey,
  verificationKeyHash,
  verificationKeyToRawBytes,
  verificationKeyToRawBytesHex,
  verificationKeyToRawBytesHexText,
  verificationKeyFromRawBytes,
  verificationKeyFromRawBytesHex,
  GYVerificationKeyToApi,
  verificationKeyToApi,
  verificationKeyFromApi,

  -- * Extended signing key
  GYExtendedSigningKey,
  extendedSigningKeyToRawBytes,
  extendedSigningKeyToRawBytesHex,
  extendedSigningKeyToRawBytesHexText,
  extendedSigningKeyFromRawBytes,
  extendedSigningKeyFromRawBytesHex,
  GYExtendedSigningKeyToApi,
  extendedSigningKeyToApi,
  extendedSigningKeyFromApi,
  writeExtendedSigningKey,
  readExtendedSigningKey,

  -- * Extended verification key
  GYExtendedVerificationKey,
  getExtendedVerificationKey,
  extendedVerificationKeyHash,
  extendedVerificationKeyToRawBytes,
  extendedVerificationKeyToRawBytesHex,
  extendedVerificationKeyToRawBytesHexText,
  extendedVerificationKeyFromRawBytes,
  extendedVerificationKeyFromRawBytesHex,
  GYExtendedVerificationKeyToApi,
  extendedVerificationKeyToApi,
  extendedVerificationKeyFromApi,

  -- * Payment verification key
  GYPaymentVerificationKey,
  paymentVerificationKeyFromApi,
  paymentVerificationKeyToApi,
  paymentVerificationKeyToLedger,
  paymentVerificationKeyRawBytes,
  pubKeyHash,
  paymentKeyHash,

  -- * Payment signing key
  GYPaymentSigningKey,
  GYExtendedPaymentSigningKey,
  paymentSigningKeyFromApi,
  extendedPaymentSigningKeyFromApi,
  paymentSigningKeyToApi,
  extendedPaymentSigningKeyToApi,
  paymentSigningKeyToLedger,
  paymentSigningKeyToLedgerKeyPair,
  paymentSigningKeyFromLedgerKeyPair,
  readPaymentSigningKey,
  readExtendedPaymentSigningKey,
  writePaymentSigningKey,
  writeExtendedPaymentSigningKey,
  paymentVerificationKey,
  generatePaymentSigningKey,

  -- * Stake verification key
  GYStakeVerificationKey,
  stakeVerificationKeyFromApi,
  stakeVerificationKeyToApi,
  stakeKeyHash,
  stakeVerificationKeyToLedger,

  -- * Stake signing key
  GYStakeSigningKey,
  GYExtendedStakeSigningKey,
  stakeSigningKeyFromApi,
  extendedStakeSigningKeyFromApi,
  stakeSigningKeyToApi,
  extendedStakeSigningKeyToApi,
  stakeSigningKeyToLedger,
  stakeSigningKeyToLedgerKeyPair,
  stakeSigningKeyFromLedgerKeyPair,
  readStakeSigningKey,
  readExtendedStakeSigningKey,
  writeStakeSigningKey,
  writeExtendedStakeSigningKey,
  stakeVerificationKey,
  generateStakeSigningKey,
  GYSomeSigningKey (..),
  GYSomePaymentSigningKey (..),
  readSomePaymentSigningKey,
  somePaymentSigningKeyToSomeSigningKey,
  GYSomeStakeSigningKey (..),
  readSomeStakeSigningKey,
  someStakeSigningKeyToSomeSigningKey,
) where

import Cardano.Api qualified as Api
import Cardano.Api.SerialiseTextEnvelope qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Crypto.Wallet qualified as Crypto.HD
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as Csv
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Cardano.Ledger.Core.KeyPair qualified as TLedger
import Text.Printf qualified as Printf

import Cardano.Crypto.DSIGN qualified as Crypto
import GeniusYield.Imports
import GeniusYield.Types.Key.Class (
  ToShelleyWitnessSigningKey,
  toShelleyWitnessSigningKey,
 )
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import GeniusYield.Types.PaymentKeyHash (
  GYPaymentKeyHash,
 )
import GeniusYield.Types.PubKeyHash (
  GYPubKeyHash,
  pubKeyHashFromApi,
 )
import GeniusYield.Types.StakeKeyHash (
  GYStakeKeyHash,
 )

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import           Data.Either                (isRight)
>>> import           GeniusYield.Types
>>> import qualified Text.Printf                as Printf
-}

{- |

>>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: (GYSigningKey 'GYKeyRolePayment)
GYSigningKey (GYKeyRolePayment) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
newtype GYSigningKey (kr :: GYKeyRole) = GYSigningKey (Ledger.SignKeyDSIGN Ledger.StandardCrypto)

instance SingGYKeyRoleI kr => Show (GYSigningKey kr) where
  showsPrec d k = showParen (d > 10) $ showString "GYSigningKey (" . shows (fromSingGYKeyRole (singGYKeyRole @kr)) . showString ") " . shows (signingKeyToRawBytesHex k)

instance IsString (GYSigningKey kr) where
  fromString = BS8.pack >>> signingKeyFromRawBytesHex >>> either error id

instance SingGYKeyRoleI kr => Eq (GYSigningKey kr) where
  (==) = (==) `on` show

instance SingGYKeyRoleI kr => Ord (GYSigningKey kr) where
  compare = compare `on` show

{- |

>>> LBS8.putStrLn $ Aeson.encode ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYSigningKeyToApi kr)) => Aeson.ToJSON (GYSigningKey kr) where
  toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . signingKeyToApi

{- |

>>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290\""
Right (GYSigningKey (GYKeyRolePayment) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290")

>>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fceczzz\""
Left "Error in $: invalid character at offset: 65"
-}
instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYSigningKeyToApi kr)) => Aeson.FromJSON (GYSigningKey kr) where
  parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
    Left err -> fail err
    Right bs -> case Api.deserialiseFromCBOR (Api.proxyToAsType Proxy) bs of
      Left err -> fail $ show err
      Right skey -> return $ signingKeyFromApi skey
  parseJSON _ = fail "signing key expected"

instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYSigningKeyToApi kr)) => Csv.ToField (GYSigningKey kr) where
  toField = LBS.toStrict . Aeson.encode

instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYSigningKeyToApi kr)) => Csv.FromField (GYSigningKey kr) where
  parseField k =
    case Aeson.decode $ LBS.fromStrict k of
      Just v -> pure v
      Nothing -> fail $ "Error Parsing signingKey from CSV: " <> show k

{- |

>>> Printf.printf "%s\n" ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290
-}
instance Printf.PrintfArg (GYSigningKey kr) where
  formatArg = Printf.formatArg . signingKeyToRawBytesHexText

-- TODO: use coerce, for some reason it's giving weird error.
signingKeyToLedger :: GYSigningKey kr -> Ledger.SignKeyDSIGN Ledger.StandardCrypto
signingKeyToLedger (GYSigningKey k) = k

-- TODO: use coerce.
signingKeyFromLedger :: Ledger.SignKeyDSIGN Ledger.StandardCrypto -> GYSigningKey kr
signingKeyFromLedger = GYSigningKey

signingKeyToRawBytes :: GYSigningKey kr -> BS8.ByteString
signingKeyToRawBytes = signingKeyToLedger >>> Crypto.rawSerialiseSignKeyDSIGN

signingKeyToRawBytesHex :: GYSigningKey kr -> BS8.ByteString
signingKeyToRawBytesHex = signingKeyToRawBytes >>> BS16.encode

signingKeyToRawBytesHexText :: GYSigningKey kr -> T.Text
signingKeyToRawBytesHexText = signingKeyToRawBytesHex >>> TE.decodeUtf8

-- | Decode from raw bytes.
signingKeyFromRawBytes :: BS8.ByteString -> Maybe (GYSigningKey kr)
signingKeyFromRawBytes bs = signingKeyFromLedger <$> Crypto.rawDeserialiseSignKeyDSIGN bs

-- | Decode from raw bytes represented as hex.
signingKeyFromRawBytesHex :: BS8.ByteString -> Either String (GYSigningKey kr)
signingKeyFromRawBytesHex bs =
  case BS16.decode bs of
    Left e -> Left $ "GeniusYield.Types.Key.signingKeyFromRawBytesHex: unable to decode from hex string: " <> BS8.unpack bs <> ", error: " <> e
    Right bs' -> case signingKeyFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.Key.signingKeyFromRawBytesHex: unable to decode from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just k -> Right k

type family GYSigningKeyToApi (kr :: GYKeyRole) where
  GYSigningKeyToApi 'GYKeyRolePayment = Api.SigningKey Api.PaymentKey
  GYSigningKeyToApi 'GYKeyRoleStaking = Api.SigningKey Api.StakeKey
  GYSigningKeyToApi 'GYKeyRoleDRep = Api.SigningKey Api.DRepKey
  GYSigningKeyToApi 'GYKeyRoleStakePool = Api.SigningKey Api.StakePoolKey
  GYSigningKeyToApi 'GYKeyRoleHotCommittee = Api.SigningKey Api.CommitteeHotKey
  GYSigningKeyToApi 'GYKeyRoleColdCommittee = Api.SigningKey Api.CommitteeColdKey

signingKeyToApi :: forall kr. SingGYKeyRoleI kr => GYSigningKey kr -> GYSigningKeyToApi kr
signingKeyToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

signingKeyFromApi :: forall kr. SingGYKeyRoleI kr => GYSigningKeyToApi kr -> GYSigningKey kr
signingKeyFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

instance SingGYKeyRoleI kr => ToShelleyWitnessSigningKey (GYSigningKey kr) where
  toShelleyWitnessSigningKey (signingKeyToApi -> sk) = case singGYKeyRole @kr of
    SingGYKeyRolePayment -> Api.WitnessPaymentKey sk
    SingGYKeyRoleStaking -> Api.WitnessStakeKey sk
    SingGYKeyRoleDRep -> Api.WitnessDRepKey sk
    SingGYKeyRoleStakePool -> Api.WitnessStakePoolKey sk
    SingGYKeyRoleHotCommittee -> Api.WitnessCommitteeHotKey sk
    SingGYKeyRoleColdCommittee -> Api.WitnessCommitteeColdKey sk

generateSigningKey :: forall kr. SingGYKeyRoleI kr => IO (GYSigningKey kr)
generateSigningKey =
  signingKeyFromApi <$> case singGYKeyRole @kr of
    SingGYKeyRolePayment -> Api.generateSigningKey (Api.proxyToAsType Proxy)
    SingGYKeyRoleStaking -> Api.generateSigningKey (Api.proxyToAsType Proxy)
    SingGYKeyRoleDRep -> Api.generateSigningKey (Api.proxyToAsType Proxy)
    SingGYKeyRoleStakePool -> Api.generateSigningKey (Api.proxyToAsType Proxy)
    SingGYKeyRoleHotCommittee -> Api.generateSigningKey (Api.proxyToAsType Proxy)
    SingGYKeyRoleColdCommittee -> Api.generateSigningKey (Api.proxyToAsType Proxy)

writeSigningKey :: forall kr. (SingGYKeyRoleI kr, Api.HasTextEnvelope (GYSigningKeyToApi kr)) => FilePath -> GYSigningKey kr -> IO ()
writeSigningKey file key = do
  e <- Api.writeFileTextEnvelope (Api.File file) (Just $ Api.TextEnvelopeDescr $ "Signing Key (" <> show (fromSingGYKeyRole (singGYKeyRole @kr)) <> ")") $ signingKeyToApi key
  case e of
    Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
    Right () -> return ()

readSigningKey :: forall kr. (SingGYKeyRoleI kr, Api.HasTextEnvelope (GYSigningKeyToApi kr)) => FilePath -> IO (GYSigningKey kr)
readSigningKey fp = do
  s <- Api.readFileTextEnvelopeAnyOf asTypes (Api.File fp)
  case s of
    Left err -> fail (show err) --- throws IOError
    Right x -> return (signingKeyFromApi x)
 where
  asTypes =
    case singGYKeyRole @kr of
      SingGYKeyRolePayment -> [Api.FromSomeType (Api.proxyToAsType Proxy) id, Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey) Api.castSigningKey]
      SingGYKeyRoleStaking -> [Api.FromSomeType (Api.proxyToAsType Proxy) id]
      SingGYKeyRoleDRep -> [Api.FromSomeType (Api.proxyToAsType Proxy) id]
      SingGYKeyRoleStakePool -> [Api.FromSomeType (Api.proxyToAsType Proxy) id, Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey) Api.castSigningKey]
      SingGYKeyRoleHotCommittee -> [Api.FromSomeType (Api.proxyToAsType Proxy) id]
      SingGYKeyRoleColdCommittee -> [Api.FromSomeType (Api.proxyToAsType Proxy) id]

signingKeyToLedgerKeyPair :: GYSigningKey kr -> TLedger.KeyPair (GYKeyRoleToLedger kr) Ledger.StandardCrypto
signingKeyToLedgerKeyPair skey =
  TLedger.KeyPair
    { TLedger.vKey = verificationKeyToLedger $ getVerificationKey skey
    , TLedger.sKey = signingKeyToLedger skey
    }

signingKeyFromLedgerKeyPair :: TLedger.KeyPair (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYSigningKey kr
signingKeyFromLedgerKeyPair = signingKeyFromLedger . TLedger.sKey

newtype GYVerificationKey (kr :: GYKeyRole) = GYVerificationKey (Ledger.VKey (GYKeyRoleToLedger kr) Ledger.StandardCrypto)
  deriving newtype Eq

instance SingGYKeyRoleI kr => Show (GYVerificationKey kr) where
  showsPrec d k = showParen (d > 10) $ showString "GYVerificationKey (" . shows (fromSingGYKeyRole (singGYKeyRole @kr)) . showString ") " . shows (verificationKeyToRawBytesHex k)

instance IsString (GYVerificationKey kr) where
  fromString = BS8.pack >>> verificationKeyFromRawBytesHex >>> either error id

{- |

>>> LBS8.putStrLn $ Aeson.encode ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
instance
  ( SingGYKeyRoleI kr
  , Api.SerialiseAsCBOR (GYVerificationKeyToApi kr)
  ) =>
  Aeson.ToJSON (GYVerificationKey kr)
  where
  toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . verificationKeyToApi

{- |

>>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605\""
Right (GYVerificationKey (GYKeyRolePayment) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")

>>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193zzz\""
Left "Error in $: invalid character at offset: 65"
-}
instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYVerificationKeyToApi kr)) => Aeson.FromJSON (GYVerificationKey kr) where
  parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
    Left err -> fail err
    Right bs -> case Api.deserialiseFromCBOR (Api.proxyToAsType Proxy) bs of
      Left err -> fail $ show err
      Right skey -> return $ verificationKeyFromApi skey
  parseJSON _ = fail "verification key expected"

{- |

>>> Printf.printf "%s\n" ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605
-}
instance Printf.PrintfArg (GYVerificationKey kr) where
  formatArg = Printf.formatArg . verificationKeyToRawBytesHexText

verificationKeyToLedger :: GYVerificationKey kr -> Ledger.VKey (GYKeyRoleToLedger kr) Ledger.StandardCrypto
verificationKeyToLedger = coerce

verificationKeyFromLedger :: Ledger.VKey (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYVerificationKey kr
verificationKeyFromLedger = coerce

getVerificationKey :: GYSigningKey kr -> GYVerificationKey kr
getVerificationKey = signingKeyToLedger >>> (Ledger.VKey . Crypto.deriveVerKeyDSIGN) >>> verificationKeyFromLedger

verificationKeyHash :: GYVerificationKey kr -> GYKeyHash kr
verificationKeyHash = keyHashFromLedger . Ledger.hashKey . verificationKeyToLedger

verificationKeyToRawBytes :: GYVerificationKey kr -> BS8.ByteString
verificationKeyToRawBytes = verificationKeyToLedger >>> (\(Ledger.VKey vk) -> Crypto.rawSerialiseVerKeyDSIGN vk)

verificationKeyToRawBytesHex :: GYVerificationKey kr -> BS8.ByteString
verificationKeyToRawBytesHex = verificationKeyToRawBytes >>> BS16.encode

verificationKeyToRawBytesHexText :: GYVerificationKey kr -> T.Text
verificationKeyToRawBytesHexText = verificationKeyToRawBytesHex >>> TE.decodeUtf8

-- | Decode from raw bytes.
verificationKeyFromRawBytes :: BS8.ByteString -> Maybe (GYVerificationKey kr)
verificationKeyFromRawBytes bs = verificationKeyFromLedger . Ledger.VKey <$> Crypto.rawDeserialiseVerKeyDSIGN bs

-- | Decode from raw bytes represented as hex.
verificationKeyFromRawBytesHex :: BS8.ByteString -> Either String (GYVerificationKey kr)
verificationKeyFromRawBytesHex bs =
  case BS16.decode bs of
    Left e -> Left $ "GeniusYield.Types.Key.verificationKeyFromRawBytesHex: unable to decode from hex string: " <> BS8.unpack bs <> ", error: " <> e
    Right bs' -> case verificationKeyFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.Key.verificationKeyFromRawBytesHex: unable to decode from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just k -> Right k

type family GYVerificationKeyToApi (kr :: GYKeyRole) where
  GYVerificationKeyToApi 'GYKeyRolePayment = Api.VerificationKey Api.PaymentKey
  GYVerificationKeyToApi 'GYKeyRoleStaking = Api.VerificationKey Api.StakeKey
  GYVerificationKeyToApi 'GYKeyRoleDRep = Api.VerificationKey Api.DRepKey
  GYVerificationKeyToApi 'GYKeyRoleStakePool = Api.VerificationKey Api.StakePoolKey
  GYVerificationKeyToApi 'GYKeyRoleHotCommittee = Api.VerificationKey Api.CommitteeHotKey
  GYVerificationKeyToApi 'GYKeyRoleColdCommittee = Api.VerificationKey Api.CommitteeColdKey

verificationKeyToApi :: forall kr. SingGYKeyRoleI kr => GYVerificationKey kr -> GYVerificationKeyToApi kr
verificationKeyToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

verificationKeyFromApi :: forall kr. SingGYKeyRoleI kr => GYVerificationKeyToApi kr -> GYVerificationKey kr
verificationKeyFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> coerce
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

-- | Extended signing key.
newtype GYExtendedSigningKey (kr :: GYKeyRole) = GYExtendedSigningKey Crypto.HD.XPrv

instance SingGYKeyRoleI kr => Show (GYExtendedSigningKey kr) where
  showsPrec d k = showParen (d > 10) $ showString "GYExtendedSigningKey (" . shows (fromSingGYKeyRole (singGYKeyRole @kr)) . showString ") " . shows (extendedSigningKeyToRawBytesHex k)

instance IsString (GYExtendedSigningKey kr) where
  fromString = BS8.pack >>> extendedSigningKeyFromRawBytesHex >>> either error id

instance SingGYKeyRoleI kr => Eq (GYExtendedSigningKey kr) where
  (==) = (==) `on` show

instance SingGYKeyRoleI kr => Ord (GYExtendedSigningKey kr) where
  compare = compare `on` show

extendedSigningKeyToRawBytes :: GYExtendedSigningKey kr -> BS8.ByteString
extendedSigningKeyToRawBytes (GYExtendedSigningKey xprv) = Crypto.HD.unXPrv xprv

extendedSigningKeyToRawBytesHex :: GYExtendedSigningKey kr -> BS8.ByteString
extendedSigningKeyToRawBytesHex = extendedSigningKeyToRawBytes >>> BS16.encode

extendedSigningKeyToRawBytesHexText :: GYExtendedSigningKey kr -> T.Text
extendedSigningKeyToRawBytesHexText = extendedSigningKeyToRawBytesHex >>> TE.decodeUtf8

-- | Decode from raw bytes.
extendedSigningKeyFromRawBytes :: BS8.ByteString -> Maybe (GYExtendedSigningKey kr)
extendedSigningKeyFromRawBytes bs = case GYExtendedSigningKey <$> Crypto.HD.xprv bs of
  Left _ -> Nothing
  Right k -> Just k

-- | Decode from raw bytes represented as hex.
extendedSigningKeyFromRawBytesHex :: BS8.ByteString -> Either String (GYExtendedSigningKey kr)
extendedSigningKeyFromRawBytesHex bs =
  case BS16.decode bs of
    Left e -> Left $ "GeniusYield.Types.Key.extendedSigningKeyFromRawBytesHex: unable to decode from hex string: " <> BS8.unpack bs <> ", error: " <> e
    Right bs' -> case extendedSigningKeyFromRawBytes bs' of
      Nothing -> Left $ "GeniusYield.Types.Key.extendedSigningKeyFromRawBytesHex: unable to decode from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs'
      Just k -> Right k

type family GYExtendedSigningKeyToApi (kr :: GYKeyRole) where
  GYExtendedSigningKeyToApi 'GYKeyRolePayment = Api.SigningKey Api.PaymentExtendedKey
  GYExtendedSigningKeyToApi 'GYKeyRoleStaking = Api.SigningKey Api.StakeExtendedKey
  GYExtendedSigningKeyToApi 'GYKeyRoleDRep = Api.SigningKey Api.DRepExtendedKey
  GYExtendedSigningKeyToApi 'GYKeyRoleHotCommittee = Api.SigningKey Api.CommitteeHotExtendedKey
  GYExtendedSigningKeyToApi 'GYKeyRoleColdCommittee = Api.SigningKey Api.CommitteeColdExtendedKey

extendedSigningKeyToApi :: forall kr. SingGYKeyRoleI kr => GYExtendedSigningKey kr -> GYExtendedSigningKeyToApi kr
extendedSigningKeyToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> error "extendedSigningKeyToApi: Impossible, no API representation for extended stake pool key"
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

extendedSigningKeyFromApi :: forall kr. SingGYKeyRoleI kr => GYExtendedSigningKeyToApi kr -> GYExtendedSigningKey kr
extendedSigningKeyFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> error "extendedSigningKeyFromApi: Impossible, no API representation for extended stake pool key"
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

instance SingGYKeyRoleI kr => ToShelleyWitnessSigningKey (GYExtendedSigningKey kr) where
  toShelleyWitnessSigningKey (extendedSigningKeyToApi -> sk) = case singGYKeyRole @kr of
    SingGYKeyRolePayment -> Api.WitnessPaymentExtendedKey sk
    SingGYKeyRoleStaking -> Api.WitnessStakeExtendedKey sk
    SingGYKeyRoleDRep -> Api.WitnessDRepExtendedKey sk
    SingGYKeyRoleStakePool -> error "toShelleyWitnessSigningKey: Impossible, no API representation for extended stake pool key"
    SingGYKeyRoleHotCommittee -> Api.WitnessCommitteeHotExtendedKey sk
    SingGYKeyRoleColdCommittee -> Api.WitnessCommitteeColdExtendedKey sk

writeExtendedSigningKey :: forall kr. (SingGYKeyRoleI kr, Api.HasTextEnvelope (GYExtendedSigningKeyToApi kr)) => FilePath -> GYExtendedSigningKey kr -> IO ()
writeExtendedSigningKey file key = do
  e <- Api.writeFileTextEnvelope (Api.File file) (Just $ Api.TextEnvelopeDescr $ "Extended Signing Key (" <> show (fromSingGYKeyRole (singGYKeyRole @kr)) <> ")") $ extendedSigningKeyToApi key
  case e of
    Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
    Right () -> return ()

readExtendedSigningKey :: forall kr. (SingGYKeyRoleI kr, Api.HasTextEnvelope (GYExtendedSigningKeyToApi kr)) => FilePath -> IO (GYExtendedSigningKey kr)
readExtendedSigningKey fp = do
  s <- Api.readFileTextEnvelope asType (Api.File fp)
  case s of
    Left err -> fail (show err) --- throws IOError
    Right x -> return (extendedSigningKeyFromApi x)
 where
  asType =
    case singGYKeyRole @kr of
      SingGYKeyRolePayment -> Api.proxyToAsType Proxy
      SingGYKeyRoleStaking -> Api.proxyToAsType Proxy
      SingGYKeyRoleDRep -> Api.proxyToAsType Proxy
      SingGYKeyRoleStakePool -> error "readExtendedSigningKey: Impossible, no API representation for extended stake pool key"
      SingGYKeyRoleHotCommittee -> Api.proxyToAsType Proxy
      SingGYKeyRoleColdCommittee -> Api.proxyToAsType Proxy

newtype GYExtendedVerificationKey (kr :: GYKeyRole) = GYExtendedVerificationKey Crypto.HD.XPub
  deriving newtype Eq

instance SingGYKeyRoleI kr => Show (GYExtendedVerificationKey kr) where
  showsPrec d k = showParen (d > 10) $ showString "GYExtendedVerificationKey (" . shows (fromSingGYKeyRole (singGYKeyRole @kr)) . showString ") " . shows (extendedVerificationKeyToRawBytesHex k)

instance IsString (GYExtendedVerificationKey kr) where
  fromString = BS8.pack >>> extendedVerificationKeyFromRawBytesHex >>> either error id

{- |

>>> LBS8.putStrLn $ Aeson.encode ("4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e" :: GYExtendedVerificationKey 'GYKeyRolePayment)
"58404081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e"
-}
instance
  ( SingGYKeyRoleI kr
  , Api.SerialiseAsCBOR (GYExtendedVerificationKeyToApi kr)
  ) =>
  Aeson.ToJSON (GYExtendedVerificationKey kr)
  where
  toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . extendedVerificationKeyToApi

{- |

>>> Aeson.eitherDecode @(GYExtendedVerificationKey 'GYKeyRolePayment) "\"4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e\""
Right (GYExtendedVerificationKey (GYKeyRolePayment) "4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e")

>>> Aeson.eitherDecode @(GYExtendedVerificationKey 'GYKeyRolePayment) "\"4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e26\""
Left "Error in $: GeniusYield.Types.Key.extendedVerificationKeyFromRawBytesHex: unable to decode from bytes, given hex string \"4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e26\", corresponding bytes \"@\\129\\176\\160;+f\\199N\\209\\176\\222\\GS\\135N\\216 \\195\\ESC,\\f\\187\\166\\&2\\244\\241\\202\\130\\161\\DC3\\172~\\CAN\\245\\234\\218\\226:\\\\Ne\\230^\\157N:\\GS\\SO\\\\T\\240\\224\\192Nn\\133\\225\\247\\234u\\219|\\159\\RS&\", error: error: xprv needs to be 64 bytes: got 65 bytes"
-}
instance (SingGYKeyRoleI kr, Api.SerialiseAsCBOR (GYExtendedVerificationKeyToApi kr)) => Aeson.FromJSON (GYExtendedVerificationKey kr) where
  parseJSON (Aeson.String t) = either fail return $ extendedVerificationKeyFromRawBytesHex $ BS8.pack $ T.unpack t
  parseJSON _ = fail "extended verification key expected"

{- |

>>> Printf.printf "%s\n" ("4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e" :: (GYExtendedVerificationKey 'GYKeyRolePayment))
4081b0a03b2b66c74ed1b0de1d874ed820c31b2c0cbba632f4f1ca82a113ac7e18f5eadae23a5c4e65e65e9d4e3a1d0e5c54f0e0c04e6e85e1f7ea75db7c9f1e
-}
instance Printf.PrintfArg (GYExtendedVerificationKey kr) where
  formatArg = Printf.formatArg . extendedVerificationKeyToRawBytesHexText

getExtendedVerificationKey :: GYExtendedSigningKey kr -> GYExtendedVerificationKey kr
getExtendedVerificationKey (GYExtendedSigningKey xpub) = Crypto.HD.toXPub xpub & GYExtendedVerificationKey

extendedVerificationKeyHash :: GYExtendedVerificationKey kr -> GYKeyHash kr
extendedVerificationKeyHash (GYExtendedVerificationKey xpub) = keyHashFromLedger $ Ledger.KeyHash $ Crypto.castHash $ Crypto.hashWith Crypto.HD.xpubPublicKey xpub

extendedVerificationKeyToRawBytes :: GYExtendedVerificationKey kr -> BS8.ByteString
extendedVerificationKeyToRawBytes (GYExtendedVerificationKey xpub) = Crypto.HD.unXPub xpub

extendedVerificationKeyToRawBytesHex :: GYExtendedVerificationKey kr -> BS8.ByteString
extendedVerificationKeyToRawBytesHex = extendedVerificationKeyToRawBytes >>> BS16.encode

extendedVerificationKeyToRawBytesHexText :: GYExtendedVerificationKey kr -> T.Text
extendedVerificationKeyToRawBytesHexText = extendedVerificationKeyToRawBytesHex >>> TE.decodeUtf8

-- | Decode from raw bytes.
extendedVerificationKeyFromRawBytes :: BS8.ByteString -> Maybe (GYExtendedVerificationKey kr)
extendedVerificationKeyFromRawBytes = either (const Nothing) Just . extendedVerificationKeyFromRawBytes'

-- | Decode from raw bytes.
extendedVerificationKeyFromRawBytes' :: BS8.ByteString -> Either String (GYExtendedVerificationKey kr)
extendedVerificationKeyFromRawBytes' bs = GYExtendedVerificationKey <$> Crypto.HD.xpub bs

-- | Decode from raw bytes represented as hex.
extendedVerificationKeyFromRawBytesHex :: BS8.ByteString -> Either String (GYExtendedVerificationKey kr)
extendedVerificationKeyFromRawBytesHex bs =
  case BS16.decode bs of
    Left e -> Left $ "GeniusYield.Types.Key.extendedVerificationKeyFromRawBytesHex: unable to decode from hex string: " <> BS8.unpack bs <> ", error: " <> e
    Right bs' -> case extendedVerificationKeyFromRawBytes' bs' of
      Left e -> Left $ "GeniusYield.Types.Key.extendedVerificationKeyFromRawBytesHex: unable to decode from bytes, given hex string " <> show bs <> ", corresponding bytes " <> show bs' <> ", error: " <> e
      res -> res

type family GYExtendedVerificationKeyToApi (kr :: GYKeyRole) where
  GYExtendedVerificationKeyToApi 'GYKeyRolePayment = Api.VerificationKey Api.PaymentExtendedKey
  GYExtendedVerificationKeyToApi 'GYKeyRoleStaking = Api.VerificationKey Api.StakeExtendedKey
  GYExtendedVerificationKeyToApi 'GYKeyRoleDRep = Api.VerificationKey Api.DRepExtendedKey
  -- GYExtendedVerificationKeyToApi 'GYKeyRoleStakePool = Api.VerificationKey Api.StakePoolKey
  GYExtendedVerificationKeyToApi 'GYKeyRoleHotCommittee = Api.VerificationKey Api.CommitteeHotExtendedKey
  GYExtendedVerificationKeyToApi 'GYKeyRoleColdCommittee = Api.VerificationKey Api.CommitteeColdExtendedKey

extendedVerificationKeyToApi :: forall kr. SingGYKeyRoleI kr => GYExtendedVerificationKey kr -> GYExtendedVerificationKeyToApi kr
extendedVerificationKeyToApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> error "extendedVerificationKeyToApi: Impossible, no API representation for extended stake pool key"
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

extendedVerificationKeyFromApi :: forall kr. SingGYKeyRoleI kr => GYExtendedVerificationKeyToApi kr -> GYExtendedVerificationKey kr
extendedVerificationKeyFromApi = case singGYKeyRole @kr of
  SingGYKeyRolePayment -> coerce
  SingGYKeyRoleStaking -> coerce
  SingGYKeyRoleDRep -> coerce
  SingGYKeyRoleStakePool -> error "extendedVerificationKeyFromApi: Impossible, no API representation for extended stake pool key"
  SingGYKeyRoleHotCommittee -> coerce
  SingGYKeyRoleColdCommittee -> coerce

-------------------------------------------------------------------------------
-- Payment verification key (public)
-------------------------------------------------------------------------------

{- |

@type GYPaymentVerificationKey = GYVerificationKey 'GYKeyRolePayment@

>>> "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey
GYVerificationKey (GYKeyRolePayment) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
type GYPaymentVerificationKey = GYVerificationKey 'GYKeyRolePayment

{- |

>>> paymentVerificationKeyFromApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
GYVerificationKey (GYKeyRolePayment) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
paymentVerificationKeyFromApi :: Api.VerificationKey Api.PaymentKey -> GYPaymentVerificationKey
paymentVerificationKeyFromApi = verificationKeyFromApi

{- |

>>> paymentVerificationKeyToApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
"0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
paymentVerificationKeyToApi :: GYPaymentVerificationKey -> Api.VerificationKey Api.PaymentKey
paymentVerificationKeyToApi = verificationKeyToApi

paymentVerificationKeyToLedger :: GYPaymentVerificationKey -> Ledger.VKey Ledger.Payment Ledger.StandardCrypto
paymentVerificationKeyToLedger = verificationKeyToLedger

paymentVerificationKeyRawBytes :: GYPaymentVerificationKey -> BS8.ByteString
paymentVerificationKeyRawBytes = verificationKeyToRawBytes

-- TODO: Would need revision once we modify GYPubKeyHash.
pubKeyHash :: GYPaymentVerificationKey -> GYPubKeyHash
pubKeyHash = verificationKeyHash >>> keyHashToApi >>> pubKeyHashFromApi

paymentKeyHash :: GYPaymentVerificationKey -> GYPaymentKeyHash
paymentKeyHash = verificationKeyHash

-------------------------------------------------------------------------------
-- Payment signing key (private)
-------------------------------------------------------------------------------

{- |

@type GYPaymentSigningKey = GYSigningKey 'GYKeyRolePayment@

>>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey
GYSigningKey (GYKeyRolePayment) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
type GYPaymentSigningKey = GYSigningKey 'GYKeyRolePayment

-- | @type GYExtendedPaymentSigningKey = GYExtendedSigningKey 'GYKeyRolePayment@
type GYExtendedPaymentSigningKey = GYExtendedSigningKey 'GYKeyRolePayment

{- |

>>> paymentSigningKeyFromApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
GYSigningKey (GYKeyRolePayment) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
paymentSigningKeyFromApi :: Api.SigningKey Api.PaymentKey -> GYPaymentSigningKey
paymentSigningKeyFromApi = signingKeyFromApi

extendedPaymentSigningKeyFromApi :: Api.SigningKey Api.PaymentExtendedKey -> GYExtendedPaymentSigningKey
extendedPaymentSigningKeyFromApi = extendedSigningKeyFromApi

{- |

>>> paymentSigningKeyToApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
"5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
paymentSigningKeyToApi :: GYPaymentSigningKey -> Api.SigningKey Api.PaymentKey
paymentSigningKeyToApi = signingKeyToApi

extendedPaymentSigningKeyToApi :: GYExtendedPaymentSigningKey -> Api.SigningKey Api.PaymentExtendedKey
extendedPaymentSigningKeyToApi = extendedSigningKeyToApi

paymentSigningKeyToLedger :: GYPaymentSigningKey -> Ledger.SignKeyDSIGN Ledger.StandardCrypto
paymentSigningKeyToLedger = signingKeyToLedger

paymentSigningKeyToLedgerKeyPair :: GYPaymentSigningKey -> TLedger.KeyPair Ledger.Payment Ledger.StandardCrypto
paymentSigningKeyToLedgerKeyPair = signingKeyToLedgerKeyPair

paymentSigningKeyFromLedgerKeyPair :: TLedger.KeyPair Ledger.Payment Ledger.StandardCrypto -> GYPaymentSigningKey
paymentSigningKeyFromLedgerKeyPair = signingKeyFromLedgerKeyPair

-- | Reads a payment signing key from a file.
readPaymentSigningKey :: FilePath -> IO GYPaymentSigningKey
readPaymentSigningKey = readSigningKey

-- | Reads extended payment signing key from file
readExtendedPaymentSigningKey :: FilePath -> IO GYExtendedPaymentSigningKey
readExtendedPaymentSigningKey = readExtendedSigningKey

-- | Writes a payment signing key to a file.
writePaymentSigningKey :: FilePath -> GYPaymentSigningKey -> IO ()
writePaymentSigningKey = writeSigningKey

-- | Writes a extended payment signing key to a file.
writeExtendedPaymentSigningKey :: FilePath -> GYExtendedPaymentSigningKey -> IO ()
writeExtendedPaymentSigningKey = writeExtendedSigningKey

{- |

>>> paymentVerificationKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
GYVerificationKey (GYKeyRolePayment) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
paymentVerificationKey :: GYPaymentSigningKey -> GYPaymentVerificationKey
paymentVerificationKey = getVerificationKey

-- | Generates a new random payment signing key.
generatePaymentSigningKey :: IO GYPaymentSigningKey
generatePaymentSigningKey = generateSigningKey

-------------------------------------------------------------------------------
-- Stake verification key (public)
-------------------------------------------------------------------------------

{- |

@type GYStakeVerificationKey = GYVerificationKey 'GYKeyRoleStaking@

>>> "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYStakeVerificationKey
GYVerificationKey (GYKeyRoleStaking) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
type GYStakeVerificationKey = GYVerificationKey 'GYKeyRoleStaking

{- |

>>> stakeVerificationKeyFromApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
GYVerificationKey (GYKeyRoleStaking) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
stakeVerificationKeyFromApi :: Api.VerificationKey Api.StakeKey -> GYStakeVerificationKey
stakeVerificationKeyFromApi = verificationKeyFromApi

{- |

>>> stakeVerificationKeyToApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
"0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
stakeVerificationKeyToApi :: GYStakeVerificationKey -> Api.VerificationKey Api.StakeKey
stakeVerificationKeyToApi = verificationKeyToApi

stakeVerificationKeyToLedger :: GYStakeVerificationKey -> Ledger.VKey Ledger.Staking Ledger.StandardCrypto
stakeVerificationKeyToLedger = verificationKeyToLedger

stakeKeyHash :: GYStakeVerificationKey -> GYStakeKeyHash
stakeKeyHash = verificationKeyHash

-------------------------------------------------------------------------------
-- Stake signing key (private)
-------------------------------------------------------------------------------

{- |

@type GYStakeSigningKey = GYSigningKey 'GYKeyRoleStaking@

>>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYStakeSigningKey
GYSigningKey (GYKeyRoleStaking) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
type GYStakeSigningKey = GYSigningKey 'GYKeyRoleStaking

-- | @type GYExtendedStakeSigningKey = GYEExtendedSigningKey 'GYKeyRoleStaking@
type GYExtendedStakeSigningKey = GYExtendedSigningKey 'GYKeyRoleStaking

{- |

>>> stakeSigningKeyFromApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
GYSigningKey (GYKeyRoleStaking) "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
stakeSigningKeyFromApi :: Api.SigningKey Api.StakeKey -> GYStakeSigningKey
stakeSigningKeyFromApi = signingKeyFromApi

extendedStakeSigningKeyFromApi :: Api.SigningKey Api.StakeExtendedKey -> GYExtendedStakeSigningKey
extendedStakeSigningKeyFromApi = extendedSigningKeyFromApi

{- |

>>> stakeSigningKeyToApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
"5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-}
stakeSigningKeyToApi :: GYStakeSigningKey -> Api.SigningKey Api.StakeKey
stakeSigningKeyToApi = signingKeyToApi

extendedStakeSigningKeyToApi :: GYExtendedStakeSigningKey -> Api.SigningKey Api.StakeExtendedKey
extendedStakeSigningKeyToApi = extendedSigningKeyToApi

stakeSigningKeyToLedger :: GYStakeSigningKey -> Ledger.SignKeyDSIGN Ledger.StandardCrypto
stakeSigningKeyToLedger = signingKeyToLedger

stakeSigningKeyToLedgerKeyPair :: GYStakeSigningKey -> TLedger.KeyPair Ledger.Staking Ledger.StandardCrypto
stakeSigningKeyToLedgerKeyPair = signingKeyToLedgerKeyPair

stakeSigningKeyFromLedgerKeyPair :: TLedger.KeyPair Ledger.Staking Ledger.StandardCrypto -> GYStakeSigningKey
stakeSigningKeyFromLedgerKeyPair = signingKeyFromLedgerKeyPair

-- | Reads a stake signing key from a file.
readStakeSigningKey :: FilePath -> IO GYStakeSigningKey
readStakeSigningKey = readSigningKey

-- | Reads extended stake signing key from file
readExtendedStakeSigningKey :: FilePath -> IO GYExtendedStakeSigningKey
readExtendedStakeSigningKey = readExtendedSigningKey

-- | Writes a stake signing key to a file.
writeStakeSigningKey :: FilePath -> GYStakeSigningKey -> IO ()
writeStakeSigningKey = writeSigningKey

-- | Writes a extended stake signing key to a file.
writeExtendedStakeSigningKey :: FilePath -> GYExtendedStakeSigningKey -> IO ()
writeExtendedStakeSigningKey = writeExtendedSigningKey

{- |

>>> stakeVerificationKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
GYVerificationKey (GYKeyRoleStaking) "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-}
stakeVerificationKey :: GYStakeSigningKey -> GYStakeVerificationKey
stakeVerificationKey = getVerificationKey

-- | Generates a new random stake signing key.
generateStakeSigningKey :: IO GYStakeSigningKey
generateStakeSigningKey = generateSigningKey

data GYSomeSigningKey = forall a. (ToShelleyWitnessSigningKey a, Show a) => GYSomeSigningKey a

instance ToShelleyWitnessSigningKey GYSomeSigningKey where
  toShelleyWitnessSigningKey (GYSomeSigningKey skey) = toShelleyWitnessSigningKey skey

data GYSomePaymentSigningKey
  = AGYPaymentSigningKey !GYPaymentSigningKey
  | AGYExtendedPaymentSigningKey !GYExtendedPaymentSigningKey
  deriving stock (Eq, Show, Ord)

instance ToShelleyWitnessSigningKey GYSomePaymentSigningKey where
  toShelleyWitnessSigningKey (AGYPaymentSigningKey key) = toShelleyWitnessSigningKey key
  toShelleyWitnessSigningKey (AGYExtendedPaymentSigningKey key) = toShelleyWitnessSigningKey key

data GYSomeStakeSigningKey
  = AGYStakeSigningKey !GYStakeSigningKey
  | AGYExtendedStakeSigningKey !GYExtendedStakeSigningKey
  deriving stock (Eq, Show, Ord)

instance ToShelleyWitnessSigningKey GYSomeStakeSigningKey where
  toShelleyWitnessSigningKey (AGYStakeSigningKey key) = toShelleyWitnessSigningKey key
  toShelleyWitnessSigningKey (AGYExtendedStakeSigningKey key) = toShelleyWitnessSigningKey key

readSomePaymentSigningKey :: FilePath -> IO GYSomePaymentSigningKey
readSomePaymentSigningKey file = do
  e <-
    Api.readFileTextEnvelopeAnyOf
      [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) $ AGYPaymentSigningKey . paymentSigningKeyFromApi
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey) $ AGYExtendedPaymentSigningKey . extendedPaymentSigningKeyFromApi
      ]
      (Api.File file)
  case e of
    Left err -> throwIO $ userError $ show err
    Right skey -> return skey

readSomeStakeSigningKey :: FilePath -> IO GYSomeStakeSigningKey
readSomeStakeSigningKey file = do
  e <-
    Api.readFileTextEnvelopeAnyOf
      [ Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey) $ AGYStakeSigningKey . stakeSigningKeyFromApi
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey) $ AGYExtendedStakeSigningKey . extendedStakeSigningKeyFromApi
      ]
      (Api.File file)
  case e of
    Left err -> throwIO $ userError $ show err
    Right skey -> return skey

somePaymentSigningKeyToSomeSigningKey :: GYSomePaymentSigningKey -> GYSomeSigningKey
somePaymentSigningKeyToSomeSigningKey (AGYPaymentSigningKey key) = GYSomeSigningKey key
somePaymentSigningKeyToSomeSigningKey (AGYExtendedPaymentSigningKey key) = GYSomeSigningKey key

someStakeSigningKeyToSomeSigningKey :: GYSomeStakeSigningKey -> GYSomeSigningKey
someStakeSigningKeyToSomeSigningKey (AGYStakeSigningKey key) = GYSomeSigningKey key
someStakeSigningKeyToSomeSigningKey (AGYExtendedStakeSigningKey key) = GYSomeSigningKey key
