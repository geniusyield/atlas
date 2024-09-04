{-|
Module      : GeniusYield.Types.Key
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Key
    ( -- * Payment verification key
      GYPaymentVerificationKey
    , paymentVerificationKeyFromApi
    , paymentVerificationKeyToApi
    , paymentVerificationKeyToLedger
    , paymentVerificationKeyRawBytes
    , pubKeyHash
    , paymentKeyHash
      -- * Payment signing key
    , GYPaymentSigningKey
    , GYExtendedPaymentSigningKey
    , paymentSigningKeyFromApi
    , extendedPaymentSigningKeyFromApi
    , paymentSigningKeyToApi
    , extendedPaymentSigningKeyToApi
    , paymentSigningKeyToLedger
    , paymentSigningKeyToLedgerKeyPair
    , paymentSigningKeyFromLedgerKeyPair
    , readPaymentSigningKey
    , readExtendedPaymentSigningKey
    , writePaymentSigningKey
    , writeExtendedPaymentSigningKey
    , paymentVerificationKey
    , generatePaymentSigningKey
    -- * Stake verification key
    , GYStakeVerificationKey
    , stakeVerificationKeyFromApi
    , stakeVerificationKeyToApi
    , stakeKeyHash
    , stakeVerificationKeyToLedger
    -- * Stake signing key
    , GYStakeSigningKey
    , GYExtendedStakeSigningKey
    , stakeSigningKeyFromApi
    , extendedStakeSigningKeyFromApi
    , stakeSigningKeyToApi
    , extendedStakeSigningKeyToApi
    , stakeSigningKeyToLedger
    , stakeSigningKeyToLedgerKeyPair
    , stakeSigningKeyFromLedgerKeyPair
    , readStakeSigningKey
    , readExtendedStakeSigningKey
    , writeStakeSigningKey
    , writeExtendedStakeSigningKey
    , stakeVerificationKey
    , generateStakeSigningKey
    , GYSomeSigningKey (..)
    , readSomeSigningKey
    , GYSomePaymentSigningKey (..)
    , readSomePaymentSigningKey
    , somePaymentSigningKeyToSomeSigningKey
) where

import qualified Cardano.Api                      as Api
import qualified Cardano.Ledger.Crypto            as Ledger
import qualified Cardano.Ledger.Keys              as Ledger
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString.Base16           as BS16
import qualified Data.ByteString.Char8            as BS8
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Csv                         as Csv
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import qualified Test.Cardano.Ledger.Core.KeyPair as TLedger
import qualified Text.Printf                      as Printf

import           GeniusYield.Imports
import           GeniusYield.Types.Key.Class      (ToShelleyWitnessSigningKey,
                                                   toShelleyWitnessSigningKey)
import           GeniusYield.Types.PaymentKeyHash (GYPaymentKeyHash,
                                                   paymentKeyHashFromApi)
import           GeniusYield.Types.PubKeyHash     (GYPubKeyHash,
                                                   pubKeyHashFromApi)
import           GeniusYield.Types.StakeKeyHash   (GYStakeKeyHash,
                                                   stakeKeyHashFromApi)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import           Data.Either                (isRight)
-- >>> import qualified Text.Printf                as Printf

-------------------------------------------------------------------------------
-- Payment verification key (public)
-------------------------------------------------------------------------------

-- |
--
-- >>> "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
newtype GYPaymentVerificationKey = GYPaymentVerificationKey (Api.VerificationKey Api.PaymentKey)
    deriving stock Show
    deriving newtype (Eq, IsString)

-- |
--
-- >>> paymentVerificationKeyFromApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKeyFromApi :: Api.VerificationKey Api.PaymentKey -> GYPaymentVerificationKey
paymentVerificationKeyFromApi = coerce

-- |
--
-- >>> paymentVerificationKeyToApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKeyToApi :: GYPaymentVerificationKey -> Api.VerificationKey Api.PaymentKey
paymentVerificationKeyToApi = coerce

paymentVerificationKeyToLedger :: GYPaymentVerificationKey -> Ledger.VKey r Ledger.StandardCrypto
paymentVerificationKeyToLedger = coerce

paymentVerificationKeyRawBytes :: GYPaymentVerificationKey -> BS8.ByteString
paymentVerificationKeyRawBytes = Api.serialiseToRawBytes . paymentVerificationKeyToApi

pubKeyHash :: GYPaymentVerificationKey -> GYPubKeyHash
pubKeyHash = pubKeyHashFromApi . Api.verificationKeyHash . paymentVerificationKeyToApi

paymentKeyHash :: GYPaymentVerificationKey -> GYPaymentKeyHash
paymentKeyHash = paymentKeyHashFromApi . Api.verificationKeyHash . paymentVerificationKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
-- "58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
instance Aeson.ToJSON GYPaymentVerificationKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . paymentVerificationKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605\""
-- Right (GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")
--
-- >>> Aeson.eitherDecode @GYPaymentVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193zzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYPaymentVerificationKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsVerificationKey Api.AsPaymentKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYPaymentVerificationKey skey
    parseJSON _ = fail "payment verification key expected"



-- |
--
-- >>> Printf.printf "%s\n" ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYPaymentVerificationKey)
-- 0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605
--
instance Printf.PrintfArg GYPaymentVerificationKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentVerificationKeyToApi

-------------------------------------------------------------------------------
-- Payment signing key (private)
-------------------------------------------------------------------------------

-- |
--
-- >>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey
-- GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
newtype GYPaymentSigningKey = GYPaymentSigningKey (Api.SigningKey Api.PaymentKey)
    deriving stock Show
    deriving newtype IsString

instance Eq GYPaymentSigningKey where
    (==) = (==) `on` show

instance Ord GYPaymentSigningKey where
    compare = compare `on` show

instance ToShelleyWitnessSigningKey GYPaymentSigningKey where
  toShelleyWitnessSigningKey (GYPaymentSigningKey skey) = Api.WitnessPaymentKey skey


-- Handle key for extended signing key
newtype GYExtendedPaymentSigningKey = GYExtendedPaymentSigningKey (Api.SigningKey Api.PaymentExtendedKey)
    deriving stock Show
    deriving newtype IsString

instance Eq GYExtendedPaymentSigningKey where
    (==) = (==) `on` show

instance Ord GYExtendedPaymentSigningKey where
    compare = compare `on` show

instance ToShelleyWitnessSigningKey GYExtendedPaymentSigningKey where
  toShelleyWitnessSigningKey (GYExtendedPaymentSigningKey skey) = Api.WitnessPaymentExtendedKey skey

-- |
--
-- >>> paymentSigningKeyFromApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
paymentSigningKeyFromApi :: Api.SigningKey Api.PaymentKey -> GYPaymentSigningKey
paymentSigningKeyFromApi = coerce

extendedPaymentSigningKeyFromApi :: Api.SigningKey Api.PaymentExtendedKey -> GYExtendedPaymentSigningKey
extendedPaymentSigningKeyFromApi = coerce

-- |
--
-- >>> paymentSigningKeyToApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
paymentSigningKeyToApi :: GYPaymentSigningKey -> Api.SigningKey Api.PaymentKey
paymentSigningKeyToApi = coerce

extendedPaymentSigningKeyToApi :: GYExtendedPaymentSigningKey -> Api.SigningKey Api.PaymentExtendedKey
extendedPaymentSigningKeyToApi = coerce

paymentSigningKeyToLedger :: GYPaymentSigningKey -> Ledger.SignKeyDSIGN Ledger.StandardCrypto
paymentSigningKeyToLedger = coerce

paymentSigningKeyToLedgerKeyPair :: GYPaymentSigningKey -> TLedger.KeyPair r Ledger.StandardCrypto
paymentSigningKeyToLedgerKeyPair skey = TLedger.KeyPair
    { TLedger.vKey = paymentVerificationKeyToLedger $ paymentVerificationKey skey
    , TLedger.sKey = paymentSigningKeyToLedger skey
    }

paymentSigningKeyFromLedgerKeyPair :: TLedger.KeyPair r Ledger.StandardCrypto -> GYPaymentSigningKey
paymentSigningKeyFromLedgerKeyPair = coerce . TLedger.sKey

-- | Reads a payment signing key from a file.
--
readPaymentSigningKey :: FilePath -> IO GYPaymentSigningKey
readPaymentSigningKey fp = do
    s <- Api.readFileTextEnvelopeAnyOf acceptedTypes (Api.File fp)
    case s of
        Left err -> fail (show err) --- throws IOError
        Right x  -> return (GYPaymentSigningKey x)
  where
    acceptedTypes =
        [ Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey) Api.castSigningKey
        , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) id
        ]

-- | Reads extended payment signing key from file
--
readExtendedPaymentSigningKey :: FilePath -> IO GYExtendedPaymentSigningKey
readExtendedPaymentSigningKey fp = do
    s <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsPaymentExtendedKey) (Api.File fp)
    case s of
        Left err -> fail (show err) --- throws IOError
        Right x  -> return $ GYExtendedPaymentSigningKey x

-- | Writes a payment signing key to a file.
--
writePaymentSigningKey :: FilePath -> GYPaymentSigningKey -> IO ()
writePaymentSigningKey file key = do
    e <- Api.writeFileTextEnvelope (Api.File file) (Just "Payment Signing Key") $ paymentSigningKeyToApi key
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

-- | Writes a extended payment signing key to a file.
--
writeExtendedPaymentSigningKey :: FilePath -> GYExtendedPaymentSigningKey -> IO ()
writeExtendedPaymentSigningKey file key = do
    e <- Api.writeFileTextEnvelope (Api.File file) (Just "Extended Payment Signing Key") $ extendedPaymentSigningKeyToApi key
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

-- |
--
-- >>> paymentVerificationKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYPaymentVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
paymentVerificationKey :: GYPaymentSigningKey -> GYPaymentVerificationKey
paymentVerificationKey = GYPaymentVerificationKey . Api.getVerificationKey . paymentSigningKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
-- "58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
instance Aeson.ToJSON GYPaymentSigningKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . paymentSigningKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290\""
-- Right (GYPaymentSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290")
--
-- >>> Aeson.eitherDecode @GYPaymentSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fceczzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYPaymentSigningKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsSigningKey Api.AsPaymentKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYPaymentSigningKey skey
    parseJSON _ = fail "payment signing key expected"


instance Csv.ToField GYPaymentSigningKey where
  toField = LBS.toStrict . Aeson.encode

instance Csv.FromField GYPaymentSigningKey where
  parseField k =
      case Aeson.decode $ LBS.fromStrict k of
        Just v  -> pure v
        Nothing -> fail $ "Error Parsing paymentSigningKey from CSV: " <> show k

-- |
--
-- >>> Printf.printf "%s\n" ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYPaymentSigningKey)
-- 5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290
--
instance Printf.PrintfArg GYPaymentSigningKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . paymentSigningKeyToApi

-- | Generates a new random payment signing key.
--
generatePaymentSigningKey :: IO GYPaymentSigningKey
generatePaymentSigningKey = paymentSigningKeyFromApi <$> Api.generateSigningKey Api.AsPaymentKey


-------------------------------------------------------------------------------
-- Stake verification key (public)
-------------------------------------------------------------------------------

-- |
--
-- >>> "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYStakeVerificationKey
-- GYStakeVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
newtype GYStakeVerificationKey = GYStakeVerificationKey (Api.VerificationKey Api.StakeKey)
    deriving stock Show
    deriving newtype (Eq, IsString)

-- |
--
-- >>> stakeVerificationKeyFromApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- GYStakeVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
stakeVerificationKeyFromApi :: Api.VerificationKey Api.StakeKey -> GYStakeVerificationKey
stakeVerificationKeyFromApi = coerce

-- |
--
-- >>> stakeVerificationKeyToApi "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
-- "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
stakeVerificationKeyToApi :: GYStakeVerificationKey -> Api.VerificationKey Api.StakeKey
stakeVerificationKeyToApi = coerce

stakeVerificationKeyToLedger :: GYStakeVerificationKey -> Ledger.VKey r Ledger.StandardCrypto
stakeVerificationKeyToLedger = coerce

stakeKeyHash :: GYStakeVerificationKey -> GYStakeKeyHash
stakeKeyHash = stakeKeyHashFromApi . Api.verificationKeyHash . stakeVerificationKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYStakeVerificationKey)
-- "58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
instance Aeson.ToJSON GYStakeVerificationKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . stakeVerificationKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYStakeVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605\""
-- Right (GYStakeVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605")
--
-- >>> Aeson.eitherDecode @GYStakeVerificationKey "\"58200717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193zzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYStakeVerificationKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsVerificationKey Api.AsStakeKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYStakeVerificationKey skey
    parseJSON _ = fail "stake verification key expected"



-- |
--
-- >>> Printf.printf "%s\n" ("0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605" :: GYStakeVerificationKey)
-- 0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605
--
instance Printf.PrintfArg GYStakeVerificationKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . stakeVerificationKeyToApi


-------------------------------------------------------------------------------
-- Stake signing key (private)
-------------------------------------------------------------------------------

-- |
--
-- >>> "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYStakeSigningKey
-- GYStakeSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
newtype GYStakeSigningKey = GYStakeSigningKey (Api.SigningKey Api.StakeKey)
    deriving stock Show
    deriving newtype IsString

instance Eq GYStakeSigningKey where
    (==) = (==) `on` show

instance Ord GYStakeSigningKey where
    compare = compare `on` show

instance ToShelleyWitnessSigningKey GYStakeSigningKey where
  toShelleyWitnessSigningKey (GYStakeSigningKey skey) = Api.WitnessStakeKey skey

-- Handle key for extended signing key
newtype GYExtendedStakeSigningKey = GYExtendedStakeSigningKey (Api.SigningKey Api.StakeExtendedKey)
    deriving stock Show
    deriving newtype IsString

instance Eq GYExtendedStakeSigningKey where
    (==) = (==) `on` show

instance Ord GYExtendedStakeSigningKey where
    compare = compare `on` show

instance ToShelleyWitnessSigningKey GYExtendedStakeSigningKey where
  toShelleyWitnessSigningKey (GYExtendedStakeSigningKey skey) = Api.WitnessStakeExtendedKey skey

-- |
--
-- >>> stakeSigningKeyFromApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYStakeSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
stakeSigningKeyFromApi :: Api.SigningKey Api.StakeKey -> GYStakeSigningKey
stakeSigningKeyFromApi = coerce

extendedStakeSigningKeyFromApi :: Api.SigningKey Api.StakeExtendedKey -> GYExtendedStakeSigningKey
extendedStakeSigningKeyFromApi = coerce

-- |
--
-- >>> stakeSigningKeyToApi "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
stakeSigningKeyToApi :: GYStakeSigningKey -> Api.SigningKey Api.StakeKey
stakeSigningKeyToApi = coerce

extendedStakeSigningKeyToApi :: GYExtendedStakeSigningKey -> Api.SigningKey Api.StakeExtendedKey
extendedStakeSigningKeyToApi = coerce

stakeSigningKeyToLedger :: GYStakeSigningKey -> Ledger.SignKeyDSIGN Ledger.StandardCrypto
stakeSigningKeyToLedger = coerce

stakeSigningKeyToLedgerKeyPair :: GYStakeSigningKey -> TLedger.KeyPair r Ledger.StandardCrypto
stakeSigningKeyToLedgerKeyPair skey = TLedger.KeyPair
    { TLedger.vKey = stakeVerificationKeyToLedger $ stakeVerificationKey skey
    , TLedger.sKey = stakeSigningKeyToLedger skey
    }

stakeSigningKeyFromLedgerKeyPair :: TLedger.KeyPair r Ledger.StandardCrypto -> GYStakeSigningKey
stakeSigningKeyFromLedgerKeyPair = coerce . TLedger.sKey

-- | Reads a stake signing key from a file.
--
readStakeSigningKey :: FilePath -> IO GYStakeSigningKey
readStakeSigningKey fp = do
    s <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsStakeKey) (Api.File fp)
    case s of
        Left err -> fail (show err) --- throws IOError
        Right x  -> return (GYStakeSigningKey x)

-- | Reads extended stake signing key from file
--
readExtendedStakeSigningKey :: FilePath -> IO GYExtendedStakeSigningKey
readExtendedStakeSigningKey fp = do
    s <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsStakeExtendedKey) (Api.File fp)
    case s of
        Left err -> fail (show err) --- throws IOError
        Right x  -> return $ GYExtendedStakeSigningKey x

-- | Writes a stake signing key to a file.
--
writeStakeSigningKey :: FilePath -> GYStakeSigningKey -> IO ()
writeStakeSigningKey file key = do
    e <- Api.writeFileTextEnvelope (Api.File file) (Just "Stake Signing Key") $ stakeSigningKeyToApi key
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

-- | Writes a extended stake signing key to a file.
--
writeExtendedStakeSigningKey :: FilePath -> GYExtendedStakeSigningKey -> IO ()
writeExtendedStakeSigningKey file key = do
    e <- Api.writeFileTextEnvelope (Api.File file) (Just "Extended Stake Signing Key") $ extendedStakeSigningKeyToApi key
    case e of
        Left (err :: Api.FileError ()) -> throwIO $ userError $ show err
        Right ()                       -> return ()

-- |
--
-- >>> stakeVerificationKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
-- GYStakeVerificationKey "0717bc56ed4897c3dde0690e3d9ce61e28a55f520fde454f6b5b61305b193605"
--
stakeVerificationKey :: GYStakeSigningKey -> GYStakeVerificationKey
stakeVerificationKey = GYStakeVerificationKey . Api.getVerificationKey . stakeSigningKeyToApi

-- |
--
-- >>> LBS8.putStrLn $ Aeson.encode ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYStakeSigningKey)
-- "58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290"
--
instance Aeson.ToJSON GYStakeSigningKey where
    toJSON = Aeson.String . TE.decodeUtf8 . BS16.encode . Api.serialiseToCBOR . stakeSigningKeyToApi

-- |
--
-- >>> Aeson.eitherDecode @GYStakeSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290\""
-- Right (GYStakeSigningKey "5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290")
--
-- >>> Aeson.eitherDecode @GYStakeSigningKey "\"58205ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fceczzz\""
-- Left "Error in $: invalid character at offset: 65"
--
instance Aeson.FromJSON GYStakeSigningKey where
    parseJSON (Aeson.String t) = case BS16.decode $ BS8.pack $ T.unpack t of
        Left err -> fail err
        Right bs -> case Api.deserialiseFromCBOR (Api.AsSigningKey Api.AsStakeKey) bs of
            Left err   -> fail $ show err
            Right skey -> return $ GYStakeSigningKey skey
    parseJSON _ = fail "stake signing key expected"


instance Csv.ToField GYStakeSigningKey where
  toField = LBS.toStrict . Aeson.encode

instance Csv.FromField GYStakeSigningKey where
  parseField k =
      case Aeson.decode $ LBS.fromStrict k of
        Just v  -> pure v
        Nothing -> fail $ "Error Parsing stakeSigningKey from CSV: " <> show k

-- |
--
-- >>> Printf.printf "%s\n" ("5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290" :: GYStakeSigningKey)
-- 5ac75cb3435ef38c5bf15d11469b301b13729deb9595133a608fc0881fcec290
--
instance Printf.PrintfArg GYStakeSigningKey where
    formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . stakeSigningKeyToApi

-- | Generates a new random stake signing key.
--
generateStakeSigningKey :: IO GYStakeSigningKey
generateStakeSigningKey = stakeSigningKeyFromApi <$> Api.generateSigningKey Api.AsStakeKey

data GYSomeSigningKey = forall a. (ToShelleyWitnessSigningKey a, Show a) => GYSomeSigningKey a

instance ToShelleyWitnessSigningKey GYSomeSigningKey where
  toShelleyWitnessSigningKey (GYSomeSigningKey skey) = toShelleyWitnessSigningKey skey

readSomeSigningKey :: FilePath -> IO GYSomeSigningKey
readSomeSigningKey file = do
    e <- Api.readFileTextEnvelopeAnyOf
        [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)         $ GYSomeSigningKey . paymentSigningKeyFromApi
        , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey) $ GYSomeSigningKey . extendedPaymentSigningKeyFromApi
        , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)           $ GYSomeSigningKey . stakeSigningKeyFromApi
        , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey)   $ GYSomeSigningKey . extendedStakeSigningKeyFromApi
        ] (Api.File file)
    case e of
        Left err   -> throwIO $ userError $ show err
        Right skey -> return skey

data GYSomePaymentSigningKey = AGYPaymentSigningKey !GYPaymentSigningKey | AGYExtendedPaymentSigningKey !GYExtendedPaymentSigningKey
  deriving stock (Eq, Show, Ord)

readSomePaymentSigningKey :: FilePath -> IO GYSomePaymentSigningKey
readSomePaymentSigningKey file = do
    e <- Api.readFileTextEnvelopeAnyOf
        [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)         $ AGYPaymentSigningKey . paymentSigningKeyFromApi
        , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey) $ AGYExtendedPaymentSigningKey . extendedPaymentSigningKeyFromApi
        ] (Api.File file)
    case e of
        Left err   -> throwIO $ userError $ show err
        Right skey -> return skey

somePaymentSigningKeyToSomeSigningKey :: GYSomePaymentSigningKey -> GYSomeSigningKey
somePaymentSigningKeyToSomeSigningKey (AGYPaymentSigningKey key) = GYSomeSigningKey key
somePaymentSigningKeyToSomeSigningKey (AGYExtendedPaymentSigningKey key) = GYSomeSigningKey key
