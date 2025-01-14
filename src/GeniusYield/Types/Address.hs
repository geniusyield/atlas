{- |
Module      : GeniusYield.Types.Address
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Address (
  GYAddress,
  addressToApi,
  addressToApi',
  addressFromApi,
  addressFromApi',
  addressToPlutus,
  addressFromPlutus,
  addressToPaymentCredential,
  addressToStakeCredential,
  addressFromPubKeyHash,
  addressFromPaymentKeyHash,
  addressFromScript,
  addressFromValidator,
  addressFromCredential,
  addressFromValidatorHash,
  addressFromScriptHash,
  addressFromSimpleScript,
  addressToText,
  addressFromTextMaybe,
  unsafeAddressFromText,
  addressToPubKeyHash,
  addressToValidatorHash,

  -- * newtype wrapper
  GYAddressBech32,
  addressToBech32,
  addressFromBech32,

  -- * Stake address.
  GYStakeAddress,
  stakeAddressFromApi,
  stakeAddressToApi,
  stakeAddressFromTextMaybe,
  unsafeStakeAddressFromText,
  stakeAddressToText,
  stakeAddressToLedger,
  stakeAddressCredential,
  stakeAddressToCredential,
  stakeAddressFromCredential,
  GYStakeKeyHashString,
  stakeKeyFromAddress,

  -- * newtype wrapper
  GYStakeAddressBech32,
  stakeAddressToBech32,
  stakeAddressFromBech32,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Byron qualified as Api.B
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Chain.Common (addrToBase58)
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Control.Lens ((?~))
import Data.Aeson.Types qualified as Aeson
import Data.Csv qualified as Csv
import Data.Hashable (Hashable (..))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Swagger.Lens qualified ()
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.PostgreSQL.Simple qualified as PQ
import Database.PostgreSQL.Simple.FromField qualified as PQ (
  FromField (..),
  returnError,
 )
import Database.PostgreSQL.Simple.ToField qualified as PQ
import PlutusLedgerApi.V1.Address qualified as Plutus
import PlutusLedgerApi.V1.Credential qualified as Plutus
import PlutusLedgerApi.V1.Crypto qualified as Plutus
import PlutusLedgerApi.V1.Scripts qualified as Plutus
import PlutusTx.Builtins.Internal qualified as Plutus
import PlutusTx.Prelude qualified as PlutusTx
import Text.Printf qualified as Printf
import Web.HttpApiData qualified as Web

import Cardano.Api.Address qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import GeniusYield.Imports
import GeniusYield.Types.Credential (
  GYPaymentCredential,
  GYStakeCredential,
  paymentCredentialFromApi,
  paymentCredentialToApi,
  stakeCredentialFromApi,
  stakeCredentialToApi,
  stakeCredentialToHexText,
 )
import GeniusYield.Types.Era
import GeniusYield.Types.Ledger
import GeniusYield.Types.NetworkId
import GeniusYield.Types.PaymentKeyHash (
  GYPaymentKeyHash,
  paymentKeyHashToApi,
 )
import GeniusYield.Types.PubKeyHash
import GeniusYield.Types.Script

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Cardano.Api                as Api
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import GeniusYield.Types.NetworkId
>>> import qualified Text.Printf                as Printf
>>> import qualified Web.HttpApiData            as Web

>>> let addr = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
>>> let addrScript = unsafeAddressFromText "addr_test1wqtcz4vq80zxr3dskdcuw7wtfq0vwssd7rrpnnvcvrjhp5sx7leew"
>>> let addrByron1 = unsafeAddressFromText "Ae2tdPwUPEYwFx4dmJheyNPPYXtvHbJLeCaA96o6Y2iiUL18cAt7AizN2zG"
>>> let addrByron2 = unsafeAddressFromText "DdzFFzCqrhsn2RLCG6ogRgDxUUpkM3yNqyaSB3jq9YuuX1zARCJerbCoghG4PGiqwR1h8o4Jk7Mjgu3qhNixep5QAA8QgG9Dp2oE4eit"
>>> let stakeAddr = unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
-}

-- | Addresses on the blockchain.
newtype GYAddress = GYAddress Api.AddressAny
  deriving (Eq, Ord, Generic)

{- |

>>> let addr = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
-}

-- >>> show addr
-- addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5
--
instance Show GYAddress where
  showsPrec d addr =
    showParen (d > 10) $
      showString "unsafeAddressFromText "
        . showsPrec 11 (addressToText addr)

instance Hashable GYAddress where
  hashWithSalt salt (GYAddress addr) = hashWithSalt salt (Api.serialiseToRawBytes addr)

{- |

>>> addressToApi addr
AddressShelley (ShelleyAddress Testnet (KeyHashObj (KeyHash {unKeyHash = "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"})) (StakeRefBase (KeyHashObj (KeyHash {unKeyHash = "1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"}))))
>>> addressToApi addrByron1
AddressByron (ByronAddress (Address {addrRoot = 04865e42d2373addbebd5d2acf81c760c848970142889f7ee763091b, addrAttributes = Attributes { data_ = AddrAttributes {aaVKDerivationPath = Nothing, aaNetworkMagic = NetworkMainOrStage} }, addrType = ATVerKey}))
>>> addressToApi addrByron2
AddressByron (ByronAddress (Address {addrRoot = 3f04ff82d3008d3a4f3d2be7d66141dcbcbda74d6a805e463895b72a, addrAttributes = Attributes { data_ = AddrAttributes {aaVKDerivationPath = Just (HDAddressPayload {getHDAddressPayload = "\251C\"a\SUB\209\210M\245S\200S\144\160\190\237y[s\176\148\n3!\DLE\147\141\168"}), aaNetworkMagic = NetworkMainOrStage} }, addrType = ATVerKey}))
-}
addressToApi :: GYAddress -> Api.AddressAny
addressToApi = coerce

addressToApi' :: GYAddress -> Api.AddressInEra ApiEra
addressToApi' = coerce addrAnyToConwayEra

-- not exported
addrAnyToConwayEra :: Api.AddressAny -> Api.AddressInEra ApiEra
addrAnyToConwayEra (Api.AddressByron addr) = Api.AddressInEra Api.ByronAddressInAnyEra addr
addrAnyToConwayEra (Api.AddressShelley addr) = Api.AddressInEra (Api.ShelleyAddressInEra Api.ShelleyBasedEraConway) addr

addressFromApi :: Api.AddressAny -> GYAddress
addressFromApi = coerce

addressFromApi' :: Api.AddressInEra era -> GYAddress
addressFromApi' = coerce addressInEraToAny

-- not exported
addressInEraToAny :: Api.AddressInEra era -> Api.AddressAny
addressInEraToAny (Api.AddressInEra Api.ByronAddressInAnyEra a) = Api.AddressByron a
addressInEraToAny (Api.AddressInEra (Api.ShelleyAddressInEra _) a) = Api.AddressShelley a

-------------------------------------------------------------------------------
-- Plutus conversions
-------------------------------------------------------------------------------

{- |

>>> addressToPlutus addr
Address {addressCredential = PubKeyCredential e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d, addressStakingCredential = Just (StakingHash (PubKeyCredential 1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616))}
-}
addressToPlutus :: GYAddress -> Plutus.Address
addressToPlutus addr = case addressToApi addr of
  Api.AddressByron addr' -> byronAddressToPlutus addr'
  Api.AddressShelley addr' -> shelleyAddressToPlutus addr'

-- Lookup Ledger.Tx.CardanoAPI module in plutus-ledger.
byronAddressToPlutus :: Api.S.Address Api.S.ByronAddr -> Plutus.Address
byronAddressToPlutus (Api.B.ByronAddress addr) = Plutus.Address plutusCredential Nothing
 where
  plutusCredential :: Plutus.Credential
  plutusCredential = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ PlutusTx.toBuiltin $ addrToBase58 addr

shelleyAddressToPlutus :: Api.Address Api.ShelleyAddr -> Plutus.Address
shelleyAddressToPlutus (Api.S.ShelleyAddress _network credential stake) =
  Plutus.Address
    (shelleyCredentialToPlutus (Api.S.fromShelleyPaymentCredential credential))
    (shelleyStakeRefToPlutus (Api.S.fromShelleyStakeReference stake))

shelleyCredentialToPlutus :: Api.S.PaymentCredential -> Plutus.Credential
shelleyCredentialToPlutus (Api.S.PaymentCredentialByKey x) = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes x
shelleyCredentialToPlutus (Api.S.PaymentCredentialByScript x) = Plutus.ScriptCredential . Plutus.ScriptHash . PlutusTx.toBuiltin . Api.serialiseToRawBytes $ x

shelleyStakeRefToPlutus :: Api.S.StakeAddressReference -> Maybe Plutus.StakingCredential
shelleyStakeRefToPlutus Api.S.NoStakeAddress = Nothing
shelleyStakeRefToPlutus Api.StakeAddressByPointer {} = Nothing
shelleyStakeRefToPlutus (Api.StakeAddressByValue stakeCredential) = Just $ Plutus.StakingHash $ fromCardanoStakeCredential stakeCredential

fromCardanoStakeCredential :: Api.StakeCredential -> Plutus.Credential
fromCardanoStakeCredential (Api.S.StakeCredentialByKey x) = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes x
fromCardanoStakeCredential (Api.S.StakeCredentialByScript x) = Plutus.ScriptCredential $ Plutus.ScriptHash $ PlutusTx.toBuiltin $ Api.serialiseToRawBytes x

{- | Used to inject wallet pubkeyhashes into addresses.

>>> import GeniusYield.Types.NetworkId

>>> addressFromPlutus GYTestnetPreprod $ addressToPlutus addr
Right (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")
-}
addressFromPlutus :: GYNetworkId -> Plutus.Address -> Either PlutusToCardanoError GYAddress
addressFromPlutus nid addr =
  maybe
    (Left $ UnknownPlutusToCardanoError $ Text.pack $ "addressFromPlutus: " <> show addr)
    (Right . GYAddress . Api.S.AddressShelley)
    $ Api.S.ShelleyAddress nid' <$> paymentCredential <*> stakeReference
 where
  nid' :: Ledger.Network
  nid' = networkIdToLedger nid

  credential :: Plutus.Credential -> Maybe (Ledger.Credential kr Ledger.StandardCrypto)
  credential (Plutus.PubKeyCredential (Plutus.PubKeyHash (Plutus.BuiltinByteString bs))) = Ledger.KeyHashObj . Ledger.KeyHash <$> Crypto.hashFromBytes bs
  credential (Plutus.ScriptCredential (Plutus.ScriptHash (Plutus.BuiltinByteString bs))) = Ledger.ScriptHashObj . Ledger.ScriptHash <$> Crypto.hashFromBytes bs

  paymentCredential :: Maybe (Ledger.PaymentCredential Ledger.StandardCrypto)
  paymentCredential = credential $ Plutus.addressCredential addr

  stakeReference :: Maybe (Ledger.StakeReference Ledger.StandardCrypto)
  stakeReference = case Plutus.addressStakingCredential addr of
    Nothing -> Just Ledger.StakeRefNull
    Just (Plutus.StakingHash c) -> Ledger.StakeRefBase <$> credential c
    Just (Plutus.StakingPtr x y z) -> Ledger.StakeRefPtr <$> ptr x y z

  ptr :: Integer -> Integer -> Integer -> Maybe Ledger.Ptr
  ptr x y z = Ledger.Ptr <$> coerce integerToWord64 x <*> coerce integerToWord64 y <*> coerce integerToWord64 z

  integerToWord64 :: Integer -> Maybe Word64
  integerToWord64 n
    | n < 0 = Nothing
    | n > toInteger (maxBound @Word64) = Nothing
    | otherwise = Just $ fromInteger n

{- | If an address is a shelley address, then we'll return payment credential wrapped in `Just`, `Nothing` otherwise.

>>> addressToPaymentCredential addr
Just (GYCredentialByKey (GYKeyHash (GYKeyRolePayment) "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"))
>>> addressToPaymentCredential addrScript
Just (GYCredentialByScript (GYScriptHash "178155803bc461c5b0b371c779cb481ec7420df0c619cd9860e570d2"))
>>> addressToPaymentCredential addrByron1
Nothing
>>> addressToPaymentCredential addrByron2
Nothing
-}
addressToPaymentCredential :: GYAddress -> Maybe GYPaymentCredential
addressToPaymentCredential (addressToApi -> Api.AddressShelley addr) = Just $ getShelleyAddressPaymentCredential addr
addressToPaymentCredential _byron = Nothing

-- | Get payment credential part of a shelley address.
getShelleyAddressPaymentCredential :: Api.S.Address Api.ShelleyAddr -> GYPaymentCredential
getShelleyAddressPaymentCredential (Api.S.ShelleyAddress _network credential _stake) = Api.S.fromShelleyPaymentCredential credential & paymentCredentialFromApi

{- | If an address is a shelley address, then we'll return stake credential, if present, wrapped in `Just` and `Nothing` otherwise.

>>> addressToStakeCredential addr
Just (GYCredentialByKey (GYKeyHash (GYKeyRoleStaking) "1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"))
>>> addressToStakeCredential addrScript
Nothing
>>> addressToStakeCredential addrByron1
Nothing
>>> addressToStakeCredential addrByron2
Nothing
-}
addressToStakeCredential :: GYAddress -> Maybe GYStakeCredential
addressToStakeCredential (addressToApi -> Api.AddressShelley addr) = getShelleyAddressStakeCredential addr
addressToStakeCredential _byron = Nothing

-- | Get stake credential part of a shelley address, if present.
getShelleyAddressStakeCredential :: Api.S.Address Api.ShelleyAddr -> Maybe GYStakeCredential
getShelleyAddressStakeCredential (Api.S.ShelleyAddress _network _payment stake) =
  case Api.S.fromShelleyStakeReference stake of
    Api.S.StakeAddressByValue stakeCred -> Just $ stakeCredentialFromApi stakeCred
    _ -> Nothing

{- | Create address from 'GYPubKeyHash'.

/note:/ no stake credential.
-}
{-# DEPRECATED addressFromPubKeyHash "Use addressFromPaymentKeyHash." #-}
addressFromPubKeyHash :: GYNetworkId -> GYPubKeyHash -> GYAddress
addressFromPubKeyHash nid pkh =
  addressFromApi $
    Api.AddressShelley $
      Api.S.makeShelleyAddress
        (networkIdToApi nid)
        (Api.S.PaymentCredentialByKey (pubKeyHashToApi pkh))
        Api.S.NoStakeAddress

{- | Create address from 'GYPaymentKeyHash'.

/note:/ no stake credential.
-}
addressFromPaymentKeyHash :: GYNetworkId -> GYPaymentKeyHash -> GYAddress
addressFromPaymentKeyHash nid pkh =
  addressFromApi $
    Api.AddressShelley $
      Api.S.makeShelleyAddress
        (networkIdToApi nid)
        (Api.S.PaymentCredentialByKey (paymentKeyHashToApi pkh))
        Api.S.NoStakeAddress

{- | Create address from 'GYScriptHash'.

/note:/ no stake credential.
-}
addressFromValidatorHash :: GYNetworkId -> GYScriptHash -> GYAddress
addressFromValidatorHash nid vh = addressFromScriptHash' nid (validatorHashToApi vh)

-- | Create address from 'GYScriptHash'.
addressFromScriptHash :: GYNetworkId -> GYScriptHash -> GYAddress
addressFromScriptHash nid sh = addressFromScriptHash' nid (scriptHashToApi sh)

addressFromScriptHash' :: GYNetworkId -> Api.ScriptHash -> GYAddress
addressFromScriptHash' nid sh =
  addressFromApi $
    Api.AddressShelley $
      Api.S.makeShelleyAddress
        (networkIdToApi nid)
        (Api.S.PaymentCredentialByScript sh)
        Api.S.NoStakeAddress

-- | Create address from `GYSimpleScript`.
addressFromSimpleScript :: GYNetworkId -> GYSimpleScript -> GYAddress
addressFromSimpleScript nid script = addressFromScriptHash' nid (hashSimpleScript' script)

-- | Create an address from payment & optionally, a stake credential.
addressFromCredential :: GYNetworkId -> GYPaymentCredential -> Maybe GYStakeCredential -> GYAddress
addressFromCredential nid pc sc =
  addressFromApi $
    Api.AddressShelley $
      Api.S.makeShelleyAddress
        (networkIdToApi nid)
        (paymentCredentialToApi pc)
        (maybe Api.S.NoStakeAddress (Api.S.StakeAddressByValue . stakeCredentialToApi) sc)

{- | Create address from 'GYScript'.

/note:/ no stake credential.
-}
addressFromScript :: GYNetworkId -> GYScript v -> GYAddress
addressFromScript nid v = addressFromScriptHash nid (scriptHash v)

{- | Create address from 'GYScript'.

/note:/ no stake credential.
-}
addressFromValidator :: GYNetworkId -> GYScript v -> GYAddress
addressFromValidator nid v = addressFromValidatorHash nid (validatorHash v)

addressToPubKeyHash :: GYAddress -> Maybe GYPubKeyHash
addressToPubKeyHash (GYAddress (Api.AddressByron (Api.B.ByronAddress _addr))) =
  Nothing -- It's not clear what to do with these, and whether GY should support Byron addresses at all (as owners of pools)
addressToPubKeyHash (GYAddress (Api.AddressShelley (Api.S.ShelleyAddress _network credential _stake))) = f (Api.S.fromShelleyPaymentCredential credential)
 where
  f :: Api.S.PaymentCredential -> Maybe GYPubKeyHash
  f (Api.S.PaymentCredentialByKey h) = Just (pubKeyHashFromApi h)
  f (Api.S.PaymentCredentialByScript _) = Nothing

addressToValidatorHash :: GYAddress -> Maybe GYScriptHash
addressToValidatorHash (GYAddress (Api.AddressByron _)) = Nothing
addressToValidatorHash (GYAddress (Api.AddressShelley (Api.S.ShelleyAddress _network credential _stake))) = f (Api.S.fromShelleyPaymentCredential credential)
 where
  f :: Api.S.PaymentCredential -> Maybe GYScriptHash
  f (Api.S.PaymentCredentialByKey _) = Nothing
  f (Api.S.PaymentCredentialByScript h) = Just (validatorHashFromApi h)

-------------------------------------------------------------------------------
-- Text conversions
-------------------------------------------------------------------------------

addressFromTextMaybe :: Text.Text -> Maybe GYAddress
addressFromTextMaybe = coerce (Api.deserialiseAddress Api.AsAddressAny)

unsafeAddressFromText :: Text.Text -> GYAddress
unsafeAddressFromText t =
  fromMaybe
    (error $ "Not an address: " ++ show t)
    (addressFromTextMaybe t)

addressToText :: GYAddress -> Text.Text
addressToText (GYAddress addr) = Api.serialiseAddress addr

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

{- | This instance is using for logging

>>> Printf.printf "addr = %s" addr
addr = addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5
-}
instance Printf.PrintfArg GYAddress where
  formatArg addr = Printf.formatArg (addressToText addr)

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

{- | In JSON context addresses are represented in hex.

>>> Aeson.decode @GYAddress "\"00e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616\""
Just (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")
-}
instance Aeson.FromJSON GYAddress where
  parseJSON = Aeson.withText "GYAddress" $ \t ->
    case Web.parseUrlPiece t of
      Left err -> fail $ Text.unpack err
      Right addr -> return addr

{- |

>>> LBS8.putStrLn $ Aeson.encode addr
"00e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"
-}
instance Aeson.ToJSON GYAddress where
  toJSON = Aeson.String . Web.toUrlPiece

-------------------------------------------------------------------------------
-- http-api-data
-------------------------------------------------------------------------------

{- | In an HTTP context, addresses are represented in hex.

>>> Web.toUrlPiece addr
"00e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"
-}
instance Web.ToHttpApiData GYAddress where
  toUrlPiece (GYAddress addr) = TE.decodeLatin1 (Api.serialiseToRawBytesHex addr)

{- |

>>> Web.parseUrlPiece @GYAddress "00e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"
Right (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")

>>> Web.parseUrlPiece @GYAddress "00"
Left "Not an address: 00; Reason: RawBytesHexErrorRawBytesDecodeFail \"00\" AddressAny (SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise AddressAny\"})"
-}
instance Web.FromHttpApiData GYAddress where
  parseUrlPiece t = case Api.deserialiseFromRawBytesHex Api.AsAddressAny (TE.encodeUtf8 t) of
    Right addr -> Right (GYAddress addr)
    Left x -> Left $ "Not an address: " <> t <> "; Reason: " <> Text.pack (show x)

-------------------------------------------------------------------------------
-- CSV
-------------------------------------------------------------------------------

{- |

>>> Csv.toField $ unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
-}
instance Csv.ToField GYAddress where
  toField = encodeUtf8 . addressToText

{- |

>>> Csv.runParser $ Csv.parseField @GYAddress "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
Right (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")

>>> Csv.runParser $ Csv.parseField @GYAddress "not an address"
Left "Not an address: not an address"
-}
instance Csv.FromField GYAddress where
  parseField = either (fail . Text.unpack) (return . addressFromBech32) . Web.parseUrlPiece . decodeUtf8Lenient

{- |

>>> Csv.encodeWith (Csv.defaultEncodeOptions {Csv.encUseCrLf = False}) [unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"]
"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5\n"
-}
instance Csv.ToRecord GYAddress where
  toRecord = Vector.singleton . Csv.toField

{- |

>>> Csv.decode @GYAddress Csv.NoHeader "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5\n"
Right [unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"]

>>> Csv.decode @GYAddress Csv.NoHeader "not an address\n"
Left "parse error (Failed reading: conversion error: Not an address: not an address) at \"\\n\""

>>> Csv.decode @GYAddress Csv.NoHeader "not, an, address\n"
Left "parse error (Failed reading: conversion error: expected exactly one field, but got: [\"not\",\" an\",\" address\"]) at \"\\n\""
-}
instance Csv.FromRecord GYAddress where
  parseRecord v = case Vector.toList v of
    [bs] -> Csv.parseField bs
    _ -> fail $ printf "expected exactly one field, but got: %s" $ show v

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYAddress where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "cbor hex"
        & Swagger.maxLength
      ?~ 114
        & Swagger.minLength
      ?~ 114

instance Swagger.ToSchema GYAddress where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYAddress" $
        Swagger.paramSchemaToSchema (Proxy @GYAddress)
          & Swagger.description
          ?~ "An address, serialised as CBOR."
            & Swagger.example
          ?~ toJSON ("00e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616" :: Text)

-------------------------------------------------------------------------------
-- newtype
-------------------------------------------------------------------------------

{- | 'GYAddressBech32' which uses "bech32" format

>>> Web.toUrlPiece $ addressToBech32 addr
"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
-}
newtype GYAddressBech32 = GYAddressBech32 GYAddress
  deriving newtype (Show, Eq, Ord, Printf.PrintfArg)

addressToBech32 :: GYAddress -> GYAddressBech32
addressToBech32 = coerce

addressFromBech32 :: GYAddressBech32 -> GYAddress
addressFromBech32 = coerce

instance Web.ToHttpApiData GYAddressBech32 where
  toUrlPiece = coerce addressToText

instance IsString GYAddressBech32 where
  fromString = fromRight (error "invalid address") . Web.parseUrlPiece . Text.pack

{- |

>>> Web.parseUrlPiece @GYAddressBech32 "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
Right (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")
-}
instance Web.FromHttpApiData GYAddressBech32 where
  parseUrlPiece t = case addressFromTextMaybe t of
    Just addr -> Right $ coerce addr
    Nothing -> Left $ "Not an address: " <> t

{- |

>>> LBS8.putStrLn $ Aeson.encode $ addressToBech32 addr
"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"
-}
instance ToJSON GYAddressBech32 where
  toJSON (GYAddressBech32 addr) = Aeson.toJSON $ addressToText addr

{- |

>>> Aeson.decode @GYAddressBech32 "\"addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5\""
Just (unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5")
-}
instance FromJSON GYAddressBech32 where
  parseJSON = Aeson.withText "GYAddressBech32" $ \t ->
    case Api.deserialiseAddress Api.AsAddressAny t of
      Just addr -> return $ GYAddressBech32 $ GYAddress addr
      Nothing -> fail "cannot deserialise address"

instance PQ.ToField GYAddressBech32 where
  toField (GYAddressBech32 addr) = PQ.toField $ addressToText addr

instance PQ.FromField GYAddressBech32 where
  fromField f bs = do
    t <- PQ.fromField f bs
    case Api.deserialiseAddress Api.AsAddressAny t of
      Just addr -> return $ GYAddressBech32 $ GYAddress addr
      Nothing -> PQ.returnError PQ.ConversionFailed f "address does not unserialise"

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToSchema GYAddressBech32 where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYAddressBech32" $
        Swagger.paramSchemaToSchema (Proxy @GYAddressBech32)
          & Swagger.description
          ?~ "An address, serialised as Bech32."
            & Swagger.example
          ?~ toJSON ("addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5" :: Text)

instance Swagger.ToParamSchema GYAddressBech32 where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "bech32"

-------------------------------------------------------------------------------
-- Stake Address
-------------------------------------------------------------------------------

-- | Stake Address.
newtype GYStakeAddress = GYStakeAddress Api.StakeAddress
  deriving (Eq, Ord, Generic)

-- | Get @GY@ type from corresponding type in @cardano-api@ library.
stakeAddressFromApi :: Api.StakeAddress -> GYStakeAddress
stakeAddressFromApi = coerce

-- | Convert @GY@ type to corresponding type in @cardano-api@ library.
stakeAddressToApi :: GYStakeAddress -> Api.StakeAddress
stakeAddressToApi = coerce

{- | Obtain `GYStakeAddress` from bech32 encoding of stake address.

>>> stakeAddressFromTextMaybe "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
Just (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")
>>> stakeAddressFromTextMaybe "e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
Nothing
-}
stakeAddressFromTextMaybe :: Text.Text -> Maybe GYStakeAddress
stakeAddressFromTextMaybe = coerce (Api.deserialiseAddress Api.AsStakeAddress)

-- | Like `stakeAddressFromTextMaybe` but errors on `Nothing` case.
unsafeStakeAddressFromText :: Text.Text -> GYStakeAddress
unsafeStakeAddressFromText t =
  fromMaybe
    (error $ "Not a stake address: " ++ show t)
    (stakeAddressFromTextMaybe t)

{- | Serialises `GYStakeAddress` to it's bech32 representation.

>>> stakeAddressToText stakeAddr
"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
-}
stakeAddressToText :: GYStakeAddress -> Text.Text
stakeAddressToText = Api.serialiseAddress . stakeAddressToApi

stakeAddressToLedger :: GYStakeAddress -> Ledger.RewardAccount Ledger.StandardCrypto
stakeAddressToLedger (stakeAddressToApi -> Api.StakeAddress nw sc) = Ledger.RewardAccount nw sc

{-# DEPRECATED stakeAddressCredential "Use stakeAddressToCredential." #-}

-- | Get a stake credential from a stake address. This drops the network information.
stakeAddressCredential :: GYStakeAddress -> GYStakeCredential
stakeAddressCredential = stakeCredentialFromApi . Api.stakeAddressCredential . stakeAddressToApi

-- | Get a stake credential from a stake address. This drops the network information.
stakeAddressToCredential :: GYStakeAddress -> GYStakeCredential
stakeAddressToCredential = stakeAddressCredential

{- | Get a stake address from a stake credential. This also requires network information.

>>> stakeAddr == stakeAddressFromCredential GYTestnetPreprod (stakeAddressToCredential stakeAddr)
True
-}
stakeAddressFromCredential :: GYNetworkId -> GYStakeCredential -> GYStakeAddress
stakeAddressFromCredential (networkIdToApi -> netId') (stakeCredentialToApi -> stakeCred') = Api.makeStakeAddress netId' stakeCred' & stakeAddressFromApi

type GYStakeKeyHashString = String

{- |

>>> stakeKeyFromAddress addr
Just "1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f616"
-}
stakeKeyFromAddress :: GYAddress -> Maybe GYStakeKeyHashString
stakeKeyFromAddress addr = addressToStakeCredential addr >>= Just . Text.unpack . stakeCredentialToHexText

instance Show GYStakeAddress where
  showsPrec d rewAddr =
    showParen (d > 10) $
      showString "unsafeStakeAddressFromText "
        . showsPrec 11 (stakeAddressToText rewAddr)

instance Hashable GYStakeAddress where
  hashWithSalt salt = hashWithSalt salt . Api.serialiseToRawBytes . stakeAddressToApi

{- | In JSON context, stake addresses are represented in hex.

>>> Aeson.decode @GYStakeAddress "\"e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
Just (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")
-}
instance Aeson.FromJSON GYStakeAddress where
  parseJSON = Aeson.withText "GYStakeAddress" $ \t ->
    case Web.parseUrlPiece t of
      Left err -> fail $ Text.unpack err
      Right addr -> return addr

{- |

>>> LBS8.putStrLn $ Aeson.encode stakeAddr
"e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
instance Aeson.ToJSON GYStakeAddress where
  toJSON = Aeson.String . Web.toUrlPiece

-------------------------------------------------------------------------------
-- http-api-data
-------------------------------------------------------------------------------

{- | In an HTTP context, stake addresses are represented in hex.

>>> Web.toUrlPiece stakeAddr
"e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
instance Web.ToHttpApiData GYStakeAddress where
  toUrlPiece = TE.decodeLatin1 . Api.serialiseToRawBytesHex . stakeAddressToApi

{- |

>>> Web.parseUrlPiece @GYStakeAddress "e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
Right (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")

>>> Web.parseUrlPiece @GYStakeAddress "00"
Left "Not a stake address: 00; Reason: RawBytesHexErrorRawBytesDecodeFail \"00\" StakeAddress (SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise StakeAddress\"})"
-}
instance Web.FromHttpApiData GYStakeAddress where
  parseUrlPiece t = case Api.deserialiseFromRawBytesHex Api.AsStakeAddress (TE.encodeUtf8 t) of
    Right addr -> Right $ stakeAddressFromApi addr
    Left x -> Left $ "Not a stake address: " <> t <> "; Reason: " <> Text.pack (show x)

-------------------------------------------------------------------------------
-- CSV
-------------------------------------------------------------------------------

{- |

>>> Csv.toField stakeAddr
"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
-}
instance Csv.ToField GYStakeAddress where
  toField = encodeUtf8 . stakeAddressToText

{- |

>>> Csv.runParser $ Csv.parseField @GYStakeAddress "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
Right (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")

>>> Csv.runParser $ Csv.parseField @GYStakeAddress "not a stake address"
Left "Not a stake address: not a stake address"
-}
instance Csv.FromField GYStakeAddress where
  parseField f =
    let t = decodeUtf8Lenient f
     in maybe (fail $ "Not a stake address: " <> Text.unpack t) return $ stakeAddressFromTextMaybe t

{- |

>>> Csv.encodeWith (Csv.defaultEncodeOptions {Csv.encUseCrLf = False}) [stakeAddr]
"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3\n"
-}
instance Csv.ToRecord GYStakeAddress where
  toRecord = Vector.singleton . Csv.toField

{- |

>>> Csv.decode @GYStakeAddress Csv.NoHeader "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3\n"
Right [unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"]

>>> Csv.decode @GYStakeAddress Csv.NoHeader "not a stake address\n"
Left "parse error (Failed reading: conversion error: Not a stake address: not a stake address) at \"\\n\""

>>> Csv.decode @GYStakeAddress Csv.NoHeader "not, a, stake address\n"
Left "parse error (Failed reading: conversion error: expected exactly one field, but got: [\"not\",\" a\",\" stake address\"]) at \"\\n\""
-}
instance Csv.FromRecord GYStakeAddress where
  parseRecord v = case Vector.toList v of
    [bs] -> Csv.parseField bs
    _ -> fail $ printf "expected exactly one field, but got: %s" $ show v

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYStakeAddress where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "cbor hex"
        & Swagger.maxLength
      ?~ 58
        & Swagger.minLength
      ?~ 58

instance Swagger.ToSchema GYStakeAddress where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYStakeAddress" $
        Swagger.paramSchemaToSchema (Proxy @GYStakeAddress)
          & Swagger.description
          ?~ "A stake address, serialised as CBOR."
            & Swagger.example
          ?~ toJSON ("e07a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d" :: Text)

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

{- | This instance is using for logging

>>> Printf.printf "stake addr = %s" stakeAddr
stake addr = stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3
-}
instance Printf.PrintfArg GYStakeAddress where
  formatArg stakeAddr = Printf.formatArg (stakeAddressToText stakeAddr)

{- | 'GYStakeAddressBech32' which uses "bech32" format

>>> Web.toUrlPiece $ stakeAddressToBech32 stakeAddr
"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
-}
newtype GYStakeAddressBech32 = GYStakeAddressBech32 GYStakeAddress
  deriving newtype (Show, Eq, Ord, Printf.PrintfArg)

stakeAddressToBech32 :: GYStakeAddress -> GYStakeAddressBech32
stakeAddressToBech32 = coerce

stakeAddressFromBech32 :: GYStakeAddressBech32 -> GYStakeAddress
stakeAddressFromBech32 = coerce

instance Web.ToHttpApiData GYStakeAddressBech32 where
  toUrlPiece = coerce stakeAddressToText

instance IsString GYStakeAddressBech32 where
  fromString = fromRight (error "invalid stake address") . Web.parseUrlPiece . Text.pack

{- |

>>> Web.parseUrlPiece @GYStakeAddressBech32 "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
Right (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")
-}
instance Web.FromHttpApiData GYStakeAddressBech32 where
  parseUrlPiece t = case stakeAddressFromTextMaybe t of
    Just stakeAddr -> Right $ coerce stakeAddr
    Nothing -> Left $ "Not a stake address: " <> t

{- |

>>> LBS8.putStrLn $ Aeson.encode $ stakeAddressToBech32 stakeAddr
"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3"
-}
instance ToJSON GYStakeAddressBech32 where
  toJSON (GYStakeAddressBech32 stakeAddr) = Aeson.toJSON $ stakeAddressToText stakeAddr

{- |

>>> Aeson.decode @GYStakeAddressBech32 "\"stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3\""
Just (unsafeStakeAddressFromText "stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3")
-}
instance FromJSON GYStakeAddressBech32 where
  parseJSON = Aeson.withText "GYStakeAddressBech32" $ \t ->
    case stakeAddressFromTextMaybe t of
      Just stakeAddr -> return $ GYStakeAddressBech32 stakeAddr
      Nothing -> fail "cannot deserialise stake address"

instance PQ.ToField GYStakeAddressBech32 where
  toField (GYStakeAddressBech32 stakeAddr) = PQ.toField $ stakeAddressToText stakeAddr

instance PQ.FromField GYStakeAddressBech32 where
  fromField f bs = do
    t <- PQ.fromField f bs
    case stakeAddressFromTextMaybe t of
      Just stakeAddr -> return $ GYStakeAddressBech32 stakeAddr
      Nothing -> PQ.returnError PQ.ConversionFailed f "stake address does not unserialise"

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToSchema GYStakeAddressBech32 where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYStakeAddressBech32" $
        Swagger.paramSchemaToSchema (Proxy @GYStakeAddressBech32)
          & Swagger.description
          ?~ "A stake address, serialised as Bech32."
            & Swagger.example
          ?~ toJSON ("stake_test1upa805fqh85x4hw88zxmhvdaydgyjzmazs9tydqrscerxnghfq4t3" :: Text)

instance Swagger.ToParamSchema GYStakeAddressBech32 where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "bech32"
