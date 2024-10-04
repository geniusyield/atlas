{- |
Module      : GeniusYield.Types.Credential
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Credential (
  -- * Payment credential.
  GYPaymentCredential (..),
  paymentCredentialToApi,
  paymentCredentialFromApi,
  paymentCredentialToLedger,
  paymentCredentialFromLedger,
  paymentCredentialToPlutus,
  paymentCredentialToHexText,
  paymentCredentialToBech32,

  -- * Stake credential.
  GYStakeCredential (..),
  stakeCredentialToApi,
  stakeCredentialFromApi,
  stakeCredentialToLedger,
  stakeCredentialFromLedger,
  stakeCredentialToPlutus,
  stakeCredentialToHexText,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Data.Hashable (Hashable (..))
import Data.Text (Text)
import GeniusYield.Imports ((>>>))
import GeniusYield.Types.PaymentKeyHash (
  GYPaymentKeyHash,
  paymentKeyHashFromApi,
  paymentKeyHashFromLedger,
  paymentKeyHashToApi,
  paymentKeyHashToLedger,
  paymentKeyHashToPlutus,
 )
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (fromPubKeyHash, toPubKeyHash))
import GeniusYield.Types.Script (
  GYScriptHash,
  scriptHashFromApi,
  scriptHashFromLedger,
  scriptHashToApi,
  scriptHashToLedger,
  scriptHashToPlutus,
  stakeValidatorHashFromApi,
  stakeValidatorHashToApi,
  stakeValidatorHashToPlutus,
 )
import GeniusYield.Types.StakeKeyHash (
  GYStakeKeyHash,
  stakeKeyHashFromApi,
  stakeKeyHashToApi,
 )
import GeniusYield.Utils (serialiseToBech32WithPrefix)
import PlutusLedgerApi.V1 qualified as Plutus (Credential (..))
import Text.Printf qualified as Printf

-- | Payment credential.
data GYPaymentCredential
  = GYPaymentCredentialByKey !GYPaymentKeyHash
  | GYPaymentCredentialByScript !GYScriptHash
  deriving (Show, Eq, Ord)

instance Printf.PrintfArg GYPaymentCredential where
  formatArg (GYPaymentCredentialByKey pkh) = Printf.formatArg $ "Payment key credential: " <> Api.serialiseToRawBytesHexText (paymentKeyHashToApi pkh)
  formatArg (GYPaymentCredentialByScript sh) = Printf.formatArg $ "Payment script credential: " <> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

instance Hashable GYPaymentCredential where
  hashWithSalt salt cred = hashWithSalt salt $ paymentCredentialToHexText cred

-- | Convert @GY@ type to corresponding type in @cardano-node@ library.
paymentCredentialToApi :: GYPaymentCredential -> Api.PaymentCredential
paymentCredentialToApi (GYPaymentCredentialByKey pkh) = Api.PaymentCredentialByKey (paymentKeyHashToApi pkh)
paymentCredentialToApi (GYPaymentCredentialByScript sh) = Api.PaymentCredentialByScript (scriptHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-node@ library.
paymentCredentialFromApi :: Api.PaymentCredential -> GYPaymentCredential
paymentCredentialFromApi (Api.PaymentCredentialByKey pkh) = GYPaymentCredentialByKey (paymentKeyHashFromApi pkh)
paymentCredentialFromApi (Api.PaymentCredentialByScript sh) = GYPaymentCredentialByScript (scriptHashFromApi sh)

-- | Convert to corresponding ledger representation.
paymentCredentialToLedger :: GYPaymentCredential -> Ledger.Credential Ledger.Payment Ledger.StandardCrypto
paymentCredentialToLedger pc = case pc of
  GYPaymentCredentialByKey kh -> Ledger.KeyHashObj $ paymentKeyHashToLedger kh
  GYPaymentCredentialByScript sh -> Ledger.ScriptHashObj $ scriptHashToLedger sh

paymentCredentialFromLedger :: Ledger.Credential Ledger.Payment Ledger.StandardCrypto -> GYPaymentCredential
paymentCredentialFromLedger c = case c of
  Ledger.KeyHashObj kh -> GYPaymentCredentialByKey $ paymentKeyHashFromLedger kh
  Ledger.ScriptHashObj sh -> GYPaymentCredentialByScript $ scriptHashFromLedger sh

-- | Convert @GY@ type to corresponding type in @plutus@ library.
paymentCredentialToPlutus :: GYPaymentCredential -> Plutus.Credential
paymentCredentialToPlutus (GYPaymentCredentialByKey pkh) = Plutus.PubKeyCredential (paymentKeyHashToPlutus pkh)
paymentCredentialToPlutus (GYPaymentCredentialByScript sh) = Plutus.ScriptCredential (scriptHashToPlutus sh)

-- | Get hexadecimal value of payment credential.
paymentCredentialToHexText :: GYPaymentCredential -> Text
paymentCredentialToHexText =
  \case
    GYPaymentCredentialByKey pkh -> Api.serialiseToRawBytesHexText (paymentKeyHashToApi pkh)
    GYPaymentCredentialByScript sh -> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

-- | Get the bech32 encoding for the given credential.
paymentCredentialToBech32 :: GYPaymentCredential -> Text
paymentCredentialToBech32 (GYPaymentCredentialByKey pkh) = serialiseToBech32WithPrefix "addr_vkh" $ paymentKeyHashToApi pkh
paymentCredentialToBech32 (GYPaymentCredentialByScript sh) = serialiseToBech32WithPrefix "addr_shared_vkh" $ scriptHashToApi sh

-- | Stake credential.
data GYStakeCredential
  = GYStakeCredentialByKey !GYStakeKeyHash
  | GYStakeCredentialByScript !GYScriptHash
  deriving (Show, Eq, Ord)

instance Printf.PrintfArg GYStakeCredential where
  formatArg (GYStakeCredentialByKey skh) = Printf.formatArg $ "Stake key credential: " <> Api.serialiseToRawBytesHexText (stakeKeyHashToApi skh)
  formatArg (GYStakeCredentialByScript sh) = Printf.formatArg $ "Stake script credential: " <> Api.serialiseToRawBytesHexText (stakeValidatorHashToApi sh)

-- | Convert @GY@ type to corresponding type in @cardano-api@ library.
stakeCredentialToApi :: GYStakeCredential -> Api.StakeCredential
stakeCredentialToApi (GYStakeCredentialByKey skh) = Api.StakeCredentialByKey (stakeKeyHashToApi skh)
stakeCredentialToApi (GYStakeCredentialByScript sh) = Api.StakeCredentialByScript (stakeValidatorHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-api@ library.
stakeCredentialFromApi :: Api.StakeCredential -> GYStakeCredential
stakeCredentialFromApi (Api.StakeCredentialByKey skh) = GYStakeCredentialByKey (stakeKeyHashFromApi skh)
stakeCredentialFromApi (Api.StakeCredentialByScript sh) = GYStakeCredentialByScript (stakeValidatorHashFromApi sh)

-- | Convert to corresponding ledger type.
stakeCredentialToLedger :: GYStakeCredential -> Ledger.Credential Ledger.Staking Ledger.StandardCrypto
stakeCredentialToLedger = stakeCredentialToApi >>> Api.toShelleyStakeCredential

-- | Convert from corresponding ledger type.
stakeCredentialFromLedger :: Ledger.Credential Ledger.Staking Ledger.StandardCrypto -> GYStakeCredential
stakeCredentialFromLedger = Api.fromShelleyStakeCredential >>> stakeCredentialFromApi

-- | Convert @GY@ type to corresponding type in @plutus@ library.
stakeCredentialToPlutus :: GYStakeCredential -> Plutus.Credential
stakeCredentialToPlutus (GYStakeCredentialByKey pkh) = Plutus.PubKeyCredential (paymentKeyHashToPlutus $ fromPubKeyHash $ toPubKeyHash pkh)
stakeCredentialToPlutus (GYStakeCredentialByScript sh) = Plutus.ScriptCredential (stakeValidatorHashToPlutus sh)

-- | Get hexadecimal value of stake credential.
stakeCredentialToHexText :: GYStakeCredential -> Text
stakeCredentialToHexText =
  \case
    GYStakeCredentialByKey skh -> Api.serialiseToRawBytesHexText (stakeKeyHashToApi skh)
    GYStakeCredentialByScript sh -> Api.serialiseToRawBytesHexText (stakeValidatorHashToApi sh)
