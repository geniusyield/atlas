{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : GeniusYield.Types.Credential
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Credential (
  -- * Credential.
  GYCredential (..),
  credentialToHexText,
  credentialToLedger,
  credentialFromLedger,
  credentialToPlutus,

  -- * Payment credential.
  GYPaymentCredential,
  pattern GYPaymentCredentialByKey,
  pattern GYPaymentCredentialByScript,
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
import Data.Text qualified as Text
import GeniusYield.Imports ((>>>))
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import GeniusYield.Types.PaymentKeyHash (
  GYPaymentKeyHash,
  paymentKeyHashFromApi,
  paymentKeyHashFromLedger,
  paymentKeyHashToApi,
  paymentKeyHashToLedger,
  paymentKeyHashToPlutus,
 )
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (fromPubKeyHash, toPubKeyHash), pubKeyHashToPlutus)
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

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications -XDataKinds
>>> import qualified Text.Printf                as Printf
>>> import GeniusYield.Types.KeyRole
>>> let pkh :: GYKeyHash 'GYKeyRolePayment = "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
>>> let pcred = GYCredentialByKey pkh
-}

{- | Payment credential.
@type GYPaymentCredential = GYCredential 'GYKeyRolePayment@
-}
type GYPaymentCredential = GYCredential 'GYKeyRolePayment

pattern GYPaymentCredentialByKey :: GYKeyHash 'GYKeyRolePayment -> GYPaymentCredential
pattern GYPaymentCredentialByKey kh = GYCredentialByKey kh
pattern GYPaymentCredentialByScript :: GYScriptHash -> GYPaymentCredential
pattern GYPaymentCredentialByScript sh = GYCredentialByScript sh

{-# COMPLETE GYPaymentCredentialByKey, GYPaymentCredentialByScript #-}

-- | Convert @GY@ type to corresponding type in @cardano-node@ library.
paymentCredentialToApi :: GYPaymentCredential -> Api.PaymentCredential
paymentCredentialToApi = credentialToApi

-- | Get @GY@ type from corresponding type in @cardano-node@ library.
paymentCredentialFromApi :: Api.PaymentCredential -> GYPaymentCredential
paymentCredentialFromApi (Api.PaymentCredentialByKey pkh) = GYPaymentCredentialByKey (paymentKeyHashFromApi pkh)
paymentCredentialFromApi (Api.PaymentCredentialByScript sh) = GYPaymentCredentialByScript (scriptHashFromApi sh)

-- | Convert to corresponding ledger representation.
paymentCredentialToLedger :: GYPaymentCredential -> Ledger.Credential Ledger.Payment Ledger.StandardCrypto
paymentCredentialToLedger = credentialToLedger

paymentCredentialFromLedger :: Ledger.Credential Ledger.Payment Ledger.StandardCrypto -> GYPaymentCredential
paymentCredentialFromLedger = credentialFromLedger

-- | Convert @GY@ type to corresponding type in @plutus@ library.
paymentCredentialToPlutus :: GYPaymentCredential -> Plutus.Credential
paymentCredentialToPlutus = credentialToPlutus

-- | Get hexadecimal value of payment credential.
paymentCredentialToHexText :: GYPaymentCredential -> Text
paymentCredentialToHexText = credentialToHexText

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

data GYCredential (kr :: GYKeyRole)
  = GYCredentialByKey !(GYKeyHash kr)
  | GYCredentialByScript !GYScriptHash
  deriving stock (Show, Eq, Ord)

-- | Get hexadecimal value of credential.
credentialToHexText :: GYCredential kr -> Text
credentialToHexText =
  \case
    GYCredentialByKey kh -> keyHashToRawBytesHexText kh
    GYCredentialByScript sh -> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

-- >>> Printf.printf "%s\n" $ pcred
instance SingGYKeyRoleI kr => Printf.PrintfArg (GYCredential kr) where
  formatArg (GYCredentialByKey kh) = Printf.formatArg $ "Key credential of role (" <> show (fromSingGYKeyRole $ singGYKeyRole @kr) <> "): " <> Text.unpack (keyHashToRawBytesHexText kh)
  formatArg (GYCredentialByScript sh) = Printf.formatArg $ "Script credential: " <> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

instance Hashable (GYCredential kr) where
  hashWithSalt salt cred = hashWithSalt salt $ credentialToHexText cred

type family GYCredentialToApi (kr :: GYKeyRole) where
  GYCredentialToApi 'GYKeyRolePayment = Api.PaymentCredential
  GYCredentialToApi 'GYKeyRoleStaking = Api.StakeCredential

credentialToApi :: forall kr. SingGYKeyRoleI kr => GYCredential kr -> GYCredentialToApi kr
credentialToApi cr = case (singGYKeyRole @kr) of
  SingGYKeyRolePayment -> case cr of
    GYCredentialByKey kh -> Api.PaymentCredentialByKey (keyHashToApi kh)
    GYCredentialByScript sh -> Api.PaymentCredentialByScript (scriptHashToApi sh)
  SingGYKeyRoleStaking -> case cr of
    GYCredentialByKey kh -> Api.StakeCredentialByKey (keyHashToApi kh)
    GYCredentialByScript sh -> Api.StakeCredentialByScript (scriptHashToApi sh)

credentialToLedger :: GYCredential kr -> Ledger.Credential (GYKeyRoleToLedger kr) Ledger.StandardCrypto
credentialToLedger (GYCredentialByKey kh) = Ledger.KeyHashObj $ keyHashToLedger kh
credentialToLedger (GYCredentialByScript sh) = Ledger.ScriptHashObj $ scriptHashToLedger sh

credentialFromLedger :: Ledger.Credential (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYCredential kr
credentialFromLedger (Ledger.KeyHashObj kh) = GYCredentialByKey $ keyHashFromLedger kh
credentialFromLedger (Ledger.ScriptHashObj sh) = GYCredentialByScript $ scriptHashFromLedger sh

credentialToPlutus :: GYCredential kr -> Plutus.Credential
credentialToPlutus (GYCredentialByKey kh) = Plutus.PubKeyCredential (pubKeyHashToPlutus $ toPubKeyHash kh)
credentialToPlutus (GYCredentialByScript sh) = Plutus.ScriptCredential (scriptHashToPlutus sh)