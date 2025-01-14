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
  GYStakeCredential,
  pattern GYStakeCredentialByKey,
  pattern GYStakeCredentialByScript,
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
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import GeniusYield.Types.PaymentKeyHash (
  paymentKeyHashFromApi,
  paymentKeyHashToApi,
 )
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (toPubKeyHash), pubKeyHashToPlutus)
import GeniusYield.Types.Script (
  GYScriptHash,
  scriptHashFromApi,
  scriptHashFromLedger,
  scriptHashToApi,
  scriptHashToLedger,
  scriptHashToPlutus,
  stakeValidatorHashFromApi,
  stakeValidatorHashToApi,
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
>>> import GeniusYield.Types.KeyHash
>>> let pkh :: GYKeyHash 'GYKeyRolePayment = "ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"
>>> let pcred = GYCredentialByKey pkh
>>> let scred = GYCredentialByScript "464eeee89f05aff787d40045af2a40a83fd96c513197d32fbc54ff02"
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
paymentCredentialToApi (GYPaymentCredentialByKey pkh) = Api.PaymentCredentialByKey (paymentKeyHashToApi pkh)
paymentCredentialToApi (GYPaymentCredentialByScript sh) = Api.PaymentCredentialByScript (scriptHashToApi sh)

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

{- | Stake credential.
@type GYStakeCredential = GYCredential 'GYKeyRoleStaking@
-}
type GYStakeCredential = GYCredential 'GYKeyRoleStaking

pattern GYStakeCredentialByKey :: GYStakeKeyHash -> GYStakeCredential
pattern GYStakeCredentialByKey kh = GYCredentialByKey kh
pattern GYStakeCredentialByScript :: GYScriptHash -> GYStakeCredential
pattern GYStakeCredentialByScript sh = GYCredentialByScript sh

{-# COMPLETE GYStakeCredentialByKey, GYStakeCredentialByScript #-}

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
stakeCredentialToLedger = credentialToLedger

-- | Convert from corresponding ledger type.
stakeCredentialFromLedger :: Ledger.Credential Ledger.Staking Ledger.StandardCrypto -> GYStakeCredential
stakeCredentialFromLedger = credentialFromLedger

-- | Convert @GY@ type to corresponding type in @plutus@ library.
stakeCredentialToPlutus :: GYStakeCredential -> Plutus.Credential
stakeCredentialToPlutus = credentialToPlutus

-- | Get hexadecimal value of stake credential.
stakeCredentialToHexText :: GYStakeCredential -> Text
stakeCredentialToHexText = credentialToHexText

-- | Credential.
data GYCredential (kr :: GYKeyRole)
  = GYCredentialByKey !(GYKeyHash kr)
  | GYCredentialByScript !GYScriptHash
  deriving stock (Show, Eq, Ord)

{- | Get hexadecimal value of credential.

>>> credentialToHexText pcred
"ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81"

>>> credentialToHexText scred
"464eeee89f05aff787d40045af2a40a83fd96c513197d32fbc54ff02"
-}
credentialToHexText :: GYCredential kr -> Text
credentialToHexText =
  \case
    GYCredentialByKey kh -> keyHashToRawBytesHexText kh
    GYCredentialByScript sh -> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

{- |
>>> Printf.printf "%s\n" $ pcred
Key credential (GYKeyRolePayment): ec91ac77b581ba928db86cd91d11e64032450677c6b80748ce0b9a81
-}
instance SingGYKeyRoleI kr => Printf.PrintfArg (GYCredential kr) where
  formatArg (GYCredentialByKey kh) = Printf.formatArg $ "Key credential (" <> show (fromSingGYKeyRole $ singGYKeyRole @kr) <> "): " <> Text.unpack (keyHashToRawBytesHexText kh)
  formatArg (GYCredentialByScript sh) = Printf.formatArg $ "Script credential: " <> Api.serialiseToRawBytesHexText (scriptHashToApi sh)

instance Hashable (GYCredential kr) where
  hashWithSalt salt cred = hashWithSalt salt $ credentialToHexText cred

credentialToLedger :: GYCredential kr -> Ledger.Credential (GYKeyRoleToLedger kr) Ledger.StandardCrypto
credentialToLedger (GYCredentialByKey kh) = Ledger.KeyHashObj $ keyHashToLedger kh
credentialToLedger (GYCredentialByScript sh) = Ledger.ScriptHashObj $ scriptHashToLedger sh

credentialFromLedger :: Ledger.Credential (GYKeyRoleToLedger kr) Ledger.StandardCrypto -> GYCredential kr
credentialFromLedger (Ledger.KeyHashObj kh) = GYCredentialByKey $ keyHashFromLedger kh
credentialFromLedger (Ledger.ScriptHashObj sh) = GYCredentialByScript $ scriptHashFromLedger sh

credentialToPlutus :: GYCredential kr -> Plutus.Credential
credentialToPlutus (GYCredentialByKey kh) = Plutus.PubKeyCredential (pubKeyHashToPlutus $ toPubKeyHash kh)
credentialToPlutus (GYCredentialByScript sh) = Plutus.ScriptCredential (scriptHashToPlutus sh)
