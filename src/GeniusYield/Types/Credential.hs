{-|
Module      : GeniusYield.Types.Credential
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Credential (
    -- * Payment credential.
    GYPaymentCredential (..)
  , paymentCredentialToApi
  , paymentCredentialFromApi
  , paymentCredentialToPlutus
  , paymentCredentialToHexText
  , paymentCredentialToBech32
    -- * Stake credential.
  , GYStakeCredential (..)
  , stakeCredentialFromApi
  , stakeCredentialToApi
  , stakeCredentialToHexText
  ) where


import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api
import           Data.Hashable                    (Hashable (..))
import           Data.Text                        (Text)
import           GeniusYield.Types.PaymentKeyHash (GYPaymentKeyHash,
                                                   paymentKeyHashFromApi,
                                                   paymentKeyHashToApi,
                                                   paymentKeyHashToPlutus)
import           GeniusYield.Types.Script         (GYValidatorHash,
                                                   validatorHashFromApi,
                                                   validatorHashToApi,
                                                   validatorHashToPlutus)
import           GeniusYield.Types.StakeKeyHash   (GYStakeKeyHash,
                                                   stakeKeyHashFromApi,
                                                   stakeKeyHashToApi)
import           GeniusYield.Utils                (serialiseToBech32WithPrefix)
import qualified PlutusLedgerApi.V1               as Plutus (Credential (..))
import qualified Text.Printf                      as Printf

-- | Payment credential.
data GYPaymentCredential
       = GYPaymentCredentialByKey !GYPaymentKeyHash
       | GYPaymentCredentialByScript !GYValidatorHash
    deriving (Show, Eq, Ord)

instance Printf.PrintfArg GYPaymentCredential where
  formatArg (GYPaymentCredentialByKey pkh) = Printf.formatArg $ "Payment key credential: " <> Api.serialiseToRawBytesHexText (paymentKeyHashToApi pkh)
  formatArg (GYPaymentCredentialByScript sh) = Printf.formatArg $ "Payment script credential: " <> Api.serialiseToRawBytesHexText (validatorHashToApi sh)

instance Hashable GYPaymentCredential where
    hashWithSalt salt cred = hashWithSalt salt $ paymentCredentialToHexText cred

-- | Convert @GY@ type to corresponding type in @cardano-node@ library.
paymentCredentialToApi :: GYPaymentCredential -> Api.PaymentCredential
paymentCredentialToApi (GYPaymentCredentialByKey pkh) = Api.PaymentCredentialByKey (paymentKeyHashToApi pkh)
paymentCredentialToApi (GYPaymentCredentialByScript sh) = Api.PaymentCredentialByScript (validatorHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-node@ library.
paymentCredentialFromApi :: Api.PaymentCredential -> GYPaymentCredential
paymentCredentialFromApi (Api.PaymentCredentialByKey pkh) = GYPaymentCredentialByKey (paymentKeyHashFromApi pkh)
paymentCredentialFromApi (Api.PaymentCredentialByScript sh) = GYPaymentCredentialByScript (validatorHashFromApi sh)

-- | Convert @GY@ type to corresponding type in @plutus@ library.
paymentCredentialToPlutus :: GYPaymentCredential -> Plutus.Credential
paymentCredentialToPlutus (GYPaymentCredentialByKey pkh) = Plutus.PubKeyCredential (paymentKeyHashToPlutus pkh)
paymentCredentialToPlutus (GYPaymentCredentialByScript sh) = Plutus.ScriptCredential (validatorHashToPlutus sh)

-- | Get hexadecimal value of payment credential.
paymentCredentialToHexText :: GYPaymentCredential -> Text
paymentCredentialToHexText =
  \case
    GYPaymentCredentialByKey pkh -> Api.serialiseToRawBytesHexText (paymentKeyHashToApi pkh)
    GYPaymentCredentialByScript sh -> Api.serialiseToRawBytesHexText (validatorHashToApi sh)

-- | Get the bech32 encoding for the given credential.
paymentCredentialToBech32 :: GYPaymentCredential -> Text
paymentCredentialToBech32 (GYPaymentCredentialByKey pkh) = serialiseToBech32WithPrefix "addr_vkh" $ paymentKeyHashToApi pkh
paymentCredentialToBech32 (GYPaymentCredentialByScript sh) = serialiseToBech32WithPrefix "addr_shared_vkh" $ validatorHashToApi sh

-- | Stake credential.
data GYStakeCredential
       = GYStakeCredentialByKey !GYStakeKeyHash
       | GYStakeCredentialByScript !GYValidatorHash
    deriving (Show, Eq, Ord)

instance Printf.PrintfArg GYStakeCredential where
  formatArg (GYStakeCredentialByKey skh) = Printf.formatArg $ "Stake key credential: " <> Api.serialiseToRawBytesHexText (stakeKeyHashToApi skh)
  formatArg (GYStakeCredentialByScript sh) = Printf.formatArg $ "Stake script credential: " <> Api.serialiseToRawBytesHexText (validatorHashToApi sh)

-- | Convert @GY@ type to corresponding type in @cardano-api@ library.
stakeCredentialToApi :: GYStakeCredential -> Api.StakeCredential
stakeCredentialToApi (GYStakeCredentialByKey skh) = Api.StakeCredentialByKey (stakeKeyHashToApi skh)
stakeCredentialToApi (GYStakeCredentialByScript sh) = Api.StakeCredentialByScript (validatorHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-api@ library.
stakeCredentialFromApi :: Api.StakeCredential -> GYStakeCredential
stakeCredentialFromApi (Api.StakeCredentialByKey skh) = GYStakeCredentialByKey (stakeKeyHashFromApi skh)
stakeCredentialFromApi (Api.StakeCredentialByScript sh) = GYStakeCredentialByScript (validatorHashFromApi sh)

-- | Get hexadecimal value of stake credential.
stakeCredentialToHexText :: GYStakeCredential -> Text
stakeCredentialToHexText =
  \case
    GYStakeCredentialByKey skh -> Api.serialiseToRawBytesHexText (stakeKeyHashToApi skh)
    GYStakeCredentialByScript sh -> Api.serialiseToRawBytesHexText (validatorHashToApi sh)
