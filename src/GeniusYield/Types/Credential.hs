{-|
Module      : GeniusYield.Types.Credential
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Credential (
    GYPaymentCredential
  , paymentCredentialToApi
  , paymentCredentialFromApi
  ) where


import qualified Cardano.Api                  as Api
import           GeniusYield.Types.PubKeyHash (GYPubKeyHash, pubKeyHashFromApi,
                                               pubKeyHashToApi)
import           GeniusYield.Types.Script     (GYValidatorHash,
                                               validatorHashFromApi,
                                               validatorHashToApi)

-- | Payment credential.
data GYPaymentCredential
       = GYPaymentCredentialByKey !GYPubKeyHash
       | GYPaymentCredentialByScript !GYValidatorHash
    deriving (Show, Eq, Ord)

-- | Convert @GY@ type to corresponding type in @cardano-node@ library.
paymentCredentialToApi :: GYPaymentCredential -> Api.PaymentCredential
paymentCredentialToApi (GYPaymentCredentialByKey pkh) = Api.PaymentCredentialByKey (pubKeyHashToApi pkh)
paymentCredentialToApi (GYPaymentCredentialByScript sh) = Api.PaymentCredentialByScript (validatorHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-node@ library.
paymentCredentialFromApi :: Api.PaymentCredential -> GYPaymentCredential
paymentCredentialFromApi (Api.PaymentCredentialByKey pkh) = GYPaymentCredentialByKey (pubKeyHashFromApi pkh)
paymentCredentialFromApi (Api.PaymentCredentialByScript sh) = GYPaymentCredentialByScript (validatorHashFromApi sh)
