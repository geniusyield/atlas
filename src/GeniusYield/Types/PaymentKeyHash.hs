{- |
Module      : GeniusYield.Types.PaymentKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.PaymentKeyHash (
  GYPaymentKeyHash,
  paymentKeyHashFromPlutus,
  paymentKeyHashToPlutus,
  paymentKeyHashToApi,
  paymentKeyHashFromApi,
  paymentKeyHashFromLedger,
  paymentKeyHashToLedger,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import GeniusYield.Imports
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import GeniusYield.Types.Ledger
import GeniusYield.Types.PubKeyHash (
  AsPubKeyHash (..),
  pubKeyHashFromPlutus,
  pubKeyHashToPlutus,
 )
import PlutusLedgerApi.V1.Crypto qualified as Plutus

-- | @type GYPaymentKeyHash = GYKeyHash 'GYKeyRolePayment@
type GYPaymentKeyHash = GYKeyHash 'GYKeyRolePayment

paymentKeyHashFromPlutus :: Plutus.PubKeyHash -> Either PlutusToCardanoError GYPaymentKeyHash
paymentKeyHashFromPlutus = fmap fromPubKeyHash . pubKeyHashFromPlutus

paymentKeyHashToPlutus :: GYPaymentKeyHash -> Plutus.PubKeyHash
paymentKeyHashToPlutus = toPubKeyHash >>> pubKeyHashToPlutus

paymentKeyHashToApi :: GYPaymentKeyHash -> Api.Hash Api.PaymentKey
paymentKeyHashToApi = keyHashToApi

paymentKeyHashFromApi :: Api.Hash Api.PaymentKey -> GYPaymentKeyHash
paymentKeyHashFromApi = keyHashFromApi

paymentKeyHashToLedger :: GYPaymentKeyHash -> Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto
paymentKeyHashToLedger = keyHashToLedger

paymentKeyHashFromLedger :: Ledger.KeyHash Ledger.Payment Ledger.StandardCrypto -> GYPaymentKeyHash
paymentKeyHashFromLedger = keyHashFromLedger