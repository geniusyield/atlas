{-|
Module      : GeniusYield.Types.TxCert
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.TxCert (
    GYTxCert,
    GYTxCertWitness (..),
    txCertToApi,
    mkStakeAddressRegistrationCertificate,
    mkStakeAddressDeregistrationCertificate,
    mkStakeAddressDelegationCertificate,
) where

import           GeniusYield.Types.Certificate
import           GeniusYield.Types.Credential      (GYStakeCredential)
import           GeniusYield.Types.Delegatee       (GYDelegatee)
import           GeniusYield.Types.TxCert.Internal

mkStakeAddressRegistrationCertificate :: GYStakeCredential -> GYTxCert v
mkStakeAddressRegistrationCertificate sc = GYTxCert (GYStakeAddressRegistrationCertificatePB sc) Nothing

{-| Note that deregistration certificate requires following preconditions:

1. The stake address must be registered.

2. The corresponding rewards balance is zero.
-}
mkStakeAddressDeregistrationCertificate :: GYStakeCredential -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressDeregistrationCertificate sc wit = GYTxCert (GYStakeAddressDeregistrationCertificatePB sc) (Just wit)

mkStakeAddressDelegationCertificate :: GYStakeCredential -> GYDelegatee -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressDelegationCertificate sc del wit = GYTxCert (GYStakeAddressDelegationCertificatePB sc del) (Just wit)
