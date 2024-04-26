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
    mkStakeAddressPoolDelegationCertificate,
) where

import           GeniusYield.Types.Certificate
import           GeniusYield.Types.Credential      (GYStakeCredential)
import           GeniusYield.Types.StakePoolId
import           GeniusYield.Types.TxCert.Internal

mkStakeAddressRegistrationCertificate :: GYStakeCredential -> GYTxCert v
mkStakeAddressRegistrationCertificate sc = GYTxCert (GYStakeAddressRegistrationCertificate sc) Nothing

mkStakeAddressDeregistrationCertificate :: GYStakeCredential -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressDeregistrationCertificate sc wit = GYTxCert (GYStakeAddressDeregistrationCertificate sc) (Just wit)

mkStakeAddressPoolDelegationCertificate :: GYStakeCredential -> GYStakePoolId -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressPoolDelegationCertificate sc spId wit = GYTxCert (GYStakeAddressPoolDelegationCertificate sc spId) (Just wit)
