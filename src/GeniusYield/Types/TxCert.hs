{- |
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
  mkDRepRegisterationCertificate,
  mkDRepUpdateCertificate,
) where

import GeniusYield.Types.Anchor (GYAnchor)
import GeniusYield.Types.Certificate
import GeniusYield.Types.Credential (GYCredential, GYStakeCredential)
import GeniusYield.Types.Delegatee (GYDelegatee)
import GeniusYield.Types.KeyRole (GYKeyRole (..))
import GeniusYield.Types.TxCert.Internal

-- | Post conway, newer stake address registration certificate also require a witness.
mkStakeAddressRegistrationCertificate :: GYStakeCredential -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressRegistrationCertificate sc wit = GYTxCert (GYStakeAddressRegistrationCertificatePB sc) (Just wit)

{- | Note that deregistration certificate requires following preconditions:

1. The stake address must be registered.

2. The corresponding rewards balance is zero.
-}
mkStakeAddressDeregistrationCertificate :: GYStakeCredential -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressDeregistrationCertificate sc wit = GYTxCert (GYStakeAddressDeregistrationCertificatePB sc) (Just wit)

mkStakeAddressDelegationCertificate :: GYStakeCredential -> GYDelegatee -> GYTxCertWitness v -> GYTxCert v
mkStakeAddressDelegationCertificate sc del wit = GYTxCert (GYStakeAddressDelegationCertificatePB sc del) (Just wit)

{- | Note that delegation certificate requires following preconditions:

1. DRep must not already be registered.

2. Deposit amount should be that given by corresponding protocol parameter.

3. Signature from the corresponding DRep key.
-}
mkDRepRegisterationCertificate :: GYCredential 'GYKeyRoleDRep -> Maybe GYAnchor -> GYTxCertWitness v -> GYTxCert v
mkDRepRegisterationCertificate cred anchor wit = GYTxCert (GYDRepRegistrationCertificatePB cred anchor) (Just wit)

{- | Note that update certificate requires following preconditions:

1. DRep must already be registered.

2. Signature from the corresponding DRep key.
-}
mkDRepUpdateCertificate :: GYCredential 'GYKeyRoleDRep -> Maybe GYAnchor -> GYTxCertWitness v -> GYTxCert v
mkDRepUpdateCertificate cred anchor wit = GYTxCert (GYDRepUpdateCertificatePB cred anchor) (Just wit)
