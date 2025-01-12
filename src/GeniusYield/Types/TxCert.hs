{- |
Module      : GeniusYield.Types.TxCert
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxCert (
  GYTxCert,
  GYTxCertWitness,
  pattern GYTxCertWitnessKey,
  pattern GYTxCertWitnessScript,
  txCertToApi,
  mkStakeAddressRegistrationCertificate,
  mkStakeAddressDeregistrationCertificate,
  mkStakeAddressDelegationCertificate,
  mkDRepRegistrationCertificate,
  mkDRepUpdateCertificate,
  mkDRepUnregistrationCertificate,
  mkStakePoolRegistrationCertificate,
  mkStakePoolRetirementCertificate,
  mkCommitteeHotKeyAuthCertificate,
  mkCommitteeColdKeyResignationCertificate,
) where

import GeniusYield.Imports (Natural)
import GeniusYield.Types.Anchor (GYAnchor)
import GeniusYield.Types.Certificate
import GeniusYield.Types.Credential (GYCredential, GYStakeCredential)
import GeniusYield.Types.Delegatee (GYDelegatee)
import GeniusYield.Types.Epoch
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole (GYKeyRole (..))
import GeniusYield.Types.Pool
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
mkDRepRegistrationCertificate :: GYCredential 'GYKeyRoleDRep -> Maybe GYAnchor -> GYTxCertWitness v -> GYTxCert v
mkDRepRegistrationCertificate cred anchor wit = GYTxCert (GYDRepRegistrationCertificatePB cred anchor) (Just wit)

{- | Note that update certificate requires following preconditions:

1. DRep must already be registered.

2. Signature from the corresponding DRep key.
-}
mkDRepUpdateCertificate :: GYCredential 'GYKeyRoleDRep -> Maybe GYAnchor -> GYTxCertWitness v -> GYTxCert v
mkDRepUpdateCertificate cred anchor wit = GYTxCert (GYDRepUpdateCertificatePB cred anchor) (Just wit)

{- | Note that unregistration certificate requires following preconditions:

1. DRep must already be registered.

2. Refund amount should be same as the deposit made by DRep while registration.

3. Signature from the corresponding DRep key.
-}
mkDRepUnregistrationCertificate :: GYCredential 'GYKeyRoleDRep -> Natural -> GYTxCertWitness v -> GYTxCert v
mkDRepUnregistrationCertificate cred refund wit = GYTxCert (GYDRepUnregistrationCertificatePB cred refund) (Just wit)

{- | Note that stake pool registration certificate requires following preconditions:

1. @poolCost@ must be more than minimum pool cost specified in protocol parameters.

2. Signature from the key corresponding to @poolId@.

3. If registering for the first time, then deposit is also deducted to that given by corresponding protocol parameter (ppPoolDepositL).

4. Signature from pool owners.
-}
mkStakePoolRegistrationCertificate ::
  GYPoolParams ->
  GYTxCert v
mkStakePoolRegistrationCertificate pp = GYTxCert (GYStakePoolRegistrationCertificatePB pp) (Just GYTxCertWitnessKey)

{- | Note that stake pool retirement certificate requires following preconditions:

1. Signature from the key corresponding to @poolId@.

2. Epoch must be greater than the current epoch and less than or equal to ppEMax after the current epoch.

3. The pool must be registered.

Note that deposit made earlier is returned at epoch transition.
-}
mkStakePoolRetirementCertificate :: GYKeyHash 'GYKeyRoleStakePool -> GYEpochNo -> GYTxCert v
mkStakePoolRetirementCertificate poolId epoch = GYTxCert (GYStakePoolRetirementCertificatePB poolId epoch) (Just GYTxCertWitnessKey)

{- | Note that committee hot key auth certificate requires following preconditions:

1. Cold key must not have resigned from the committee.

2. Should be part of current committee or future committee as dictated by a governance action.

3. Signature from the corresponding cold committee key.
-}
mkCommitteeHotKeyAuthCertificate :: GYCredential 'GYKeyRoleColdCommittee -> GYCredential 'GYKeyRoleHotCommittee -> GYTxCert v
mkCommitteeHotKeyAuthCertificate cold hot = GYTxCert (GYCommitteeHotKeyAuthCertificatePB cold hot) (Just GYTxCertWitnessKey)

{- | Note that committee cold key resignation certificate requires following preconditions:

1. Cold key must not have resigned from the committee.

2. Should be part of current committee or future committee as dictated by a governance action.

3. Signature from the corresponding cold committee key.
-}
mkCommitteeColdKeyResignationCertificate ::
  GYCredential 'GYKeyRoleColdCommittee ->
  -- | Potential explanation for resignation.
  Maybe GYAnchor ->
  GYTxCert v
mkCommitteeColdKeyResignationCertificate cold anchor = GYTxCert (GYCommitteeColdKeyResignationCertificatePB cold anchor) (Just GYTxCertWitnessKey)
