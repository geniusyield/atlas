{- |
Module      : GeniusYield.Types.Certificate
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Certificate (
  GYCertificatePreBuild (..),
  GYCertificate (..),
  finaliseCert,
  certificateToApi,
  certificateFromApiMaybe,
  certificateFromApiExperimentalMaybe,
  certificateToStakeCredential,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Experimental.Certificate qualified as Api.Cert
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Conway.Core qualified as Ledger
import Cardano.Ledger.Conway.TxCert qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Control.Lens ((^.))
import GHC.Natural (Natural)
import GeniusYield.Imports ((&))
import GeniusYield.Types.Anchor
import GeniusYield.Types.Credential (
  GYCredential (GYCredentialByKey),
  GYStakeCredential,
  credentialFromLedger,
  credentialToLedger,
  stakeCredentialFromLedger,
 )
import GeniusYield.Types.Delegatee (
  GYDelegatee,
  delegateeFromLedger,
  delegateeToLedger,
 )
import GeniusYield.Types.Epoch (GYEpochNo, epochNoFromLedger, epochNoToLedger)
import GeniusYield.Types.Era
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole
import GeniusYield.Types.Pool (GYPoolParams (..), poolParamsFromLedger, poolParamsToLedger)
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)

-- | Certificate state before building the transaction.
data GYCertificatePreBuild
  = GYStakeAddressRegistrationCertificatePB !GYStakeCredential
  | GYStakeAddressDeregistrationCertificatePB !GYStakeCredential
  | GYStakeAddressDelegationCertificatePB !GYStakeCredential !GYDelegatee
  | GYStakeAddressRegistrationDelegationCertificatePB !GYStakeCredential !GYDelegatee
  | GYDRepRegistrationCertificatePB !(GYCredential 'GYKeyRoleDRep) !(Maybe GYAnchor)
  | GYDRepUpdateCertificatePB !(GYCredential 'GYKeyRoleDRep) !(Maybe GYAnchor)
  | GYDRepUnregistrationCertificatePB !(GYCredential 'GYKeyRoleDRep) !Natural
  | GYStakePoolRegistrationCertificatePB !GYPoolParams
  | GYStakePoolRetirementCertificatePB !(GYKeyHash 'GYKeyRoleStakePool) !GYEpochNo
  | GYCommitteeHotKeyAuthCertificatePB !(GYCredential 'GYKeyRoleColdCommittee) !(GYCredential 'GYKeyRoleHotCommittee)
  | GYCommitteeColdKeyResignationCertificatePB !(GYCredential 'GYKeyRoleColdCommittee) !(Maybe GYAnchor)
  deriving stock (Eq, Ord, Show)

-- | Certificate state after populating missing entries from `GYCertificatePreBuild`.
data GYCertificate
  = GYStakeAddressRegistrationCertificate !Natural !GYStakeCredential
  | GYStakeAddressDeregistrationCertificate !Natural !GYStakeCredential
  | GYStakeAddressDelegationCertificate !GYStakeCredential !GYDelegatee
  | GYStakeAddressRegistrationDelegationCertificate !Natural !GYStakeCredential !GYDelegatee
  | GYDRepRegistrationCertificate !Natural !(GYCredential 'GYKeyRoleDRep) !(Maybe GYAnchor)
  | GYDRepUpdateCertificate !(GYCredential 'GYKeyRoleDRep) !(Maybe GYAnchor)
  | GYDRepUnregistrationCertificate !(GYCredential 'GYKeyRoleDRep) !Natural
  | GYStakePoolRegistrationCertificate !GYPoolParams
  | GYStakePoolRetirementCertificate !(GYKeyHash 'GYKeyRoleStakePool) !GYEpochNo
  | GYCommitteeHotKeyAuthCertificate !(GYCredential 'GYKeyRoleColdCommittee) !(GYCredential 'GYKeyRoleHotCommittee)
  | GYCommitteeColdKeyResignationCertificate !(GYCredential 'GYKeyRoleColdCommittee) !(Maybe GYAnchor)
  deriving stock (Eq, Ord, Show)

-- FIXME: Stake address unregistration should make use of deposit that was actually used when registering earlier.
finaliseCert :: ApiProtocolParameters -> GYCertificatePreBuild -> GYCertificate
finaliseCert pp = \case
  GYStakeAddressRegistrationCertificatePB sc -> GYStakeAddressRegistrationCertificate ppDep' sc
  GYStakeAddressDeregistrationCertificatePB sc -> GYStakeAddressDeregistrationCertificate ppDep' sc
  GYStakeAddressDelegationCertificatePB sc del -> GYStakeAddressDelegationCertificate sc del
  GYStakeAddressRegistrationDelegationCertificatePB sc del -> GYStakeAddressRegistrationDelegationCertificate ppDep' sc del
  GYDRepRegistrationCertificatePB cred manchor -> GYDRepRegistrationCertificate ppDRepDeposit' cred manchor
  GYDRepUpdateCertificatePB cred manchor -> GYDRepUpdateCertificate cred manchor
  GYDRepUnregistrationCertificatePB cred dep -> GYDRepUnregistrationCertificate cred dep
  GYStakePoolRegistrationCertificatePB poolParams -> GYStakePoolRegistrationCertificate poolParams
  GYStakePoolRetirementCertificatePB poolId epoch -> GYStakePoolRetirementCertificate poolId epoch
  GYCommitteeHotKeyAuthCertificatePB cold hot -> GYCommitteeHotKeyAuthCertificate cold hot
  GYCommitteeColdKeyResignationCertificatePB cold manchor -> GYCommitteeColdKeyResignationCertificate cold manchor
 where
  Ledger.Coin ppDep = pp ^. Ledger.ppKeyDepositL
  ppDep' :: Natural = fromIntegral ppDep
  Ledger.Coin ppDRepDeposit = pp ^. Ledger.ppDRepDepositL
  ppDRepDeposit' :: Natural = fromIntegral ppDRepDeposit

certificateToApi :: GYCertificate -> Api.Cert.Certificate (Api.ShelleyLedgerEra ApiEra)
certificateToApi = \case
  GYStakeAddressRegistrationCertificate dep sc ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertDeleg $
        Ledger.ConwayRegCert (credentialToLedger sc) (Ledger.SJust $ Ledger.Coin $ fromIntegral dep)
  GYStakeAddressDeregistrationCertificate ref sc ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertDeleg $
        Ledger.ConwayUnRegCert (credentialToLedger sc) (Ledger.SJust $ Ledger.Coin $ fromIntegral ref)
  GYStakeAddressDelegationCertificate sc del ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertDeleg $
        Ledger.ConwayDelegCert (credentialToLedger sc) (delegateeToLedger del)
  GYStakeAddressRegistrationDelegationCertificate dep sc del ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertDeleg $
        Ledger.ConwayRegDelegCert (credentialToLedger sc) (delegateeToLedger del) (Ledger.Coin $ fromIntegral dep)
  GYDRepRegistrationCertificate dep cred manchor ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertGov $
        Ledger.ConwayRegDRep (credentialToLedger cred) (Ledger.Coin $ fromIntegral dep) (Ledger.maybeToStrictMaybe $ anchorToLedger <$> manchor)
  GYDRepUpdateCertificate cred manchor ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertGov $
        Ledger.ConwayUpdateDRep (credentialToLedger cred) (Ledger.maybeToStrictMaybe $ anchorToLedger <$> manchor)
  GYDRepUnregistrationCertificate cred refund ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertGov $
        Ledger.ConwayUnRegDRep (credentialToLedger cred) (Ledger.Coin $ fromIntegral refund)
  GYStakePoolRegistrationCertificate poolParams ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertPool $
        Ledger.RegPool (poolParamsToLedger poolParams)
  GYStakePoolRetirementCertificate poolId epoch ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertPool $
        Ledger.RetirePool (keyHashToLedger poolId) (epochNoToLedger epoch)
  GYCommitteeHotKeyAuthCertificate cold hot ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertGov $
        Ledger.ConwayAuthCommitteeHotKey (credentialToLedger cold) (credentialToLedger hot)
  GYCommitteeColdKeyResignationCertificate cold manchor ->
    Api.Cert.Certificate $
      Ledger.ConwayTxCertGov $
        Ledger.ConwayResignCommitteeColdKey (credentialToLedger cold) (Ledger.maybeToStrictMaybe $ anchorToLedger <$> manchor)

certificateFromApiMaybe :: Api.Cert.Certificate (Api.ShelleyLedgerEra ApiEra) -> Maybe GYCertificate
certificateFromApiMaybe = certificateFromApiExperimentalMaybe

certificateFromApiExperimentalMaybe :: Api.Cert.Certificate (Api.ShelleyLedgerEra ApiEra) -> Maybe GYCertificate
certificateFromApiExperimentalMaybe (Api.Cert.Certificate x) = certificateFromConwayLedgerMaybe x

certificateFromConwayLedgerMaybe :: Ledger.ConwayTxCert (Api.ShelleyLedgerEra ApiEra) -> Maybe GYCertificate
certificateFromConwayLedgerMaybe = \case
  Ledger.ConwayTxCertDeleg delCert -> case delCert of
    Ledger.ConwayRegCert sc (Ledger.SJust dep) -> Just $ GYStakeAddressRegistrationCertificate (fromIntegral dep) (f sc)
    Ledger.ConwayRegCert _ Ledger.SNothing -> Nothing
    Ledger.ConwayUnRegCert sc (Ledger.SJust ref) -> Just $ GYStakeAddressDeregistrationCertificate (fromIntegral ref) (f sc)
    Ledger.ConwayUnRegCert _ Ledger.SNothing -> Nothing
    Ledger.ConwayDelegCert sc del -> Just $ GYStakeAddressDelegationCertificate (f sc) (g del)
    Ledger.ConwayRegDelegCert sc del dep -> Just $ GYStakeAddressRegistrationDelegationCertificate (fromIntegral dep) (f sc) (g del)
  Ledger.ConwayTxCertGov govCert -> case govCert of
    Ledger.ConwayRegDRep cred dep manchor -> Just $ GYDRepRegistrationCertificate (fromIntegral dep) (credentialFromLedger cred) (Ledger.strictMaybeToMaybe (anchorFromLedger <$> manchor))
    Ledger.ConwayUpdateDRep cred manchor -> Just $ GYDRepUpdateCertificate (credentialFromLedger cred) (Ledger.strictMaybeToMaybe (anchorFromLedger <$> manchor))
    Ledger.ConwayUnRegDRep cred refund -> Just $ GYDRepUnregistrationCertificate (credentialFromLedger cred) (fromIntegral refund)
    Ledger.ConwayAuthCommitteeHotKey cold hot -> Just $ GYCommitteeHotKeyAuthCertificate (credentialFromLedger cold) (credentialFromLedger hot)
    Ledger.ConwayResignCommitteeColdKey cold manchor -> Just $ GYCommitteeColdKeyResignationCertificate (credentialFromLedger cold) (Ledger.strictMaybeToMaybe (anchorFromLedger <$> manchor))
  Ledger.ConwayTxCertPool poolCert -> case poolCert of
    Ledger.RegPool poolParams -> Just $ GYStakePoolRegistrationCertificate (poolParamsFromLedger poolParams)
    Ledger.RetirePool poolId epoch -> Just $ GYStakePoolRetirementCertificate (keyHashFromLedger poolId) (epochNoFromLedger epoch)
 where
  f = stakeCredentialFromLedger
  g = delegateeFromLedger

-- | This casts relevant credentials to stake credentials as that's how cardano-api treats these under the hood, which is nonetheless ugly.
certificateToStakeCredential :: GYCertificate -> GYStakeCredential
certificateToStakeCredential = \case
  GYStakeAddressRegistrationCertificate _ sc -> sc
  GYStakeAddressDeregistrationCertificate _ sc -> sc
  GYStakeAddressDelegationCertificate sc _ -> sc
  GYStakeAddressRegistrationDelegationCertificate _ sc _ -> sc
  GYDRepRegistrationCertificate _ cred _ -> castCred cred
  GYDRepUpdateCertificate cred _ -> castCred cred
  GYDRepUnregistrationCertificate cred _ -> castCred cred
  GYStakePoolRegistrationCertificate GYPoolParams {poolId} -> castCred $ GYCredentialByKey poolId
  GYStakePoolRetirementCertificate poolId _ -> castCred $ GYCredentialByKey poolId
  GYCommitteeHotKeyAuthCertificate cold _ -> castCred cold
  GYCommitteeColdKeyResignationCertificate cold _ -> castCred cold
 where
  castCred cred = credentialToLedger cred & Ledger.coerceKeyRole & credentialFromLedger
