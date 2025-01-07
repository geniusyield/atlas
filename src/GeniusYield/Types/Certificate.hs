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
  certificateToStakeCredential,
) where

import Cardano.Api qualified as Api
import Cardano.Api.ReexposeLedger qualified as Ledger
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Conway.Core qualified as Ledger
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
  stakeCredentialToApi,
 )
import GeniusYield.Types.Delegatee (
  GYDelegatee,
  delegateeFromLedger,
  delegateeToLedger,
 )
import GeniusYield.Types.Era
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
  deriving stock (Eq, Ord, Show)

-- FIXME: Unregistration should make use of deposit that was actually used when registering earlier.
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
 where
  Ledger.Coin ppDep = pp ^. Ledger.ppKeyDepositL
  ppDep' :: Natural = fromIntegral ppDep
  Ledger.Coin ppDRepDeposit = pp ^. Ledger.ppDRepDepositL
  ppDRepDeposit' :: Natural = fromIntegral ppDRepDeposit

certificateToApi :: GYCertificate -> Api.Certificate ApiEra
certificateToApi = \case
  GYStakeAddressRegistrationCertificate dep sc ->
    Api.makeStakeAddressRegistrationCertificate
      . Api.StakeAddrRegistrationConway Api.ConwayEraOnwardsConway (fromIntegral dep)
      $ f sc
  GYStakeAddressDeregistrationCertificate ref sc ->
    Api.makeStakeAddressUnregistrationCertificate
      . Api.StakeAddrRegistrationConway Api.ConwayEraOnwardsConway (fromIntegral ref)
      $ f sc
  GYStakeAddressDelegationCertificate sc del ->
    Api.makeStakeAddressDelegationCertificate $
      Api.StakeDelegationRequirementsConwayOnwards Api.ConwayEraOnwardsConway (f sc) (g del)
  GYStakeAddressRegistrationDelegationCertificate dep sc del -> Api.makeStakeAddressAndDRepDelegationCertificate Api.ConwayEraOnwardsConway (f sc) (g del) (fromIntegral dep)
  GYDRepRegistrationCertificate dep cred manchor -> Api.makeDrepRegistrationCertificate (Api.DRepRegistrationRequirements Api.ConwayEraOnwardsConway (credentialToLedger cred) (fromIntegral dep)) (anchorToLedger <$> manchor)
  GYDRepUpdateCertificate cred manchor -> Api.makeDrepUpdateCertificate (Api.DRepUpdateRequirements Api.ConwayEraOnwardsConway (credentialToLedger cred)) (anchorToLedger <$> manchor)
  GYDRepUnregistrationCertificate cred refund -> Api.makeDrepUnregistrationCertificate (Api.DRepUnregistrationRequirements Api.ConwayEraOnwardsConway (credentialToLedger cred) (fromIntegral refund))
  GYStakePoolRegistrationCertificate poolParams -> Api.makeStakePoolRegistrationCertificate (Api.StakePoolRegistrationRequirementsConwayOnwards Api.ConwayEraOnwardsConway (poolParamsToLedger poolParams))
 where
  f = stakeCredentialToApi
  g = delegateeToLedger

certificateFromApiMaybe :: Api.Certificate ApiEra -> Maybe GYCertificate
certificateFromApiMaybe (Api.ConwayCertificate _ x) = case x of
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
    _anyOther -> Nothing
  Ledger.ConwayTxCertPool poolCert -> case poolCert of
    Ledger.RegPool poolParams -> Just $ GYStakePoolRegistrationCertificate (poolParamsFromLedger poolParams)
    _anyOther -> Nothing
 where
  f = stakeCredentialFromLedger
  g = delegateeFromLedger
certificateFromApiMaybe _ = Nothing

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
 where
  castCred cred = credentialToLedger cred & Ledger.coerceKeyRole & credentialFromLedger
