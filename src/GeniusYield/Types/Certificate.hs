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
import Control.Lens ((^.))
import GHC.Natural (Natural)
import GeniusYield.Types.Credential (
  GYStakeCredential,
  stakeCredentialFromLedger,
  stakeCredentialToApi,
 )
import GeniusYield.Types.Delegatee (
  GYDelegatee,
  delegateeFromLedger,
  delegateeToLedger,
 )
import GeniusYield.Types.Era
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)

-- | Certificate state before building the transaction.
data GYCertificatePreBuild
  = GYStakeAddressRegistrationCertificatePB !GYStakeCredential
  | GYStakeAddressDeregistrationCertificatePB !GYStakeCredential
  | GYStakeAddressDelegationCertificatePB !GYStakeCredential !GYDelegatee
  | GYStakeAddressRegistrationDelegationCertificatePB !GYStakeCredential !GYDelegatee
  deriving stock (Eq, Ord, Show)

-- | Certificate state after populating missing entries from `GYCertificatePreBuild`.
data GYCertificate
  = GYStakeAddressRegistrationCertificate !Natural !GYStakeCredential
  | GYStakeAddressDeregistrationCertificate !Natural !GYStakeCredential
  | GYStakeAddressDelegationCertificate !GYStakeCredential !GYDelegatee
  | GYStakeAddressRegistrationDelegationCertificate !Natural !GYStakeCredential !GYDelegatee
  deriving stock (Eq, Ord, Show)

-- FIXME: Unregistration should make use of deposit that was actually used when registering earlier.
finaliseCert :: ApiProtocolParameters -> GYCertificatePreBuild -> GYCertificate
finaliseCert pp = \case
  GYStakeAddressRegistrationCertificatePB sc -> GYStakeAddressRegistrationCertificate ppDep' sc
  GYStakeAddressDeregistrationCertificatePB sc -> GYStakeAddressDeregistrationCertificate ppDep' sc
  GYStakeAddressDelegationCertificatePB sc del -> GYStakeAddressDelegationCertificate sc del
  GYStakeAddressRegistrationDelegationCertificatePB sc del -> GYStakeAddressRegistrationDelegationCertificate ppDep' sc del
 where
  Ledger.Coin ppDep = pp ^. Ledger.ppKeyDepositL
  ppDep' :: Natural = fromIntegral ppDep

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
  _ -> Nothing
 where
  f = stakeCredentialFromLedger
  g = delegateeFromLedger
certificateFromApiMaybe _ = Nothing

certificateToStakeCredential :: GYCertificate -> GYStakeCredential
certificateToStakeCredential = \case
  GYStakeAddressRegistrationCertificate _ sc -> sc
  GYStakeAddressDeregistrationCertificate _ sc -> sc
  GYStakeAddressDelegationCertificate sc _ -> sc
  GYStakeAddressRegistrationDelegationCertificate _ sc _ -> sc
