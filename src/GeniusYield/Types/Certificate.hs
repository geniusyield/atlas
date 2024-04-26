{-|
Module      : GeniusYield.Types.Certificate
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Certificate (
  GYCertificate (..),
  certificateToApi,
  certificateFromApiMaybe,
  certificateToStakeCredential,
) where

import qualified Cardano.Api                   as Api
import           GeniusYield.Types.Credential  (GYStakeCredential,
                                                stakeCredentialFromApi,
                                                stakeCredentialToApi)
import           GeniusYield.Types.StakePoolId

data GYCertificate =
    GYStakeAddressRegistrationCertificate !GYStakeCredential
  | GYStakeAddressDeregistrationCertificate !GYStakeCredential
  | GYStakeAddressPoolDelegationCertificate !GYStakeCredential !GYStakePoolId
  deriving stock (Eq, Show)

certificateToApi :: GYCertificate -> Api.Certificate
certificateToApi = \case
  GYStakeAddressRegistrationCertificate sc -> Api.StakeAddressRegistrationCertificate (f sc)
  GYStakeAddressDeregistrationCertificate sc -> Api.StakeAddressDeregistrationCertificate (f sc)
  GYStakeAddressPoolDelegationCertificate sc spId -> Api.StakeAddressPoolDelegationCertificate (f sc) (g spId)
  where
    f = stakeCredentialToApi
    g = stakePoolIdToApi

certificateFromApiMaybe :: Api.Certificate -> Maybe GYCertificate
certificateFromApiMaybe = \case
  Api.StakeAddressRegistrationCertificate sc -> Just $ GYStakeAddressRegistrationCertificate (f sc)
  Api.StakeAddressDeregistrationCertificate sc -> Just $ GYStakeAddressDeregistrationCertificate (f sc)
  Api.StakeAddressPoolDelegationCertificate sc spId -> Just $ GYStakeAddressPoolDelegationCertificate (f sc) (g spId)
  _ -> Nothing
  where
    f = stakeCredentialFromApi
    g = stakePoolIdFromApi

certificateToStakeCredential :: GYCertificate -> GYStakeCredential
certificateToStakeCredential = \case
  GYStakeAddressRegistrationCertificate sc -> sc
  GYStakeAddressDeregistrationCertificate sc -> sc
  GYStakeAddressPoolDelegationCertificate sc _ -> sc
