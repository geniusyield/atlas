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
import qualified Cardano.Api.Address           as Api
import qualified Cardano.Api.Keys.Shelley      as Api.S
import qualified Cardano.Api.ReexposeLedger    as Ledger
import           GeniusYield.Types.Credential  (GYStakeCredential,
                                                stakeCredentialFromApi,
                                                stakeCredentialToApi)
import           GeniusYield.Types.StakePoolId

data GYCertificate =
    GYStakeAddressRegistrationCertificate !GYStakeCredential
  | GYStakeAddressDeregistrationCertificate !GYStakeCredential
  | GYStakeAddressPoolDelegationCertificate !GYStakeCredential !GYStakePoolId
  deriving stock (Eq, Show)

certificateToApi :: GYCertificate -> (Api.Certificate Api.BabbageEra)
certificateToApi = \case
  GYStakeAddressRegistrationCertificate sc -> Api.makeStakeAddressRegistrationCertificate
    . Api.StakeAddrRegistrationPreConway Api.ShelleyToBabbageEraBabbage $ f sc
  GYStakeAddressDeregistrationCertificate sc -> Api.makeStakeAddressUnregistrationCertificate
    . Api.StakeAddrRegistrationPreConway Api.ShelleyToBabbageEraBabbage $ f sc
  GYStakeAddressPoolDelegationCertificate sc spId -> Api.makeStakeAddressDelegationCertificate
    . Api.StakeDelegationRequirementsPreConway Api.ShelleyToBabbageEraBabbage (f sc) $ g spId
  where
    f = stakeCredentialToApi
    g = stakePoolIdToApi

certificateFromApiMaybe :: (Api.Certificate Api.BabbageEra) -> Maybe GYCertificate
certificateFromApiMaybe (Api.ShelleyRelatedCertificate _ x) = case x of
  Ledger.RegTxCert (Api.fromShelleyStakeCredential -> sc) -> Just $ GYStakeAddressRegistrationCertificate (f sc)
  Ledger.UnRegTxCert (Api.fromShelleyStakeCredential -> sc) -> Just $ GYStakeAddressDeregistrationCertificate (f sc)
  Ledger.DelegStakeTxCert (Api.fromShelleyStakeCredential -> sc) (Api.S.StakePoolKeyHash -> spId) -> Just $ GYStakeAddressPoolDelegationCertificate (f sc) (g spId)
  _ -> Nothing
  where
    f = stakeCredentialFromApi
    g = stakePoolIdFromApi
-- TODO: Conway support.
certificateFromApiMaybe _ = Nothing

certificateToStakeCredential :: GYCertificate -> GYStakeCredential
certificateToStakeCredential = \case
  GYStakeAddressRegistrationCertificate sc -> sc
  GYStakeAddressDeregistrationCertificate sc -> sc
  GYStakeAddressPoolDelegationCertificate sc _ -> sc
