module GeniusYield.Types.ProtocolParameters (
    GYProtocolParameters,
    protocolParametersFromApi,
    protocolParametersToApi,
  ) where

import qualified Cardano.Api.Ledger  as Api.L
import qualified Cardano.Api.Shelley as Api.S
import           GeniusYield.Imports (coerce)

type ApiProtocolParameters = Api.L.PParams (Api.S.ShelleyLedgerEra Api.S.ConwayEra)

newtype GYProtocolParameters = GYProtocolParameters ApiProtocolParameters
  deriving newtype (Eq, Ord, Show)

protocolParametersFromApi :: ApiProtocolParameters -> GYProtocolParameters
protocolParametersFromApi = coerce

protocolParametersToApi :: GYProtocolParameters -> ApiProtocolParameters
protocolParametersToApi = coerce
