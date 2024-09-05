module GeniusYield.Types.ProtocolParameters (
  ApiProtocolParameters,
) where

import Cardano.Api.Ledger qualified as Api.L
import Cardano.Api.Shelley qualified as Api.S
import GeniusYield.Types.Era

type ApiProtocolParameters = Api.L.PParams (Api.S.ShelleyLedgerEra ApiEra)
