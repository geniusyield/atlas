module GeniusYield.Types.ProtocolParameters (
  ApiProtocolParameters,
) where

import Cardano.Api qualified as Api.S
import Cardano.Api.Ledger qualified as Api.L
import GeniusYield.Types.Era

type ApiProtocolParameters = Api.L.PParams (Api.S.ShelleyLedgerEra ApiEra)
