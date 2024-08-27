module GeniusYield.Types.ProtocolParameters (
    ApiProtocolParameters
  ) where

import qualified Cardano.Api.Ledger    as Api.L
import qualified Cardano.Api.Shelley   as Api.S
import           GeniusYield.Types.Era

type ApiProtocolParameters = Api.L.PParams (Api.S.ShelleyLedgerEra ApiEra)
