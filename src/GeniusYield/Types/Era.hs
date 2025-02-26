{- |
Module      : GeniusYield.Types.Era
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Era (
  ApiEra,
  apiSBE,
) where

import Cardano.Api.Shelley qualified as Api.S

-- TODO: Make this module internal.
type ApiEra = Api.S.ConwayEra
apiSBE :: Api.S.ShelleyBasedEra Api.S.ConwayEra
apiSBE = Api.S.ShelleyBasedEraConway
