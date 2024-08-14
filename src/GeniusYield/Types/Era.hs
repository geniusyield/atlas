{-|
Module      : GeniusYield.Types.Era
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Era (
    ApiEra,
    ConwayEra,
) where

import qualified Cardano.Api.Shelley as Api.S

-- TODO: Make this module internal.
type ConwayEra = Api.S.ShelleyLedgerEra Api.S.ConwayEra
type ApiEra = Api.S.ConwayEra
