{- |
Module      : GeniusYield.Types.Era
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Era (
  ApiEra,
  ApiLedgerEra,
  apiAlonzoEraOnwards,
  apiAllegraEraOnwards,
  apiAnyShelleyBasedEra,
  apiAsType,
  apiBabbageEraOnwards,
  apiCardanoEra,
  apiConwayEraOnwards,
  apiMaryEraOnwards,
  apiPlutusScriptV1InEra,
  apiPlutusScriptV2InEra,
  apiPlutusScriptV3InEra,
  apiSimpleScriptInEra,
  apiSBE,
) where

import Cardano.Api qualified as Api.S

-- TODO: Make this module internal.
type ApiEra = Api.S.ConwayEra

type ApiLedgerEra = Api.S.ShelleyLedgerEra ApiEra

apiAsType :: Api.S.AsType ApiEra
apiAsType = Api.S.AsConwayEra

apiCardanoEra :: Api.S.CardanoEra ApiEra
apiCardanoEra = Api.S.ConwayEra

apiSBE :: Api.S.ShelleyBasedEra ApiEra
apiSBE = Api.S.ShelleyBasedEraConway

apiAnyShelleyBasedEra :: Api.S.AnyShelleyBasedEra
apiAnyShelleyBasedEra = Api.S.AnyShelleyBasedEra apiSBE

apiMaryEraOnwards :: Api.S.MaryEraOnwards ApiEra
apiMaryEraOnwards = Api.S.MaryEraOnwardsConway

apiAllegraEraOnwards :: Api.S.AllegraEraOnwards ApiEra
apiAllegraEraOnwards = Api.S.AllegraEraOnwardsConway

apiAlonzoEraOnwards :: Api.S.AlonzoEraOnwards ApiEra
apiAlonzoEraOnwards = Api.S.AlonzoEraOnwardsConway

apiBabbageEraOnwards :: Api.S.BabbageEraOnwards ApiEra
apiBabbageEraOnwards = Api.S.BabbageEraOnwardsConway

apiConwayEraOnwards :: Api.S.ConwayEraOnwards ApiEra
apiConwayEraOnwards = Api.S.ConwayEraOnwardsConway

apiSimpleScriptInEra :: Api.S.ScriptLanguageInEra Api.S.SimpleScript' ApiEra
apiSimpleScriptInEra = Api.S.SimpleScriptInConway

apiPlutusScriptV1InEra :: Api.S.ScriptLanguageInEra Api.S.PlutusScriptV1 ApiEra
apiPlutusScriptV1InEra = Api.S.PlutusScriptV1InConway

apiPlutusScriptV2InEra :: Api.S.ScriptLanguageInEra Api.S.PlutusScriptV2 ApiEra
apiPlutusScriptV2InEra = Api.S.PlutusScriptV2InConway

apiPlutusScriptV3InEra :: Api.S.ScriptLanguageInEra Api.S.PlutusScriptV3 ApiEra
apiPlutusScriptV3InEra = Api.S.PlutusScriptV3InConway
