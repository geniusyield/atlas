{- |
Module      : GeniusYield.Types.BuildWitness
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.BuildWitness (
  GYTxBuildWitness (..),
  buildWitnessToApi,
  unsafeBuildScriptWitnessToApi,
) where

import Cardano.Api qualified as Api
import GeniusYield.Types.BuildScript
import GeniusYield.Types.Era
import GeniusYield.Types.Redeemer

-- | Represents witness type.
data GYTxBuildWitness v
  = -- | Key witness.
    GYTxBuildWitnessKey
  | -- | Script witness with associated script and redeemer.
    GYTxBuildWitnessPlutusScript !(GYBuildPlutusScript v) !GYRedeemer
  | -- | Simple script witness.
    GYTxBuildWitnessSimpleScript !(GYBuildSimpleScript v)
  deriving stock (Eq, Show)

buildWitnessToApi :: GYTxBuildWitness v -> Api.Witness Api.WitCtxStake ApiEra
buildWitnessToApi GYTxBuildWitnessKey = Api.KeyWitness Api.KeyWitnessForStakeAddr
buildWitnessToApi (GYTxBuildWitnessPlutusScript v r) =
  Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
    gyStakeValScriptWitnessToApiPlutusSW
      v
      (redeemerToApi r)
      (Api.ExecutionUnits 0 0)
buildWitnessToApi (GYTxBuildWitnessSimpleScript v) = Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ simpleScriptWitnessToApi v

{- | Convert 'GYTxBuildWitness' to 'Api.ScriptWitness'. Throws an error if the input is 'GYTxBuildWitnessKey'.

Would likely remove depending upon resolution of https://github.com/IntersectMBO/cardano-api/issues/722.
-}
unsafeBuildScriptWitnessToApi :: GYTxBuildWitness v -> Api.ScriptWitness Api.WitCtxStake ApiEra
unsafeBuildScriptWitnessToApi GYTxBuildWitnessKey = error "unsafeBuildScriptWitnessToApi: GYTxBuildWitnessKey"
unsafeBuildScriptWitnessToApi (GYTxBuildWitnessPlutusScript v r) =
  gyStakeValScriptWitnessToApiPlutusSW
    v
    (redeemerToApi r)
    (Api.ExecutionUnits 0 0)
unsafeBuildScriptWitnessToApi (GYTxBuildWitnessSimpleScript v) = simpleScriptWitnessToApi v
