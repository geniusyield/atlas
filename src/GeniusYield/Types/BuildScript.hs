{- |
Module      : GeniusYield.Types.BuildScript
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.BuildScript (
  GYBuildScript (..),
  GYBuildPlutusScript (..),
  GYBuildSimpleScript (..),
  buildPlutusScriptVersion,
  simpleScriptWitnessToApi,

  -- * Witness for stake validator (deprecated in favour of 'GYBuildPlutusScript')
  GYStakeValScript,
  pattern GYStakeValScript,
  pattern GYStakeValReference,
  gyStakeValScriptToSerialisedScript,
  gyStakeValScriptWitnessToApiPlutusSW,
  stakeValidatorVersionFromWitness,

  -- * Witness for minting policy (deprecated in favour of 'GYBuildScript')
  GYMintScript,
  pattern GYMintScript,
  pattern GYMintReference,
  gyMintingScriptWitnessToApiPlutusSW,
  mintingPolicyIdFromWitness,
  mintingPolicyApiIdFromWitness,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Internal.Script qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Data.GADT.Compare
import GeniusYield.Imports
import GeniusYield.Types.Era
import GeniusYield.Types.PlutusVersion
import GeniusYield.Types.Script
import GeniusYield.Types.TxOutRef
import PlutusLedgerApi.Common qualified as Plutus

data GYBuildScript (u :: PlutusVersion) where
  GYBuildPlutusScript :: GYBuildPlutusScript u -> GYBuildScript u
  GYBuildSimpleScript :: GYBuildSimpleScript u -> GYBuildScript u

deriving instance Show (GYBuildScript v)

instance Eq (GYBuildScript v) where
  GYBuildPlutusScript script1 == GYBuildPlutusScript script2 = script1 == script2
  GYBuildSimpleScript script1 == GYBuildSimpleScript script2 = script1 == script2
  _ == _ = False

deriving instance Ord (GYBuildScript v)

data GYBuildPlutusScript (u :: PlutusVersion) where
  -- | 'VersionIsGreaterOrEqual' restricts which version validators can be used in this transaction.
  GYBuildPlutusScriptInlined :: forall u v. v `VersionIsGreaterOrEqual` u => GYScript v -> GYBuildPlutusScript u
  GYBuildPlutusScriptReference :: forall u v. v `VersionIsGreaterOrEqual` u => !GYTxOutRef -> !(GYScript v) -> GYBuildPlutusScript u

deriving instance Show (GYBuildPlutusScript v)

instance Eq (GYBuildPlutusScript v) where
  GYBuildPlutusScriptReference ref1 script1 == GYBuildPlutusScriptReference ref2 script2 = ref1 == ref2 && eqScript script1 script2
  GYBuildPlutusScriptInlined v1 == GYBuildPlutusScriptInlined v2 = defaultEq v1 v2
  _ == _ = False

instance Ord (GYBuildPlutusScript v) where
  GYBuildPlutusScriptReference r s `compare` GYBuildPlutusScriptReference r' s' = compare r r' <> compareScript s s'
  GYBuildPlutusScriptReference _ _ `compare` _ = LT
  GYBuildPlutusScriptInlined p `compare` GYBuildPlutusScriptInlined p' = defaultCompare p p'
  GYBuildPlutusScriptInlined _ `compare` _ = GT

-- | Returns the 'PlutusVersion' of the given 'GYBuildPlutusScript'.
buildPlutusScriptVersion :: GYBuildPlutusScript v -> PlutusVersion
buildPlutusScriptVersion (GYBuildPlutusScriptReference _ s) = case scriptVersion s of
  SingPlutusV3 -> PlutusV3
  SingPlutusV2 -> PlutusV2
  SingPlutusV1 -> PlutusV1
buildPlutusScriptVersion (GYBuildPlutusScriptInlined v) = case validatorVersion v of
  SingPlutusV3 -> PlutusV3
  SingPlutusV2 -> PlutusV2
  SingPlutusV1 -> PlutusV1

data GYBuildSimpleScript (u :: PlutusVersion) where
  GYBuildSimpleScriptInlined :: !GYSimpleScript -> GYBuildSimpleScript u
  GYBuildSimpleScriptReference :: !GYTxOutRef -> !GYSimpleScript -> GYBuildSimpleScript v

deriving instance Show (GYBuildSimpleScript v)

instance Eq (GYBuildSimpleScript v) where
  GYBuildSimpleScriptInlined s1 == GYBuildSimpleScriptInlined s2 = s1 == s2
  GYBuildSimpleScriptReference ref1 s1 == GYBuildSimpleScriptReference ref2 s2 = ref1 == ref2 && s1 == s2
  _ == _ = False

instance Ord (GYBuildSimpleScript v) where
  GYBuildSimpleScriptReference r s `compare` GYBuildSimpleScriptReference r' s' = compare r r' <> compare s s'
  GYBuildSimpleScriptReference _ _ `compare` _ = LT
  GYBuildSimpleScriptInlined p `compare` GYBuildSimpleScriptInlined p' = compare p p'
  GYBuildSimpleScriptInlined _ `compare` _ = GT

simpleScriptWitnessToApi :: GYBuildSimpleScript u -> Api.S.ScriptWitness witctx Api.S.ConwayEra
simpleScriptWitnessToApi = Api.SimpleScriptWitness Api.SimpleScriptInConway . h
 where
  h :: GYBuildSimpleScript u -> Api.S.SimpleScriptOrReferenceInput lang
  h (GYBuildSimpleScriptInlined v) = Api.SScript $ simpleScriptToApi v
  h (GYBuildSimpleScriptReference ref _s) = Api.SReferenceScript (txOutRefToApi ref)

type GYStakeValScript v = GYBuildPlutusScript v

pattern GYStakeValScript :: () => VersionIsGreaterOrEqual v u => GYScript v -> GYBuildPlutusScript u
pattern GYStakeValScript s = GYBuildPlutusScriptInlined s

pattern GYStakeValReference :: () => v `VersionIsGreaterOrEqual` u => GYTxOutRef -> GYScript v -> GYBuildPlutusScript u
pattern GYStakeValReference r s = GYBuildPlutusScriptReference r s

{-# COMPLETE GYStakeValScript, GYStakeValReference #-}

gyStakeValScriptToSerialisedScript :: GYStakeValScript u -> Plutus.SerialisedScript
gyStakeValScriptToSerialisedScript (GYStakeValScript mp) = coerce mp & scriptToSerialisedScript & coerce
gyStakeValScriptToSerialisedScript (GYStakeValReference _ s) = scriptToSerialisedScript s & coerce

gyStakeValScriptWitnessToApiPlutusSW ::
  GYStakeValScript u ->
  Api.S.ScriptRedeemer ->
  Api.S.ExecutionUnits ->
  Api.S.ScriptWitness Api.S.WitCtxStake ApiEra
gyStakeValScriptWitnessToApiPlutusSW (GYStakeValScript p) = stakeValidatorToApiPlutusScriptWitness p
gyStakeValScriptWitnessToApiPlutusSW (GYStakeValReference r s) =
  referenceScriptToApiPlutusScriptWitness
    r
    s
    Api.NoScriptDatumForStake

stakeValidatorVersionFromWitness :: GYStakeValScript v -> PlutusVersion
stakeValidatorVersionFromWitness (GYStakeValScript mp) = fromSingPlutusVersion $ stakeValidatorVersion mp
stakeValidatorVersionFromWitness (GYStakeValReference _ s) = fromSingPlutusVersion $ stakeValidatorVersion $ coerce s

type GYMintScript v = GYBuildScript v

pattern GYMintScript :: () => VersionIsGreaterOrEqual v u => GYScript v -> GYBuildScript u
pattern GYMintScript s = GYBuildPlutusScript (GYBuildPlutusScriptInlined s)

pattern GYMintReference :: () => v `VersionIsGreaterOrEqual` u => GYTxOutRef -> GYScript v -> GYBuildScript u
pattern GYMintReference r s = GYBuildPlutusScript (GYBuildPlutusScriptReference r s)

gyMintingScriptWitnessToApiPlutusSW ::
  GYBuildPlutusScript u ->
  Api.S.ScriptRedeemer ->
  Api.S.ExecutionUnits ->
  Api.S.ScriptWitness Api.S.WitCtxMint ApiEra
gyMintingScriptWitnessToApiPlutusSW (GYBuildPlutusScriptInlined p) = mintingPolicyToApiPlutusScriptWitness p
gyMintingScriptWitnessToApiPlutusSW (GYBuildPlutusScriptReference r s) =
  referenceScriptToApiPlutusScriptWitness
    r
    s
    Api.NoScriptDatumForMint

mintingPolicyIdFromWitness :: GYBuildScript v -> GYMintingPolicyId
mintingPolicyIdFromWitness (GYBuildPlutusScript (GYBuildPlutusScriptInlined s)) = mintingPolicyId s
mintingPolicyIdFromWitness (GYBuildPlutusScript (GYBuildPlutusScriptReference _ s)) = mintingPolicyId s
mintingPolicyIdFromWitness (GYBuildSimpleScript (GYBuildSimpleScriptInlined s)) = simpleScriptToPolicyId s
mintingPolicyIdFromWitness (GYBuildSimpleScript (GYBuildSimpleScriptReference _ s)) = simpleScriptToPolicyId s

mintingPolicyApiIdFromWitness :: GYBuildScript v -> Api.PolicyId
mintingPolicyApiIdFromWitness = mintingPolicyIdToApi . mintingPolicyIdFromWitness
