{- |
Module      : GeniusYield.Types.TxIn
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxIn (
  GYTxIn (..),
  GYInScript,
  pattern GYInScript,
  pattern GYInReference,
  GYInSimpleScript,
  pattern GYInSimpleScript,
  pattern GYInReferenceSimpleScript,
  inScriptVersion,
  GYTxInWitness (..),
  txInToApi,
) where

import Cardano.Api qualified as Api
import GeniusYield.Types.BuildScript
import GeniusYield.Types.Datum
import GeniusYield.Types.Era
import GeniusYield.Types.PlutusVersion
import GeniusYield.Types.Redeemer
import GeniusYield.Types.Script
import GeniusYield.Types.TxOutRef

{- | Transaction input:

* an UTxO

* non-key witness for script utxos

The parameter @v@ indicates the minimum version of scripts allowed as inputs
in the transaction.
-}
data GYTxIn v = GYTxIn
  { gyTxInTxOutRef :: !GYTxOutRef
  , gyTxInWitness :: !(GYTxInWitness v)
  }
  deriving (Eq, Show)

-- | Represents witness type and associated information for tx inputs.
data GYTxInWitness v
  = -- | Key witness without datum.
    GYTxInWitnessKey
  | -- | Script witness with associated script, datum, and redeemer. Datum can be omitted if it is inlined in the input or if it's not needed under PlutusV3 (or beyond) script.
    GYTxInWitnessScript !(GYBuildPlutusScript v) !(Maybe GYDatum) !GYRedeemer
  | -- | Simple script witness.
    GYTxInWitnessSimpleScript !(GYBuildSimpleScript v)
  deriving stock (Eq, Show)

type GYInScript = GYBuildPlutusScript

pattern GYInScript :: () => v `VersionIsGreaterOrEqual` u => GYScript v -> GYBuildPlutusScript u
pattern GYInScript s = GYBuildPlutusScriptInlined s

pattern GYInReference :: () => v `VersionIsGreaterOrEqual` u => GYTxOutRef -> GYScript v -> GYBuildPlutusScript u
pattern GYInReference ref s = GYBuildPlutusScriptReference ref s

{-# COMPLETE GYInScript, GYInReference #-}

-- | Returns the 'PlutusVersion' of the given 'GYBuildPlutusScript'.
inScriptVersion :: GYBuildPlutusScript v -> PlutusVersion
inScriptVersion = buildPlutusScriptVersion

type GYInSimpleScript = GYBuildSimpleScript

pattern GYInSimpleScript :: GYSimpleScript -> GYBuildSimpleScript u
pattern GYInSimpleScript s = GYBuildSimpleScriptInlined s
pattern GYInReferenceSimpleScript :: () => GYTxOutRef -> GYSimpleScript -> GYBuildSimpleScript u
pattern GYInReferenceSimpleScript ref s = GYBuildSimpleScriptReference ref s

{-# COMPLETE GYInSimpleScript, GYInReferenceSimpleScript #-}

{- |

/Note:/ @TxIns@ type synonym is not exported: https://github.com/input-output-hk/cardano-node/issues/3732
-}
txInToApi ::
  -- | does corresponding utxo contains inline datum?
  Bool ->
  GYTxIn v ->
  (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn ApiEra))
txInToApi useInline (GYTxIn oref m) = (txOutRefToApi oref, Api.BuildTxWith $ f m)
 where
  f :: GYTxInWitness v -> Api.Witness Api.WitCtxTxIn ApiEra
  f GYTxInWitnessKey = Api.KeyWitness Api.KeyWitnessForSpending
  f (GYTxInWitnessScript v md r) =
    Api.ScriptWitness Api.ScriptWitnessForSpending $
      ( case v of
          GYBuildPlutusScriptInlined s -> validatorToApiPlutusScriptWitness s
          GYBuildPlutusScriptReference ref s -> referenceScriptToApiPlutusScriptWitness ref s
      )
        (if useInline then Api.InlineScriptDatum else Api.ScriptDatumForTxIn $ datumToApi' <$> md)
        (redeemerToApi r)
        (Api.ExecutionUnits 0 0)
  f (GYTxInWitnessSimpleScript v) =
    Api.ScriptWitness Api.ScriptWitnessForSpending $ simpleScriptWitnessToApi v
