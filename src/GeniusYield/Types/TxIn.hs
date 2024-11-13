{- |
Module      : GeniusYield.Types.TxIn
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxIn (
  GYTxIn (..),
  GYInScript (..),
  GYInSimpleScript (..),
  inScriptVersion,
  GYTxInWitness (..),
  txInToApi,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api
import Data.GADT.Compare (defaultEq)
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
  | -- | Script witness with associated script, datum, and redeemer.
    GYTxInWitnessScript !(GYInScript v) !GYDatum !GYRedeemer
  | -- | Simple script witness.
    GYTxInWitnessSimpleScript !(GYInSimpleScript v)
  deriving stock (Eq, Show)

data GYInScript (u :: PlutusVersion) where
  -- | 'VersionIsGreaterOrEqual' restricts which version validators can be used in this transaction.
  GYInScript :: forall u v. v `VersionIsGreaterOrEqual` u => GYScript v -> GYInScript u
  -- | Reference inputs can be only used in V2 transactions.
  GYInReference :: forall v. v `VersionIsGreaterOrEqual` 'PlutusV2 => !GYTxOutRef -> !(GYScript v) -> GYInScript v

-- | Returns the 'PlutusVersion' of the given 'GYInScript'.
inScriptVersion :: GYInScript v -> PlutusVersion
inScriptVersion (GYInReference _ s) = case scriptVersion s of
  SingPlutusV3 -> PlutusV3
  SingPlutusV2 -> PlutusV2
inScriptVersion (GYInScript v) = case validatorVersion v of
  SingPlutusV3 -> PlutusV3
  SingPlutusV2 -> PlutusV2
  SingPlutusV1 -> PlutusV1

deriving instance Show (GYInScript v)

instance Eq (GYInScript v) where
  GYInReference ref1 script1 == GYInReference ref2 script2 = ref1 == ref2 && script1 == script2
  GYInScript v1 == GYInScript v2 = defaultEq v1 v2
  _ == _ = False

data GYInSimpleScript (u :: PlutusVersion) where
  GYInSimpleScript :: !GYSimpleScript -> GYInSimpleScript u
  GYInReferenceSimpleScript :: v `VersionIsGreaterOrEqual` 'PlutusV2 => !GYTxOutRef -> !GYSimpleScript -> GYInSimpleScript v

deriving instance Show (GYInSimpleScript v)

instance Eq (GYInSimpleScript v) where
  GYInSimpleScript s1 == GYInSimpleScript s2 = s1 == s2
  GYInReferenceSimpleScript ref1 s1 == GYInReferenceSimpleScript ref2 s2 = ref1 == ref2 && s1 == s2
  _ == _ = False

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
  f (GYTxInWitnessScript v d r) =
    Api.ScriptWitness Api.ScriptWitnessForSpending $
      ( case v of
          GYInScript s -> validatorToApiPlutusScriptWitness s
          GYInReference ref s -> referenceScriptToApiPlutusScriptWitness ref s
      )
        (if useInline then Api.InlineScriptDatum else Api.ScriptDatumForTxIn $ Just $ datumToApi' d)
        (redeemerToApi r)
        (Api.ExecutionUnits 0 0)
  f (GYTxInWitnessSimpleScript v) =
    Api.ScriptWitness Api.ScriptWitnessForSpending $ Api.SimpleScriptWitness Api.SimpleScriptInConway $ h v

  h (GYInSimpleScript v) = Api.SScript $ simpleScriptToApi v
  h (GYInReferenceSimpleScript ref s) = Api.SReferenceScript (txOutRefToApi ref) $ Just $ hashSimpleScript' s
