{-|
Module      : GeniusYield.Types.TxIn
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.TxIn (
    GYTxIn (..),
    GYInScript (..),
    inScriptVersion,
    GYTxInWitness (..),
    txInToApi,
) where

import           Data.GADT.Compare               (defaultEq)

import qualified Cardano.Api                     as Api

import           GeniusYield.Types.Datum
import           GeniusYield.Types.PlutusVersion
import           GeniusYield.Types.Redeemer
import           GeniusYield.Types.Script
import           GeniusYield.Types.TxOutRef
-- | Transaction input:
--
-- * an UTxO
--
-- * non-key witness for script utxos
--
-- The parameter @v@ indicates the minimum version of scripts allowed as inputs
-- in the transaction.
--
data GYTxIn v = GYTxIn
    { gyTxInTxOutRef :: !GYTxOutRef
    , gyTxInWitness  :: !(GYTxInWitness v)
    }
  deriving (Eq, Show)

-- | Represents witness type and associated information for tx inputs.
data GYTxInWitness v
    -- | Key witness without datum.
    = GYTxInWitnessKey
    -- | Script witness with associated script, datum, and redeemer.
    | GYTxInWitnessScript !(GYInScript v) !GYDatum !GYRedeemer
    deriving stock (Eq, Show)

data GYInScript (u :: PlutusVersion) where
    -- | 'VersionIsGreaterOrEqual' restricts which version validators can be used in this transaction.
    GYInScript    :: v `VersionIsGreaterOrEqual` u => GYValidator v -> GYInScript u

    -- | Reference inputs can be only used in V2 transactions.
    GYInReference :: !GYTxOutRef -> !(GYScript 'PlutusV2) -> GYInScript 'PlutusV2

-- | Returns the 'PlutusVersion' of the given 'GYInScript'.
inScriptVersion :: GYInScript v -> PlutusVersion
inScriptVersion (GYInReference _ _) = PlutusV2
inScriptVersion (GYInScript v)      = case validatorVersion v of
    SingPlutusV2 -> PlutusV2
    SingPlutusV1 -> PlutusV1

deriving instance Show (GYInScript v)

instance Eq (GYInScript v) where
    GYInReference ref1 script1 == GYInReference ref2 script2 = ref1 == ref2 && script1 == script2
    GYInScript v1 == GYInScript v2                           = defaultEq v1 v2
    _ == _ = False

-- |
--
-- /Note:/ @TxIns@ type synonym is not exported: https://github.com/input-output-hk/cardano-node/issues/3732
txInToApi
    :: Bool   -- ^ does corresponding utxo contains inline datum?
    -> GYTxIn v
    -> (Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn Api.BabbageEra))
txInToApi useInline (GYTxIn oref m) = (txOutRefToApi oref, Api.BuildTxWith $ f m) where
    f :: GYTxInWitness v -> Api.Witness Api.WitCtxTxIn Api.BabbageEra
    f GYTxInWitnessKey = Api.KeyWitness Api.KeyWitnessForSpending
    f (GYTxInWitnessScript v d r) =
        Api.ScriptWitness Api.ScriptWitnessForSpending $ g v
        (if useInline then Api.InlineScriptDatum else Api.ScriptDatumForTxIn $ Api.unsafeHashableScriptData $ datumToApi' d)
        (Api.unsafeHashableScriptData $ redeemerToApi r)
        (Api.ExecutionUnits 0 0)

    g (GYInScript v)        = validatorToApiPlutusScriptWitness v
    g (GYInReference ref s) = referenceScriptToApiPlutusScriptWitness ref s
