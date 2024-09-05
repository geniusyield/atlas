{- |
Module      : GeniusYield.Types.TxOut
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxOut (
  GYTxOut (..),
  GYTxOutUseInlineDatum (..),
  gyTxOutDatumL,
  mkGYTxOut,
  mkGYTxOutNoDatum,
  txOutToApi,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Control.Lens (Traversal)
import GeniusYield.Types.Address
import GeniusYield.Types.Datum
import GeniusYield.Types.Era
import GeniusYield.Types.PlutusVersion
import GeniusYield.Types.Script
import GeniusYield.Types.Value

{- | Transaction output.

The parameter @v@ indicates the minimum version of scripts allowed as inputs
in the transaction.
-}
data GYTxOut (v :: PlutusVersion) = GYTxOut
  { gyTxOutAddress :: !GYAddress
  , gyTxOutValue :: !GYValue
  , gyTxOutDatum :: !(Maybe (GYDatum, GYTxOutUseInlineDatum v))
  -- ^ The Boolean indicates whether to use inline datums or not. May be overridden by a flag to 'txOutToApi'.
  , gyTxOutRefS :: !(Maybe GYAnyScript)
  }
  deriving stock (Eq, Show)

data GYTxOutUseInlineDatum (v :: PlutusVersion) where
  GYTxOutUseInlineDatum :: (v `VersionIsGreaterOrEqual` 'PlutusV2) => GYTxOutUseInlineDatum v
  GYTxOutDontUseInlineDatum :: GYTxOutUseInlineDatum v

deriving instance Show (GYTxOutUseInlineDatum v)
deriving instance Eq (GYTxOutUseInlineDatum v)

-- | The most common variant: create a 'GYTxOut' from address, value and datum
mkGYTxOut :: GYAddress -> GYValue -> GYDatum -> GYTxOut v
mkGYTxOut addr v d =
  GYTxOut
    { gyTxOutAddress = addr
    , gyTxOutValue = v
    , gyTxOutDatum = Just (d, GYTxOutDontUseInlineDatum)
    , gyTxOutRefS = Nothing
    }

-- | Same as 'mkGYTxOut' but without a datum.
mkGYTxOutNoDatum :: GYAddress -> GYValue -> GYTxOut v
mkGYTxOutNoDatum addr v =
  GYTxOut
    { gyTxOutAddress = addr
    , gyTxOutValue = v
    , gyTxOutDatum = Nothing
    , gyTxOutRefS = Nothing
    }

-- | Whether to use inline datum in this transaction output
gyTxOutDatumL :: Traversal (GYTxOut v) (GYTxOut u) (GYTxOutUseInlineDatum v) (GYTxOutUseInlineDatum u)
gyTxOutDatumL f (GYTxOut addr v md s) =
  (\md' -> GYTxOut addr v md' s) <$> traverse (traverse f) md

txOutToApi ::
  GYTxOut v ->
  Api.TxOut Api.CtxTx ApiEra
txOutToApi (GYTxOut addr v md mrs) =
  Api.TxOut
    (addressToApi' addr)
    (valueToApiTxOutValue v)
    (mkDatum md)
    (maybe Api.S.ReferenceScriptNone (Api.S.ReferenceScript Api.S.BabbageEraOnwardsConway . resolveOutputScript) mrs)
  where
    resolveOutputScript (GYSimpleScript s) = Api.ScriptInAnyLang Api.SimpleScriptLanguage (Api.SimpleScript $ simpleScriptToApi s)
    resolveOutputScript (GYPlutusScript s) =
      let version = singPlutusVersionToApi $ scriptVersion s
       in Api.ScriptInAnyLang (Api.PlutusScriptLanguage version) (Api.PlutusScript version (scriptToApi s))

    mkDatum :: Maybe (GYDatum, GYTxOutUseInlineDatum v) -> Api.TxOutDatum Api.CtxTx ApiEra
    mkDatum Nothing = Api.TxOutDatumNone
    mkDatum (Just (d, di))
      | di' = Api.TxOutDatumInline Api.BabbageEraOnwardsConway d'
      | otherwise = Api.TxOutDatumInTx Api.AlonzoEraOnwardsConway d'
      where
        d' = datumToApi' d

        di' = case di of
          GYTxOutUseInlineDatum -> True
          GYTxOutDontUseInlineDatum -> False
