{-|
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

import           Control.Lens                    (Traversal)

import qualified Cardano.Api                     as Api
import qualified Cardano.Api.Shelley             as Api.S

import           GeniusYield.Types.Address
import           GeniusYield.Types.Datum
import           GeniusYield.Types.PlutusVersion
import           GeniusYield.Types.Script
import           GeniusYield.Types.Value

-- | Transaction output.
--
-- The parameter @v@ indicates the minimum version of scripts allowed as inputs
-- in the transaction.
--
data GYTxOut (v :: PlutusVersion) = GYTxOut
    { gyTxOutAddress :: !GYAddress
    , gyTxOutValue   :: !GYValue
    , gyTxOutDatum   :: !(Maybe (GYDatum, GYTxOutUseInlineDatum v))  -- ^ The Boolean indicates whether to use inline datums or not. May be overridden by a flag to 'txOutToApi'.
    , gyTxOutRefS    :: !(Maybe (GYScript 'PlutusV2))
    } deriving stock (Eq, Show)

data GYTxOutUseInlineDatum (v :: PlutusVersion) where
    GYTxOutUseInlineDatum     :: GYTxOutUseInlineDatum 'PlutusV2
    GYTxOutDontUseInlineDatum :: GYTxOutUseInlineDatum v

deriving instance Show (GYTxOutUseInlineDatum v)
deriving instance Eq (GYTxOutUseInlineDatum v)

-- | The most common variant: create a 'GYTxOut' from address, value and datum
mkGYTxOut :: GYAddress -> GYValue -> GYDatum -> GYTxOut v
mkGYTxOut addr v d = GYTxOut
    { gyTxOutAddress     = addr
    , gyTxOutValue       = v
    , gyTxOutDatum       = Just (d, GYTxOutDontUseInlineDatum)
    , gyTxOutRefS        = Nothing
    }

-- | Same as 'mkGYTxOut' but without a datum.
mkGYTxOutNoDatum :: GYAddress -> GYValue -> GYTxOut v
mkGYTxOutNoDatum addr v = GYTxOut
    { gyTxOutAddress     = addr
    , gyTxOutValue       = v
    , gyTxOutDatum       = Nothing
    , gyTxOutRefS        = Nothing
    }

-- | Whether to use inline datum in this transaction output
gyTxOutDatumL :: Traversal (GYTxOut v) (GYTxOut u) (GYTxOutUseInlineDatum v) (GYTxOutUseInlineDatum u)
gyTxOutDatumL f (GYTxOut addr v md s) =
    (\md' -> GYTxOut addr v md' s) <$> traverse (traverse f) md

txOutToApi
    :: GYTxOut v
    -> Api.TxOut Api.CtxTx Api.BabbageEra
txOutToApi (GYTxOut addr v md mrs) = Api.TxOut
    (addressToApi' addr)
    (Api.TxOutValue Api.MultiAssetInBabbageEra $ valueToApi v)
    (mkDatum md)
    (maybe Api.S.ReferenceScriptNone (Api.S.ReferenceScript Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra . Api.toScriptInAnyLang . Api.PlutusScript Api.S.PlutusScriptV2 . scriptToApi) mrs)
  where
    mkDatum :: Maybe (GYDatum, GYTxOutUseInlineDatum v) -> Api.TxOutDatum Api.CtxTx Api.BabbageEra
    mkDatum Nothing        = Api.TxOutDatumNone
    mkDatum (Just (d, di))
        | di'    = Api.TxOutDatumInline Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra d'
        | otherwise        = Api.TxOutDatumInTx Api.ScriptDataInBabbageEra d'
      where
        d' = datumToApi' d

        di' = case di of
          GYTxOutUseInlineDatum     -> True
          GYTxOutDontUseInlineDatum -> False
