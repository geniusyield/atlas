{-|
Module      : GeniusYield.Types.UTxO
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.UTxO (
    GYUTxO (..),
    utxoFromApi,
    utxoFromApi',
    utxoToPlutus,
    utxoHasInlineDatum,
    utxoHasReferenceScript,
    utxoTranslatableToV1,
    GYUTxOs,
    utxosSize,
    utxosFromApi,
    utxosToApi,
    utxosRemoveTxOutRef,
    utxosRemoveTxOutRefs,
    utxosRemoveRefScripts,
    utxosLookup,
    someTxOutRef,
    randomTxOutRef,
    -- * UTxO datums
    GYOutDatum (..),
    isInlineDatum,
    outDatumToPlutus,
    -- * Filter and map
    filterUTxOs,
    mapMaybeUTxOs,
    mapUTxOs,
    witherUTxOs,
    -- * List conversions
    utxosFromList,
    utxosFromUTxO,
    utxosToList,
    -- * Folds
    foldlUTxOs',
    foldMapUTxOs,
    forUTxOs_,
    foldMUTxOs,
    -- * Extract refs
    utxosRefs,
) where

import           GeniusYield.Imports

import qualified Cardano.Api                as Api
import qualified Cardano.Api.Shelley        as Api.S
import           Control.Monad.Random       (MonadRandom (getRandomR))
import qualified Data.Map.Strict            as Map
import qualified Plutus.V2.Ledger.Tx        as Plutus
import qualified Text.Printf                as Printf

import           Data.Maybe                 (isNothing)
import           GeniusYield.Types.Address
import           GeniusYield.Types.Datum
import           GeniusYield.Types.Script
import           GeniusYield.Types.TxOutRef
import           GeniusYield.Types.Value

-- | The datum contained within a transaction output.
data GYOutDatum
  -- | The output has no datum.
  = GYOutDatumNone
  -- | The output contains a datum hash, the associated datum may or may not be included in the tx.
  | GYOutDatumHash !GYDatumHash
  -- | The output contains an inline datum (i.e datum within the output itself).
  | GYOutDatumInline !GYDatum
  deriving stock (Eq, Show)

isInlineDatum :: GYOutDatum -> Bool
isInlineDatum (GYOutDatumInline _) = True
isInlineDatum _                    = False

outDatumToPlutus :: GYOutDatum -> Plutus.OutputDatum
outDatumToPlutus GYOutDatumNone       = Plutus.NoOutputDatum
outDatumToPlutus (GYOutDatumHash h)   = Plutus.OutputDatumHash $ datumHashToPlutus h
outDatumToPlutus (GYOutDatumInline d) = Plutus.OutputDatum     $ datumToPlutus d

-- | An unspent transaction output.
--
data GYUTxO = GYUTxO
    { utxoRef       :: !GYTxOutRef
    , utxoAddress   :: !GYAddress
    , utxoValue     :: !GYValue
    , utxoOutDatum  :: !GYOutDatum
    , utxoRefScript :: !(Maybe (Some GYScript)) -- TODO: change to GYScriptHash? #31, but then we won't be able to convert faithfully back to Api.UTxO (https://github.com/geniusyield/atlas/issues/31)
    } deriving stock (Eq, Show)

instance Ord GYUTxO where
  u1 `compare` u2 = utxoRef u1 `compare` utxoRef u2

-- | A set of unspent transaction outputs.
--
-- Actually a map from unspent transaction outputs to address, value and datum hash.
--
newtype GYUTxOs = GYUTxOs (Map GYTxOutRef (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript)))
  deriving (Eq, Show)

instance Semigroup GYUTxOs where
    GYUTxOs x <> GYUTxOs y = GYUTxOs (Map.union x y)

instance Monoid GYUTxOs where
    mempty = GYUTxOs mempty

utxosFromApi :: Api.UTxO era -> GYUTxOs
utxosFromApi (Api.UTxO m) = utxosFromList
    [ utxoFromApi' txIn out
    | (txIn, out) <- Map.toList m
    ]

utxosToApi :: GYUTxOs -> Api.UTxO Api.BabbageEra
utxosToApi (GYUTxOs m) = Api.UTxO $ Map.foldlWithKey' f Map.empty m
  where
    f :: Map Api.TxIn (Api.TxOut Api.CtxUTxO Api.BabbageEra)
      -> GYTxOutRef -> (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript))
      -> Map Api.TxIn (Api.TxOut Api.CtxUTxO Api.BabbageEra)
    f m' oref out = Map.insert (txOutRefToApi oref) (g out) m'

    g :: (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript)) -> Api.TxOut Api.CtxUTxO Api.BabbageEra
    g (addr, v, md, ms) = Api.TxOut
        (addressToApi' addr)
        (valueToApiTxOutValue v)
        (outDatumToApi md)
        (maybe Api.S.ReferenceScriptNone someScriptToReferenceApi ms)

    outDatumToApi GYOutDatumNone     = Api.TxOutDatumNone
    outDatumToApi (GYOutDatumHash h) = Api.TxOutDatumHash Api.ScriptDataInBabbageEra $ datumHashToApi h
    outDatumToApi (GYOutDatumInline d) = Api.TxOutDatumInline Api.S.ReferenceTxInsScriptsInlineDatumsInBabbageEra $ datumToApi' d

utxoFromApi :: Api.TxIn -> Api.TxOut Api.CtxTx Api.BabbageEra -> GYUTxO
utxoFromApi txIn (Api.TxOut a v d s) = GYUTxO
    { utxoRef       = txOutRefFromApi txIn
    , utxoAddress   = addressFromApi' a
    , utxoValue     = valueFromApiTxOutValue v
    , utxoOutDatum  = f d
    , utxoRefScript = someScriptFromReferenceApi s
    }
  where
    f :: Api.TxOutDatum Api.CtxTx Api.BabbageEra -> GYOutDatum
    f Api.TxOutDatumNone          = GYOutDatumNone
    f (Api.TxOutDatumHash _ hash) = GYOutDatumHash $ datumHashFromApi hash
    f (Api.TxOutDatumInTx _ sd)   = GYOutDatumHash . hashDatum $ datumFromApi' sd
    f (Api.TxOutDatumInline _ sd) = GYOutDatumInline $ datumFromApi' sd

utxoFromApi' :: Api.TxIn -> Api.TxOut Api.CtxUTxO era -> GYUTxO
utxoFromApi' txIn (Api.TxOut a v d s) = GYUTxO
    { utxoRef       = txOutRefFromApi txIn
    , utxoAddress   = addressFromApi' a
    , utxoValue     = valueFromApiTxOutValue v
    , utxoOutDatum  = f d
    , utxoRefScript = someScriptFromReferenceApi s
    }
  where
    f :: Api.TxOutDatum Api.CtxUTxO era -> GYOutDatum
    f Api.TxOutDatumNone          = GYOutDatumNone
    f (Api.TxOutDatumHash _ hash) = GYOutDatumHash $ datumHashFromApi hash
    f (Api.TxOutDatumInline _ sd) = GYOutDatumInline $ datumFromApi' sd

utxoToPlutus :: GYUTxO -> Plutus.TxOut
utxoToPlutus GYUTxO{..} = Plutus.TxOut
    { Plutus.txOutAddress         = addressToPlutus utxoAddress
    , Plutus.txOutValue           = valueToPlutus utxoValue
    , Plutus.txOutDatum           = outDatumToPlutus utxoOutDatum
    , Plutus.txOutReferenceScript = (\(Some s) -> scriptPlutusHash s) <$> utxoRefScript
    }

-- | Whether the UTxO has it's datum inlined?
utxoHasInlineDatum :: GYUTxO -> Bool
utxoHasInlineDatum  = isInlineDatum . utxoOutDatum

-- | Whether the UTxO has script to refer?
utxoHasReferenceScript :: GYUTxO -> Bool
utxoHasReferenceScript = isJust . utxoRefScript

-- | Is a UTxO translatable to language PlutusV1 output?
utxoTranslatableToV1 :: GYUTxO -> Bool
utxoTranslatableToV1 u = not (utxoHasReferenceScript u) && not (utxoHasInlineDatum u)

-- | Number of UTxOs within given 'GYUTxOs'.
utxosSize :: GYUTxOs -> Int
utxosSize (GYUTxOs m) = Map.size m

-- | Remove particular 'GYTxOutRef' from 'GYUTxOs'.
--
-- Used to remove collateral, so we don't use it in transactions.
--
utxosRemoveTxOutRef :: GYTxOutRef -> GYUTxOs -> GYUTxOs
utxosRemoveTxOutRef oref (GYUTxOs m) = GYUTxOs $ Map.delete oref m

-- | Remove several 'GYTxOutRef's from 'GYUTxOs'.
utxosRemoveTxOutRefs :: Set GYTxOutRef -> GYUTxOs -> GYUTxOs
utxosRemoveTxOutRefs orefs (GYUTxOs m) = GYUTxOs $ Map.withoutKeys m orefs

-- | Remove UTxOs containing reference scripts inside them from 'GYUTxOs'.
utxosRemoveRefScripts :: GYUTxOs -> GYUTxOs
utxosRemoveRefScripts = filterUTxOs $ isNothing . utxoRefScript

-- | Lookup a UTxO given a ref.
utxosLookup :: GYTxOutRef -> GYUTxOs -> Maybe GYUTxO
utxosLookup r (GYUTxOs m) = (\(a, v, mh, ms) -> GYUTxO r a v mh ms) <$> Map.lookup r m

-- | Get some output reference from 'GYUTxOs'.
--
-- Used to pick an input for minting, or selecting collateral (in tests).
--
someTxOutRef :: GYUTxOs -> Maybe (GYTxOutRef, GYUTxOs)
someTxOutRef (GYUTxOs m) = f <$> Map.minViewWithKey m where
    f ((oref, _), m') = (oref, GYUTxOs m')

-- | Get a random output reference from 'GYUTxOs'.
randomTxOutRef :: MonadRandom m => GYUTxOs -> m (Maybe (GYTxOutRef, GYUTxOs))
randomTxOutRef (GYUTxOs m)
  | Map.null m = pure Nothing
  | otherwise = Just <$> do
      ix <- getRandomR (0, Map.size m - 1)
      let entry = fst $ Map.elemAt ix m
      let remainder = Map.deleteAt ix m
      pure (entry, GYUTxOs remainder)

-- | Filter 'GYUTxOs' with a predicate on 'GYUTxO'.
filterUTxOs :: (GYUTxO -> Bool) -> GYUTxOs -> GYUTxOs
filterUTxOs p (GYUTxOs m) = GYUTxOs $ Map.filterWithKey p' m where
    p' r (a, v, mh, ms) = p $ GYUTxO r a v mh ms

-- | Map & filter 'GYUTxOs' contents.
mapMaybeUTxOs :: (GYUTxO -> Maybe a) -> GYUTxOs -> Map GYTxOutRef a
mapMaybeUTxOs p (GYUTxOs m) = Map.mapMaybeWithKey p' m where
    p' r (a, v, mh, ms) = p $ GYUTxO r a v mh ms

-- | Map 'GYUTxOs' contents.
mapUTxOs :: (GYUTxO -> a) -> GYUTxOs -> Map GYTxOutRef a
mapUTxOs f = mapMaybeUTxOs $ Just . f

-- | Applicative version of 'mapMaybeUTxOs'.
witherUTxOs :: Applicative f => (GYUTxO -> f (Maybe a)) -> GYUTxOs -> f (Map GYTxOutRef a)
witherUTxOs f (GYUTxOs m) = iwither g m where
    g ref (a, v, mh, ms) = f (GYUTxO ref a v mh ms)

-- | Returns a 'GYUTxOs' from a given list of 'GYUTxO's.
utxosFromList :: [GYUTxO] -> GYUTxOs
utxosFromList xs = GYUTxOs $ Map.fromList
    [ (r, (a, v, mh, ms))
    | GYUTxO r a v mh ms <- xs
    ]

-- | Returns a list of 'GYUTxO's from a given 'GYUTxOs'.
utxosToList :: GYUTxOs -> [GYUTxO]
utxosToList (GYUTxOs m) = [GYUTxO r a v mh ms | (r, (a, v, mh, ms)) <- Map.toList m]

-- | Returns a list of all 'GYTxOutRef's inside a given 'GYUTxOs'.
utxosRefs :: GYUTxOs -> [GYTxOutRef]
utxosRefs (GYUTxOs m) = Map.keys m

-- | Returns a 'GYUTxOs' from a single 'GYUTxO'.
utxosFromUTxO :: GYUTxO -> GYUTxOs
utxosFromUTxO utxo = utxosFromList [utxo]

-- | Fold operation over a 'GYUTxOs'.
foldlUTxOs' :: forall a. (a -> GYUTxO -> a) -> a -> GYUTxOs -> a
foldlUTxOs' f x (GYUTxOs m) = Map.foldlWithKey' f' x m
  where
    f' :: a -> GYTxOutRef -> (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript)) -> a
    f' y r (a, v, mh, ms) = f y $ GYUTxO r a v mh ms

-- | FoldMap operation over a 'GYUTxOs'.
foldMapUTxOs :: Monoid m => (GYUTxO -> m) -> GYUTxOs -> m
foldMapUTxOs f = foldlUTxOs' (\m utxo -> m <> f utxo) mempty

forUTxOs_ :: forall f a. Applicative f => GYUTxOs -> (GYUTxO -> f a) -> f ()
forUTxOs_ (GYUTxOs m) f = ifor_ m f'
  where
    f' :: GYTxOutRef -> (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript)) -> f a
    f' r (a, v, mh, ms) = f $ GYUTxO r a v mh ms

foldMUTxOs :: forall m a. Monad m => (a -> GYUTxO -> m a) -> a -> GYUTxOs -> m a
foldMUTxOs f x (GYUTxOs m) = foldM f' x $ Map.toList m
  where
    f' :: a -> (GYTxOutRef, (GYAddress, GYValue, GYOutDatum, Maybe (Some GYScript))) -> m a
    f' y (r, (a, v, mh, ms)) = f y $ GYUTxO r a v mh ms

instance Printf.PrintfArg GYUTxOs where
    formatArg (GYUTxOs m) = Printf.formatArg $ unlines
        [ Printf.printf "%s %s" oref v
        | (oref, (_, v, _, _)) <- Map.toList m
        ]
