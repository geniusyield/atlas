{-|
Module      : GeniusYield.Types.Datum
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Datum (
    -- * Datum
    GYDatum,
    datumToApi',
    datumFromApi',
    datumToPlutus,
    datumToPlutus',
    datumFromPlutus,
    datumFromPlutus',
    datumFromPlutusData,
    hashDatum,
    -- * Datum hash
    GYDatumHash,
    datumHashFromHex,
    datumHashFromHexE,
    datumHashFromPlutus,
    unsafeDatumHashFromPlutus,
    datumHashToPlutus,
    datumHashFromApi,
    datumHashToApi,
) where

import qualified Cardano.Api                          as Api
import           Control.Monad                        ((>=>))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Base16               as Base16
import qualified Data.ByteString.Char8                as BS8
import           Data.Either.Combinators              (maybeToRight)
import qualified Data.Text                            as Txt
import qualified Database.PostgreSQL.Simple           as PQ
import qualified Database.PostgreSQL.Simple.FromField as PQ (FromField (..), returnError)
import qualified Database.PostgreSQL.Simple.ToField   as PQ
import qualified Plutus.V1.Ledger.Api                 as Plutus

import           GeniusYield.Imports
import           GeniusYield.Types.Ledger

-- | Datum
--
-- In the GY system we always include datums in transactions
-- so this simple type is sufficient.
--
newtype GYDatum = GYDatum Plutus.BuiltinData
    deriving stock (Eq, Show)
    deriving newtype (Plutus.ToData, Plutus.FromData)

-- | Convert a 'GYDatum' to 'Api.ScriptData' from Cardano Api
datumToApi' :: GYDatum -> Api.ScriptData
datumToApi' (GYDatum (Plutus.BuiltinData x)) = dataToScriptData x

-- | Get a 'GYDatum' from a Cardano Api 'Api.ScriptData'
datumFromApi' :: Api.ScriptData -> GYDatum
datumFromApi' = GYDatum . Plutus.BuiltinData . scriptDataToData

-- | Convert a 'GYDatum' to 'Plutus.Datum' from Plutus
datumToPlutus :: GYDatum -> Plutus.Datum
datumToPlutus = Plutus.Datum . datumToPlutus'

-- | Convert a 'GYDatum' to 'Plutus.BuiltinData' from Plutus
datumToPlutus' :: GYDatum -> Plutus.BuiltinData
datumToPlutus' (GYDatum x) = x

-- | Get a 'GYDatum' from a Plutus 'Plutus.Datum'
datumFromPlutus :: Plutus.Datum -> GYDatum
datumFromPlutus (Plutus.Datum d) = GYDatum d

-- | Get a 'GYDatum' from a Plutus 'Plutus.BuiltinData'
datumFromPlutus' :: Plutus.BuiltinData -> GYDatum
datumFromPlutus' = GYDatum

-- | Get a 'GYDatum' from any Plutus 'Plutus.ToData' type.
datumFromPlutusData :: Plutus.ToData a => a -> GYDatum
datumFromPlutusData = GYDatum . Plutus.toBuiltinData

scriptDataToData :: Api.ScriptData -> Plutus.Data
scriptDataToData (Api.ScriptDataConstructor n xs) = Plutus.Constr n $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataMap xs)           = Plutus.Map $ bimap scriptDataToData scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataList xs)          = Plutus.List $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataNumber n)         = Plutus.I n
scriptDataToData (Api.ScriptDataBytes bs)         = Plutus.B bs

dataToScriptData :: Plutus.Data -> Api.ScriptData
dataToScriptData (Plutus.Constr n xs) = Api.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Plutus.Map xs)      = Api.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (Plutus.List xs)     = Api.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (Plutus.I n)         = Api.ScriptDataNumber n
dataToScriptData (Plutus.B bs)        = Api.ScriptDataBytes bs

-- | Returns the 'GYDatumHash' of the given 'GYDatum'
hashDatum :: GYDatum -> GYDatumHash
hashDatum = datumHashFromApi . Api.hashScriptData . datumToApi'

-------------------------------------------------------------------------------
-- DatumHash
-------------------------------------------------------------------------------

newtype GYDatumHash = GYDatumHash (Api.Hash Api.ScriptData)
    deriving stock   (Show)
    deriving newtype (Eq, Ord, ToJSON, FromJSON)

instance IsString GYDatumHash where
    fromString = unsafeDatumHashFromPlutus . fromString

instance PQ.FromField GYDatumHash where
    fromField f bs' = do
        PQ.Binary bs <- PQ.fromField f bs'
        case Api.deserialiseFromRawBytes (Api.AsHash Api.AsScriptData) bs of
            Just dh -> return (datumHashFromApi dh)
            Nothing -> PQ.returnError PQ.ConversionFailed f "datum hash does not unserialise"

instance PQ.ToField GYDatumHash where
    toField (GYDatumHash dh) = PQ.toField (PQ.Binary (Api.serialiseToRawBytes dh))

datumHashFromHex :: String -> Maybe GYDatumHash
datumHashFromHex = rightToMaybe . datumHashFromHexE

datumHashFromBS :: ByteString -> Either String GYDatumHash
datumHashFromBS = fmap datumHashFromApi
    . maybeToRight "RawBytes GYDatumHash decode fail"
    . Api.deserialiseFromRawBytes (Api.proxyToAsType @(Api.Hash Api.ScriptData) Proxy)

datumHashFromHexE :: String -> Either String GYDatumHash
datumHashFromHexE = Base16.decode . BS8.pack
    >=> datumHashFromBS

datumHashFromPlutus :: Plutus.DatumHash -> Either PlutusToCardanoError GYDatumHash
datumHashFromPlutus (Plutus.DatumHash h) = first
    (\t -> DeserialiseRawBytesError . Txt.pack $ "datumHashFromPlutus" ++ '.':t)
    . datumHashFromBS $ Plutus.fromBuiltin h

unsafeDatumHashFromPlutus :: Plutus.DatumHash -> GYDatumHash
unsafeDatumHashFromPlutus =
    either (error . ("unsafeDatumHashFromPlutus: " ++) . show) id . datumHashFromPlutus

-- TODO: remove me
datumHashToPlutus :: GYDatumHash -> Plutus.DatumHash
datumHashToPlutus h = Plutus.DatumHash (Plutus.toBuiltin (Api.serialiseToRawBytes (datumHashToApi h)))

datumHashFromApi :: Api.Hash Api.ScriptData -> GYDatumHash
datumHashFromApi = coerce

datumHashToApi :: GYDatumHash -> Api.Hash Api.ScriptData
datumHashToApi = coerce
