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
    serialiseDatum,
    serialiseDatumHex,
    datumToApi',
    datumFromApi',
    datumToPlutus,
    datumToPlutus',
    datumFromPlutus,
    datumFromPlutus',
    datumFromPlutusData,
    hashDatum,
    hashDatum',
    hashDatum'Hex,
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
import           Data.Either.Combinators              (mapLeft)
import qualified Data.Text                            as Txt
import qualified Database.PostgreSQL.Simple           as PQ
import qualified Database.PostgreSQL.Simple.FromField as PQ (FromField (..),
                                                             returnError)
import qualified Database.PostgreSQL.Simple.ToField   as PQ
import qualified PlutusLedgerApi.V1                   as Plutus

import qualified Cardano.Api.Shelley                  as Api
import           Control.Arrow                        ((>>>))
import           GeniusYield.Imports
import           GeniusYield.Types.Ledger
import qualified PlutusTx.Builtins                    as Plutus

-- | Datum
--
-- In the GY system we always include datums in transactions
-- so this simple type is sufficient.
--
newtype GYDatum = GYDatum Plutus.BuiltinData
    deriving stock (Eq, Ord, Show)
    deriving newtype (Plutus.ToData, Plutus.FromData)

-- | Serialise datum.
serialiseDatum :: GYDatum -> ByteString
serialiseDatum = datumToPlutus' >>> Plutus.serialiseData >>> Plutus.fromBuiltin

-- | Hex representation of serialized datum.
serialiseDatumHex :: GYDatum -> ByteString
serialiseDatumHex = serialiseDatum >>> Base16.encode

-- | Convert a 'GYDatum' to 'Api.ScriptData' from Cardano Api.
datumToApi' :: GYDatum -> Api.ScriptData
datumToApi' = datumToPlutus' >>> Plutus.builtinDataToData >>> Api.fromPlutusData

-- | Get a 'GYDatum' from a Cardano Api 'Api.ScriptData'
datumFromApi' :: Api.ScriptData -> GYDatum
datumFromApi' = GYDatum . Plutus.dataToBuiltinData . Api.toPlutusData

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

-- | Returns the 'GYDatumHash' of the given 'GYDatum'
hashDatum :: GYDatum -> GYDatumHash
hashDatum = datumHashFromApi . Api.hashScriptDataBytes . Api.unsafeHashableScriptData . datumToApi'

-- | Returns the hash of the given datum.
hashDatum' :: GYDatum -> ByteString
hashDatum' = serialiseDatum >>> Plutus.toBuiltin >>> Plutus.blake2b_256 >>> Plutus.fromBuiltin

-- | Returns the hash as hex of the given datum.
hashDatum'Hex :: GYDatum -> ByteString
hashDatum'Hex = hashDatum' >>> Base16.encode

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
            Right dh -> return (datumHashFromApi dh)
            Left e -> PQ.returnError PQ.ConversionFailed f ("datum hash does not unserialise: " <> show e)

instance PQ.ToField GYDatumHash where
    toField (GYDatumHash dh) = PQ.toField (PQ.Binary (Api.serialiseToRawBytes dh))

datumHashFromHex :: String -> Maybe GYDatumHash
datumHashFromHex = rightToMaybe . datumHashFromHexE

datumHashFromBS :: ByteString -> Either String GYDatumHash
datumHashFromBS = fmap datumHashFromApi
    . mapLeft (\e -> "RawBytes GYDatumHash decode fail: " <> show e)
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

-- TODO: remove me #27
--       (https://github.com/geniusyield/atlas/issues/27)
datumHashToPlutus :: GYDatumHash -> Plutus.DatumHash
datumHashToPlutus h = Plutus.DatumHash (Plutus.toBuiltin (Api.serialiseToRawBytes (datumHashToApi h)))

datumHashFromApi :: Api.Hash Api.ScriptData -> GYDatumHash
datumHashFromApi = coerce

datumHashToApi :: GYDatumHash -> Api.Hash Api.ScriptData
datumHashToApi = coerce
