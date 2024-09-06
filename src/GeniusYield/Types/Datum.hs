{- |
Module      : GeniusYield.Types.Datum
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Datum (
  -- * Docspec setup
  -- $setup

  -- * Datum
  GYDatum,
  datumToApi',
  datumFromApi',
  datumToPlutus,
  datumToPlutus',
  datumFromPlutus,
  datumFromPlutus',
  datumFromPlutusData,
  unitDatum,
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

import Cardano.Api qualified as Api
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.Either.Combinators (mapLeft)
import Data.Text qualified as Txt
import Database.PostgreSQL.Simple qualified as PQ
import Database.PostgreSQL.Simple.FromField qualified as PQ (
  FromField (..),
  returnError,
 )
import Database.PostgreSQL.Simple.ToField qualified as PQ
import PlutusLedgerApi.V1.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx

import Cardano.Api.Shelley qualified as Api
import GeniusYield.Imports
import GeniusYield.Types.Ledger
import Web.HttpApiData qualified as Web

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Char8      as BS8
>>> import qualified Data.ByteString.Lazy       as BSL
>>> import qualified Web.HttpApiData            as Web
>>>
-}

{- | Datum

In the GY system we always include datums in transactions
so this simple type is sufficient.
-}
newtype GYDatum = GYDatum PlutusTx.BuiltinData
  deriving stock (Eq, Ord, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData)

{- | Convert a 'GYDatum' to 'Api.HashableScriptData' from Cardano Api.

__NOTE:__ This function is to be used only when generating for new outputs in a transaction as doing `datumFromApi'` followed by `datumToApi'` does not guarantee same low level CBOR representation of the high level data type.
-}
datumToApi' :: GYDatum -> Api.HashableScriptData
datumToApi' = datumToPlutus' >>> PlutusTx.builtinDataToData >>> Api.fromPlutusData >>> Api.unsafeHashableScriptData -- @unsafeHashableScriptData@ is fine here as there is no original datum bytes here, i.e. to say, this is a new datum which we are serialising and since we are serialising, we govern it.

-- | Get a 'GYDatum' from a Cardano Api 'Api.ScriptData'
datumFromApi' :: Api.HashableScriptData -> GYDatum
datumFromApi' = GYDatum . PlutusTx.dataToBuiltinData . Api.toPlutusData . Api.getScriptData

-- | Convert a 'GYDatum' to 'Plutus.Datum' from Plutus
datumToPlutus :: GYDatum -> Plutus.Datum
datumToPlutus = Plutus.Datum . datumToPlutus'

-- | Convert a 'GYDatum' to 'Plutus.BuiltinData' from Plutus
datumToPlutus' :: GYDatum -> PlutusTx.BuiltinData
datumToPlutus' (GYDatum x) = x

-- | Get a 'GYDatum' from a Plutus 'Plutus.Datum'
datumFromPlutus :: Plutus.Datum -> GYDatum
datumFromPlutus (Plutus.Datum d) = GYDatum d

-- | Get a 'GYDatum' from a Plutus 'Plutus.BuiltinData'
datumFromPlutus' :: PlutusTx.BuiltinData -> GYDatum
datumFromPlutus' = GYDatum

-- | Get a 'GYDatum' from any Plutus 'Plutus.ToData' type.
datumFromPlutusData :: PlutusTx.ToData a => a -> GYDatum
datumFromPlutusData = GYDatum . PlutusTx.toBuiltinData

{- | Unit datum

@'datumFromPlutusData' ()@.
-}
unitDatum :: GYDatum
unitDatum = datumFromPlutusData ()

-- | Returns the 'GYDatumHash' of the given 'GYDatum'
hashDatum :: GYDatum -> GYDatumHash
hashDatum = datumHashFromApi . Api.hashScriptDataBytes . datumToApi'

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

{- | Datums use cardano-api's detailed schema for JSON representation.

>>> Aeson.decode @GYDatum "{\"constructor\":0,\"fields\":[{\"int\":42},{\"list\":[{\"bytes\":\"\"}]}]}"
Just (GYDatum Constr 0 [I 42,List [B ""]])
-}
instance Aeson.FromJSON GYDatum where
  parseJSON = either (fail . show) (pure . datumFromApi') . Api.scriptDataFromJsonDetailedSchema

{- |

>>> BSL.putStr . Aeson.encode . datumFromPlutus' . PlutusTx.dataToBuiltinData $ PlutusTx.Constr 0 [ PlutusTx.I 42, PlutusTx.List [ PlutusTx.B "" ] ]
{"constructor":0,"fields":[{"int":42},{"list":[{"bytes":""}]}]}
-}
instance Aeson.ToJSON GYDatum where
  toJSON = Api.scriptDataToJsonDetailedSchema . datumToApi'

-------------------------------------------------------------------------------
-- DatumHash
-------------------------------------------------------------------------------

newtype GYDatumHash = GYDatumHash (Api.Hash Api.ScriptData)
  deriving stock Show
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

-- >>> Web.toUrlPiece (GYDatumHash "0103c27d58a7b32241bb7f03045fae8edc01dd2f2a70a349addc17f6536fde76")
-- "0103c27d58a7b32241bb7f03045fae8edc01dd2f2a70a349addc17f6536fde76"
--
instance Web.ToHttpApiData GYDatumHash where
  toUrlPiece = Api.serialiseToRawBytesHexText . datumHashToApi

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
datumHashFromBS =
  fmap datumHashFromApi
    . mapLeft (\e -> "RawBytes GYDatumHash decode fail: " <> show e)
    . Api.deserialiseFromRawBytes (Api.proxyToAsType @(Api.Hash Api.ScriptData) Proxy)

datumHashFromHexE :: String -> Either String GYDatumHash
datumHashFromHexE =
  Base16.decode . BS8.pack
    >=> datumHashFromBS

datumHashFromPlutus :: Plutus.DatumHash -> Either PlutusToCardanoError GYDatumHash
datumHashFromPlutus (Plutus.DatumHash h) =
  first
    (\t -> DeserialiseRawBytesError . Txt.pack $ "datumHashFromPlutus" ++ '.' : t)
    . datumHashFromBS
    $ PlutusTx.fromBuiltin h

unsafeDatumHashFromPlutus :: Plutus.DatumHash -> GYDatumHash
unsafeDatumHashFromPlutus =
  either (error . ("unsafeDatumHashFromPlutus: " ++) . show) id . datumHashFromPlutus

datumHashToPlutus :: GYDatumHash -> Plutus.DatumHash
datumHashToPlutus h = Plutus.DatumHash (PlutusTx.toBuiltin (Api.serialiseToRawBytes (datumHashToApi h)))

datumHashFromApi :: Api.Hash Api.ScriptData -> GYDatumHash
datumHashFromApi = coerce

datumHashToApi :: GYDatumHash -> Api.Hash Api.ScriptData
datumHashToApi = coerce
