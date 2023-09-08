{-|
Module     : GeniusYield.Providers.CardanoDbSync
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Functions in this module throw 'CardanoDbSyncException's.
They may also throw @postgresql-simple@'s 'PQ.SqlError'.
-}
module GeniusYield.Providers.CardanoDbSync (
    -- * cardano-db-sync connection
    CardanoDbSyncConn,
    openDbSyncConn,
    -- * Await Tx confirmed
    dbSyncAwaitTxConfirmed,
    -- * Slot number
    dbSyncSlotNumber,
    dbSyncWaitUntilSlot,
    -- * Datum lookup
    dbSyncLookupDatum,
    -- * Query UTxO
    dbSyncQueryUtxo,
    -- * Get parameters
    dbSyncGetParameters,
) where

import           Cardano.Slotting.Time                          (SystemStart (..),
                                                                 mkSlotLength)
import           Data.ByteString                                (ByteString)
import           Data.Maybe                                     (listToMaybe)
import           Data.Scientific                                (Scientific)
import           Data.SOP.BasicFunctors                         (K (..))
import           Data.SOP.Strict                                (NP (..))
import qualified Data.Text                                      as Txt
import           Data.Word                                      (Word64)
import           Type.Reflection                                (Typeable,
                                                                 typeRep)

import qualified Cardano.Api                                    as Api
import qualified Cardano.Api.Shelley                            as Api.S
import           Control.Concurrent                             (threadDelay)
import qualified Data.Aeson                                     as Aeson
import qualified Data.Aeson.Types                               as Aeson
import qualified Data.ByteString.Base16                         as Base16
import qualified Data.Map.Strict                                as Map
import qualified Data.Pool                                      as Pool
import qualified Data.Scientific                                as Sci
import qualified Data.Set                                       as Set
import qualified Data.Time                                      as Time
import qualified Database.PostgreSQL.Simple                     as PQ
import qualified Database.PostgreSQL.Simple.FromField           as PQ (Conversion,
                                                                       FromField (..),
                                                                       returnError)
import qualified Database.PostgreSQL.Simple.Newtypes            as PQ
import qualified Database.PostgreSQL.Simple.ToField             as PQ

import qualified Data.SOP.Counting                              as Ouroboros
import qualified Ouroboros.Consensus.Block.Abstract             as Ouroboros
import qualified Ouroboros.Consensus.Cardano.Block              as Ouroboros
import qualified Ouroboros.Consensus.Config.SecurityParam       as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History.EraParams as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History.Qry       as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History.Summary   as Ouroboros


import           Database.PostgreSQL.Simple                     (ResultError (UnexpectedNull))
import           GeniusYield.Imports
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types


-- | Connection to cardano-db-sync database.
newtype CardanoDbSyncConn = Conn (Pool.Pool PQ.Connection)

-- | Type alias for the for the result returned when querying for UTxO.
type QueryUtxo = (RawBytes Api.TxId, Integer, GYAddressBech32, Integer, PQ.Aeson MA,
     Maybe GYDatumHash, Maybe Integer, Maybe Integer)

-- | Type alias for the for the result obtained on operations above 'QueryUtxo'.
type QueryUtxo' = (RawBytes Api.TxId, Integer, GYAddressBech32, Integer, PQ.Aeson MA,
     Maybe GYDatumHash, Maybe GYDatum, Maybe (Some GYScript))

openDbSyncConn :: PQ.ConnectInfo -> IO CardanoDbSyncConn
openDbSyncConn ci = coerce $ Pool.newPool (Pool.setNumStripes (Just 1) $ Pool.defaultPoolConfig (PQ.connect ci) PQ.close 30 5)

newtype CardanoDbSyncException = CardanoDbSyncException String
  deriving stock (Show)
  deriving anyclass (Exception)

-------------------------------------------------------------------------------
-- Await tx confirmation
-------------------------------------------------------------------------------

-- | Awaits for the confirmation of a given 'GYTxId'
dbSyncAwaitTxConfirmed :: CardanoDbSyncConn -> GYAwaitTx
dbSyncAwaitTxConfirmed (Conn pool) p@GYAwaitTxParameters{..} txId =
    Pool.withResource pool $ dbSyncAwaitTx 0
  where
    dbSyncAwaitTx :: Int -> PQ.Connection -> IO ()
    dbSyncAwaitTx attempt _ | maxAttempts <= attempt = throwIO $ GYAwaitTxException p
    dbSyncAwaitTx attempt conn = do
        res <- PQ.query conn "SELECT (mtable.max - block_no) as confirmations FROM block, ( SELECT MAX(block_no) as max FROM block ) as mtable, ( SELECT block_id, hash FROM tx WHERE hash = ? ) AS b WHERE block.id = b.block_id;" (PQ.Only txId)
        case res of
            [PQ.Only (blockConfirmations :: Int)] ->
                when (blockConfirmations < confirmations) $
                threadDelay checkInterval >> dbSyncAwaitTx (attempt + 1) conn
            [] | attempt + 1 == maxAttempts -> throwIO $ GYAwaitTxException p
            [] -> threadDelay checkInterval >> dbSyncAwaitTx (attempt + 1) conn
            _anyOtherMatch -> throwIO $ CardanoDbSyncException "dbSyncAwaitTxConfirmed: zero or multiple SQL results"

-------------------------------------------------------------------------------
-- Slot number
-------------------------------------------------------------------------------

dbSyncSlotNumber :: CardanoDbSyncConn -> IO GYSlot
dbSyncSlotNumber (Conn pool) = Pool.withResource pool $ \conn -> do
    res <- PQ.query_ conn "select max(slot_no) from block;"
    case res of
        [PQ.Only n] -> maybe
            (throwIO $ CardanoDbSyncException "dbSyncSlotNumber: overflow or underflow ")
            pure
            $ slotFromInteger n
        _           -> throwIO $ CardanoDbSyncException "dbSyncSlotNumber: zero or multiple SQL results"

-- | Wait until 'CardanoDbSyncConn' has processed a given slot.
dbSyncWaitUntilSlot :: CardanoDbSyncConn -> GYSlot -> IO GYSlot
dbSyncWaitUntilSlot conn = gyWaitUntilSlotDefault (dbSyncSlotNumber conn)

-------------------------------------------------------------------------------
-- Datum lookup provider
-------------------------------------------------------------------------------

dbSyncLookupDatum :: CardanoDbSyncConn -> GYLookupDatum
dbSyncLookupDatum (Conn pool) dh = Pool.withResource pool $ \conn -> do
    res <- PQ.query conn "SELECT value FROM datum WHERE hash = ?" (PQ.Only dh)
    case res of
        [PQ.Only (PQ.Aeson (SD sd))] -> return $ Just $ datumFromApi' sd
        _anyOtherMatch               -> return Nothing

newtype SD = SD Api.HashableScriptData
  deriving Show

instance FromJSON SD where
    parseJSON v = case Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v of
        Right x -> return (SD x)
        Left e  -> fail $ show e

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

dbSyncQueryUtxo :: CardanoDbSyncConn -> GYQueryUTxO
dbSyncQueryUtxo conn = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefs'           = dbSyncQueryUtxosAtTxOutRefs conn
    , gyQueryUtxosAtTxOutRefsWithDatums' = Nothing  -- Will use the default implementation.
    , gyQueryUtxoAtTxOutRef'             = dbSyncQueryUtxoAtTxOutRef conn
    , gyQueryUtxoRefsAtAddress'          = gyQueryUtxoRefsAtAddressDefault $ dbSyncQueryUtxosAtAddress conn
    , gyQueryUtxosAtAddresses'           = dbSyncQueryUtxosAtAddresses conn
    , gyQueryUtxosAtAddressesWithDatums' = Nothing  -- Will use the default implementation.
    , gyQueryUtxosAtPaymentCredential'   = Nothing
    }

gyDatumFromId :: PQ.Connection -> Integer -> IO (Maybe GYDatum)
gyDatumFromId conn inlineDatumId = do
  datumValue <- PQ.query conn "SELECT value FROM datum WHERE id = ?;" (PQ.Only inlineDatumId)
  case datumValue of
    [PQ.Only (PQ.Aeson (SD sd))] -> return $ Just $ datumFromApi' sd
    _anyOtherMatch               -> return Nothing

data ScriptType = Unsupported | STPlutusV1 | STPlutusV2

instance PQ.FromField ScriptType where
  fromField f = makeScriptType
    where
      makeScriptType :: Maybe ByteString -> PQ.Conversion ScriptType
      makeScriptType (Just "plutusV1") = return STPlutusV1
      makeScriptType (Just "plutusV2") = return STPlutusV2
      makeScriptType (Just _)          = return Unsupported
      makeScriptType Nothing           = PQ.returnError UnexpectedNull f "Empty script type"

gyScriptFromId :: PQ.Connection -> Integer -> IO (Maybe (Some GYScript))
gyScriptFromId conn referenceScriptId = do
  res <- PQ.query conn "SELECT type, bytes FROM script WHERE id = ?;" (PQ.Only referenceScriptId)
  case res of
    [(STPlutusV1, Just (cbor :: ByteString))] -> return $ Some <$> scriptFromCBOR'  @'PlutusV1 (Base16.encode cbor)
    [(STPlutusV2, Just cbor)]                 -> return $ Some <$> scriptFromCBOR'  @'PlutusV2 (Base16.encode cbor)
    _anyOtherMatch                            -> return Nothing

modifyRes:: PQ.Connection -> [QueryUtxo] -> IO [QueryUtxo']
modifyRes conn = traverse
    (\(RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, mInlineDatumId, mReferenceScriptId) -> do
      inlineDatum <- maybe (return Nothing) (gyDatumFromId conn) mInlineDatumId
      referenceScript <- maybe (return Nothing) (gyScriptFromId conn) mReferenceScriptId
      return (RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, inlineDatum, referenceScript)
    )

-- SELECT tx_id, tx.hash, index, address, value, data_hash, linline_datum_id, reference_script_id FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND address = 'addr_test1vrjt027trymct9tx8zmj4sk9hhdfsmhz7tl3u3df928rz3sfpaxk0';
dbSyncQueryUtxosAtAddress :: CardanoDbSyncConn -> GYAddress -> IO GYUTxOs
dbSyncQueryUtxosAtAddress (Conn pool) addr = Pool.withResource pool $ \conn -> do
    res <- PQ.query conn
        "SELECT tx.hash, index, address, value :: bigint, to_json(array(select json_build_object('policy', encode(policy, 'hex'), 'name', encode(name, 'hex'), 'quantity', quantity) from ma_tx_out, multi_asset where multi_asset.id = ma_tx_out.ident AND tx_out_id = tx_out.id)) as assets, data_hash, inline_datum_id, reference_script_id FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND NOT EXISTS (SELECT FROM tx_in WHERE tx_out_id = tx.id AND tx_out_index = index) AND address = ?;"
        (PQ.Only (addressToBech32 addr))
    res' <- modifyRes conn res
    let utxos :: [GYUTxO]
        utxos =
            [ GYUTxO
                { utxoRef       = txOutRefFromApi (Api.TxIn txid (Api.TxIx (fromInteger txix)))
                , utxoAddress   = addressFromBech32 addr32
                , utxoValue     = valueFromLovelace lovelace <> valueFromList value
                , utxoOutDatum  = maybe (maybe GYOutDatumNone GYOutDatumHash mdh) GYOutDatumInline inlineDatum
                , utxoRefScript = referenceScript
                }
            | (RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, inlineDatum, referenceScript) <- res'
            ]

    return $ utxosFromList utxos


-- SELECT tx_id, tx.hash, index, address, value, data_hash FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND address  in ('addr_test1vrjt027trymct9tx8zmj4sk9hhdfsmhz7tl3u3df928rz3sfpaxk0');
dbSyncQueryUtxosAtAddresses :: CardanoDbSyncConn -> [GYAddress] -> IO GYUTxOs
dbSyncQueryUtxosAtAddresses (Conn pool) addrs = Pool.withResource pool $ \conn -> do
    let bech32Addrs = map addressToBech32 addrs
    res <- PQ.query conn
        "SELECT tx.hash, index, address, value :: bigint, to_json(array(select json_build_object('policy', encode(policy, 'hex'), 'name', encode(name, 'hex'), 'quantity', quantity) from ma_tx_out, multi_asset where multi_asset.id = ma_tx_out.ident AND tx_out_id = tx_out.id)) as assets, data_hash, inline_datum_id, reference_script_id FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND NOT EXISTS (SELECT FROM tx_in WHERE tx_out_id = tx.id AND tx_out_index = index) AND address in ?;"
        (PQ.Only $ PQ.In bech32Addrs)
    res' <- modifyRes conn res
    let utxos :: [GYUTxO]
        utxos =
            [ GYUTxO
                { utxoRef       = txOutRefFromApi (Api.TxIn txid (Api.TxIx (fromInteger txix)))
                , utxoAddress   = addressFromBech32 addr32
                , utxoValue     = valueFromLovelace lovelace <> valueFromList value
                , utxoOutDatum  = maybe (maybe GYOutDatumNone GYOutDatumHash mdh) GYOutDatumInline inlineDatum
                , utxoRefScript = referenceScript
                }
            | (RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, inlineDatum, referenceScript) <- res'
            ]

    return $ utxosFromList utxos

dbSyncQueryUtxosAtTxOutRefs :: CardanoDbSyncConn -> [GYTxOutRef] -> IO GYUTxOs
dbSyncQueryUtxosAtTxOutRefs (Conn pool) orefs = Pool.withResource pool $ \conn -> do
    -- we select all outputs of a transactions
    -- ... and later filter them to include only wanted outputs (= indexes)
    let txids :: [RawBytes Api.TxId]
        txids =
            [ RawBytes txid'
            | oref <- orefs
            , let Api.TxIn txid' _ = txOutRefToApi oref
            ]

    res <- PQ.query conn
        "SELECT tx.hash, index, address, value :: bigint, to_json(array(select json_build_object('policy', encode(policy, 'hex'), 'name', encode(name, 'hex'), 'quantity', quantity) from ma_tx_out, multi_asset where multi_asset.id = ma_tx_out.ident AND tx_out_id = tx_out.id)) as assets, data_hash, inline_datum_id, reference_script_id FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND NOT EXISTS (SELECT FROM tx_in WHERE tx_out_id = tx.id AND tx_out_index = index) AND tx.hash IN ?;"
        (PQ.Only (PQ.In txids))
    res' <- modifyRes conn res
    let utxos :: [GYUTxO]
        utxos =
            [ GYUTxO
                { utxoRef       = ref
                , utxoAddress   = addressFromBech32 addr32
                , utxoValue     = valueFromLovelace lovelace <> valueFromList value
                , utxoOutDatum  = maybe (maybe GYOutDatumNone GYOutDatumHash mdh) GYOutDatumInline inlineDatum
                , utxoRefScript = referenceScript
                }
            | (RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, inlineDatum, referenceScript) <- res'
            , let ref = txOutRefFromApi (Api.TxIn txid (Api.TxIx (fromInteger txix)))
            , ref `elem` orefs
            ]

    return $ utxosFromList utxos

dbSyncQueryUtxoAtTxOutRef :: CardanoDbSyncConn -> GYTxOutRef -> IO (Maybe GYUTxO)
dbSyncQueryUtxoAtTxOutRef (Conn pool) oref = Pool.withResource pool $ \conn -> do
    let Api.TxIn txid' (Api.TxIx txix') = txOutRefToApi oref

    res <- PQ.query conn
        "SELECT tx.hash, index, address, value :: bigint, to_json(array(select json_build_object('policy', encode(policy, 'hex'), 'name', encode(name, 'hex'), 'quantity', quantity) from ma_tx_out, multi_asset where multi_asset.id = ma_tx_out.ident AND tx_out_id = tx_out.id)) as assets, data_hash, inline_datum_id, reference_script_id FROM tx_out, tx WHERE tx_out.tx_id = tx.id AND NOT EXISTS (SELECT FROM tx_in WHERE tx_out_id = tx.id AND tx_out_index = index) AND tx.hash = ? AND index = ?;"
        (RawBytes txid', toInteger txix')
    res' <- modifyRes conn res
    let utxos :: [GYUTxO]
        utxos =
            [ GYUTxO
                { utxoRef       = txOutRefFromApi (Api.TxIn txid (Api.TxIx (fromInteger txix)))
                , utxoAddress   = addressFromBech32 addr32
                , utxoValue     = valueFromLovelace lovelace <> valueFromList value
                , utxoOutDatum  = maybe (maybe GYOutDatumNone GYOutDatumHash mdh) GYOutDatumInline inlineDatum
                , utxoRefScript = referenceScript
                }
            | (RawBytes txid, txix, addr32, lovelace, PQ.Aeson (MA value), mdh, inlineDatum, referenceScript) <- res'
            ]

    return $ listToMaybe utxos

-- We query native assets as subquery per each row result,
-- and returns that as JSON
newtype MA = MA [(GYAssetClass, Integer)] deriving Show

instance FromJSON MA where
    parseJSON = Aeson.withArray "Assets" $ \xs ->
        fmap MA $ forM (toList xs) $ Aeson.withObject "Asset" $ \obj -> do
            Hex pn' <- obj Aeson..: "policy"
            Hex tn' <- obj Aeson..: "name"
            amt     <- obj Aeson..: "quantity"

            pn     <- either (\e -> fail $ "invalid policy: " <> show e)    return $ Api.deserialiseFromRawBytes Api.AsPolicyId pn'
            tn     <- maybe (fail "invalid tokenname") return $ tokenNameFromBS tn'
            return (GYToken (mintingPolicyIdFromApi pn) tn, amt)

newtype Hex = Hex ByteString

instance FromJSON Hex where
    parseJSON = Aeson.withText "Hex" $ \t ->
        case Base16.decode (encodeUtf8 t) of
            Right bs -> return (Hex bs)
            Left _   -> fail "Not a hex encoding"

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

dbSyncGetParameters :: CardanoDbSyncConn -> GYGetParameters
dbSyncGetParameters info = GYGetParameters
    { gyGetProtocolParameters' = dbSyncGetProtocolParameters info
    , gyGetStakePools'         = dbSyncGetStakePools info
    , gyGetSystemStart'        = dbSyncGetSystemStart info
    , gyGetEraHistory'         = dbSyncGetEraHistory info
    , gyGetSlotConfig'         = either
                                    (throwIO . GYConversionException . GYEraSummariesToSlotConfigError . Txt.pack)
                                    pure
                                    =<< (makeSlotConfig <$> dbSyncGetSystemStart info <*> dbSyncGetEraHistory info)
    }

dbSyncGetSystemStart :: CardanoDbSyncConn -> IO SystemStart
dbSyncGetSystemStart (Conn pool) = Pool.withResource pool $ \conn -> do
    res <- PQ.query_ conn "SELECT start_time FROM meta LIMIT 1;"
    case res of
        [PQ.Only time] -> return $ SystemStart $ Time.localTimeToUTC Time.utc time
        _anyOtherMatch -> throwIO $ CardanoDbSyncException "dbSyncGetSystemStart: zero or multipl SQL results"

dbSyncGetStakePools :: CardanoDbSyncConn -> IO (Set.Set Api.S.PoolId)
dbSyncGetStakePools (Conn pool) = Pool.withResource pool $ \conn -> do
    res <- PQ.query_ conn "SELECT hash_raw FROM pool_hash;"
    return $ Set.fromList $ coerce (res :: [PQ.Only (RawBytes Api.S.PoolId)])

dbSyncGetProtocolParameters :: CardanoDbSyncConn -> IO Api.S.ProtocolParameters
dbSyncGetProtocolParameters (Conn pool) = Pool.withResource pool $ \conn -> do
    res <- PQ.query_ conn "SELECT protocol_major, protocol_minor, decentralisation, max_bh_size, max_block_size, max_tx_size, min_fee_b, min_fee_a, key_deposit, pool_deposit, min_pool_cost, max_epoch, optimal_pool_count, influence,  monetary_expand_rate, treasury_growth_rate, min_utxo_value, price_step, price_mem, max_tx_ex_steps, max_tx_ex_mem,  max_block_ex_steps, max_block_ex_mem, max_val_size, collateral_percent, max_collateral_inputs, costs FROM epoch_param, cost_model WHERE epoch_param.cost_model_id = cost_model.id ORDER BY epoch_no DESC LIMIT 1;"
    case res of
        [PP {..}] -> return Api.S.ProtocolParameters
            { protocolParamProtocolVersion     = (unN ppMaj, unN ppMin)
            , protocolParamDecentralization    = Just $ toRational ppDecentr
            , protocolParamExtraPraosEntropy   = Nothing
            , protocolParamMaxBlockHeaderSize  = unN ppBHSize
            , protocolParamMaxBlockBodySize    = unN ppBlockSize
            , protocolParamMaxTxSize           = unN ppMaxTxSize
            , protocolParamTxFeeFixed          = Api.Lovelace $ toInteger $ unN ppTxFeeFixed
            , protocolParamTxFeePerByte        = Api.Lovelace $ toInteger $ unN ppTxFeePerByte
            , protocolParamMinUTxOValue        = Nothing
            , protocolParamStakeAddressDeposit = Api.Lovelace (unSI ppStakeAddressDeposit)
            , protocolParamStakePoolDeposit    = Api.Lovelace (unSI ppStakePoolDeposit)
            , protocolParamMinPoolCost         = Api.Lovelace (unSI ppMinPoolCost)
            , protocolParamPoolRetireMaxEpoch  = Api.EpochNo (unW64 ppPoolRetireMaxEpoch)
            , protocolParamStakePoolTargetNum  = unN ppStakePoolTargetNum
            , protocolParamPoolPledgeInfluence = toRational ppPoolPledgeInfluence
            , protocolParamMonetaryExpansion   = toRational ppMonetaryExpansion
            , protocolParamTreasuryCut         = toRational ppTreasuryCut
            , protocolParamUTxOCostPerWord     = Nothing
            , protocolParamPrices              = Just (Api.S.ExecutionUnitPrices (toRational ppPriceStep) (toRational ppPriceMemory))
            , protocolParamMaxTxExUnits        = Just (Api.ExecutionUnits (unSN ppMaxTxExSteps) (unSN ppMaxTxExMemory))
            , protocolParamMaxBlockExUnits     = Just (Api.ExecutionUnits (unSN ppMaxBlockExSteps) (unSN ppMaxBlockExMemory))
            , protocolParamMaxValueSize        = Just (unSN ppMaxValueSize)
            , protocolParamCollateralPercent   = Just (unSN ppMaxCollateralPercent)
            , protocolParamMaxCollateralInputs = Just (unSN ppMaxCollateralInputs)
            , protocolParamCostModels          =
                let
                  ppCosts' :: Map PlutusScriptVersion CostModel = coerce ppCosts
                  ppCosts'' :: Map PlutusScriptVersion Api.CostModel = fmap (coerce . Map.elems) ppCosts'
                in Map.mapKeys coerce ppCosts''
            , protocolParamUTxOCostPerByte     = Just $ Api.Lovelace (unSI ppUTxOCostPerWord `div` 8)
            }

        _ -> throwIO $ CardanoDbSyncException "dbSyncGetProtocolParameters: zero or multiple SQL results"

type CostModel = (Map Text Integer)

data PP = PP
    { ppMaj                  :: !N
    , ppMin                  :: !N
    , ppDecentr              :: !Scientific
    , ppBHSize               :: !N
    , ppBlockSize            :: !N
    , ppMaxTxSize            :: !N
    , ppTxFeeFixed           :: !N
    , ppTxFeePerByte         :: !N
    , ppStakeAddressDeposit  :: !SI
    , ppStakePoolDeposit     :: !SI
    , ppMinPoolCost          :: !SI
    , ppPoolRetireMaxEpoch   :: !W64
    , ppStakePoolTargetNum   :: !N
    , ppPoolPledgeInfluence  :: !Scientific
    , ppMonetaryExpansion    :: !Scientific
    , ppTreasuryCut          :: !Scientific
    , ppUTxOCostPerWord      :: !SI
    , ppPriceStep            :: !Scientific
    , ppPriceMemory          :: !Scientific
    , ppMaxTxExSteps         :: !SN
    , ppMaxTxExMemory        :: !SN
    , ppMaxBlockExSteps      :: !SN
    , ppMaxBlockExMemory     :: !SN
    , ppMaxValueSize         :: !SN
    , ppMaxCollateralPercent :: !SN
    , ppMaxCollateralInputs  :: !SN
    , ppCosts                :: !(PQ.Aeson (Map PlutusScriptVersion CostModel))
    }
  deriving (Generic)

instance PQ.FromRow PP

-- for some reason, cardano-db-sync uses difference encoding then there is in cardano-api (or was)
newtype PlutusScriptVersion = PlutusScriptVersion Api.S.AnyPlutusScriptVersion
  deriving (Eq, Ord)

parsePlutusScriptVersion :: Text -> Aeson.Parser PlutusScriptVersion
parsePlutusScriptVersion t =
  case t of
    "PlutusV1" -> return (PlutusScriptVersion (Api.AnyPlutusScriptVersion Api.PlutusScriptV1))
    -- "PlutusV2" -> return (PlutusScriptVersion (Api.AnyPlutusScriptVersion Api.PlutusScriptV2))
    _          -> fail "Expected PlutusScriptV1 or PlutusScriptV2"

instance Aeson.FromJSONKey PlutusScriptVersion where
    fromJSONKey     = Aeson.FromJSONKeyTextParser parsePlutusScriptVersion
    fromJSONKeyList = Aeson.FromJSONKeyTextParser $ \_ -> fail "fromJSONKeyList @PlutusScriptVersion Not needed"


dbSyncGetEraHistory :: CardanoDbSyncConn -> IO (Api.EraHistory Api.CardanoMode)
dbSyncGetEraHistory (Conn pool) =  Pool.withResource pool $ \conn -> do
    res <- PQ.query_ conn "SELECT network_name FROM meta LIMIT 1;"
    case res of
        [PQ.Only network_name]
            | network_name == ("privatenet" :: Text) -> return privatenetEraHistory
            -- TODO: add testnet and mainnets #35 (https://github.com/geniusyield/atlas/issues/35)
            | otherwise -> throwIO $ CardanoDbSyncException $ "dbSyncGetEraHistory: Unknown network: " ++ show network_name

        _ -> throwIO $ CardanoDbSyncException "dbSyncGetEraHistory: zero or multiple SQL results"
  where
    -- WARNING: this configuration was found by trial&error.
    -- It would help if @Show Interpeter@ printed the actual structure,
    -- as recover-rtti doesn't show unpacked fields.
    privatenetEraHistory :: Api.S.EraHistory Api.S.CardanoMode
    privatenetEraHistory = Api.S.EraHistory Api.CardanoMode
        $ Ouroboros.unsafeExtendSafeZone
        $ Ouroboros.mkInterpreter
        $ Ouroboros.summarize
            Ouroboros.Origin
            shape
            transitions
      where
        shape :: Ouroboros.Shape (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
        shape = Ouroboros.Shape $ Ouroboros.Exactly $
            K (Ouroboros.defaultEraParams (Ouroboros.SecurityParam 10) (mkSlotLength 1)) :*
            K (mkEraParams 500 0.1 300) :*
            K (mkEraParams 500 0.1 300) :*
            K (mkEraParams 500 0.1 300) :*
            K (mkEraParams 500 0.1 300) :*
            K (mkEraParams 500 0.1 300) :*
            K (mkEraParams 500 0.1 300) :*
            Nil

        -- privatenet eras don't use defaultEraParams
        mkEraParams :: Word64 -> Time.NominalDiffTime -> Word64 -> Ouroboros.EraParams
        mkEraParams s l z = Ouroboros.EraParams
            { eraEpochSize  = Ouroboros.EpochSize s
            , eraSlotLength = mkSlotLength l
            , eraSafeZone   = Ouroboros.StandardSafeZone z
            }

        transitions :: Ouroboros.Transitions (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
        transitions = Ouroboros.Transitions $
            Ouroboros.AtMostCons (Ouroboros.EpochNo 0) $
            Ouroboros.AtMostCons (Ouroboros.EpochNo 0) $
            Ouroboros.AtMostCons (Ouroboros.EpochNo 0) $
            Ouroboros.AtMostCons (Ouroboros.EpochNo 0)
            Ouroboros.AtMostNil

-------------------------------------------------------------------------------
-- General helpers
-------------------------------------------------------------------------------

newtype RawBytes a = RawBytes a

instance (Api.SerialiseAsRawBytes a, Api.HasTypeProxy a, Typeable a) => PQ.FromField (RawBytes a) where
    fromField f bs = do
        PQ.Binary bs' <- PQ.fromField f bs
        case Api.deserialiseFromRawBytes (Api.proxyToAsType (Proxy @a)) bs' of
            Right x -> return (RawBytes x)
            Left e  -> PQ.returnError PQ.ConversionFailed f $ "does not unserialise: " <> show (typeRep @a) <> ", error: " <> show e

instance Api.SerialiseAsRawBytes a => PQ.ToField (RawBytes a) where
    toField (RawBytes x) = PQ.toField (PQ.Binary (Api.serialiseToRawBytes x))

newtype N = N { unN :: Natural }

instance PQ.FromField N where
    fromField f bs = do
        i <- PQ.fromField f bs
        if i >= 0
        then return (N (fromInteger i))
        else PQ.returnError PQ.ConversionFailed f "negative natural"

-- scientific integral
newtype SI = SI { unSI :: Integer }

instance PQ.FromField SI where
    fromField f bs = do
        s <- PQ.fromField f bs
        case Sci.floatingOrInteger s :: Either Double Integer of
            Right i -> return (SI i)
            Left _  -> PQ.returnError PQ.ConversionFailed f $ "decimal when integer numeric expected " ++ show s

-- scientific natural
newtype SN = SN { unSN :: Natural }

instance PQ.FromField SN where
    fromField f bs = do
        s <- PQ.fromField f bs
        case Sci.floatingOrInteger s :: Either Double Integer of
            Right i | i >= 0 -> return (SN (fromInteger i))
            _                -> PQ.returnError PQ.ConversionFailed f $ "decimal when integer numeric expected " ++ show s

newtype W64 = W64 { unW64 :: Word64 }

instance PQ.FromField W64 where
    fromField f bs = do
        s <- PQ.fromField f bs
        case Sci.toBoundedInteger s of
            Just i  -> return (W64 i)
            Nothing -> PQ.returnError PQ.ConversionFailed f $ "not W64: " ++ show s
