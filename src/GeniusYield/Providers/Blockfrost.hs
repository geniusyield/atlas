module GeniusYield.Providers.Blockfrost
    ( Blockfrost.Project
    , blockfrostProtocolParams
    , blockfrostStakePools
    , blockfrostSystemStart
    , blockfrostEraHistory
    , blockfrostQueryUtxo
    , blockfrostLookupDatum
    , blockfrostSlotActions
    , blockfrostGetCurrentSlot
    , blockfrostSubmitTx
    , networkIdToProject
    ) where

import qualified Blockfrost.Client                    as Blockfrost
import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Slotting.Slot                as CSlot
import qualified Cardano.Slotting.Time                as CTime
import           Control.Monad                        ((<=<))
import           Control.Monad.Except                 (throwError)
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Base16               as BS16
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Either.Combinators              (maybeToRight)
import           Data.Foldable                        (fold)
import           Data.Functor                         ((<&>))
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified Data.Time.Clock.POSIX                as Time
import qualified Money
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import qualified PlutusTx.Builtins                    as Plutus
import qualified Web.HttpApiData                      as Web

import           GeniusYield.Imports
import           GeniusYield.Providers.Common
import           GeniusYield.Types

data BlockfrostProviderException
    = BlpvApiError !Text !Blockfrost.BlockfrostError
    | BlpvDeserializeFailure !Text !SomeDeserializeError            -- ^ This error should never actually happen (unless there's a bug).
    | BlpvNoSlotInfo !Blockfrost.BlockHash
    | BlpvUnsupportedOperation !Text
    | BlpvIncorrectEraHistoryLength ![Blockfrost.NetworkEraSummary]
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

handleBlockfrostError :: Text -> Either Blockfrost.BlockfrostError a -> IO a
handleBlockfrostError locationInfo = either (throwIO . BlpvApiError locationInfo . silenceHeadersBlockfrostClientError) pure
  where
    silenceHeadersBlockfrostClientError (Blockfrost.ServantClientError e) = Blockfrost.ServantClientError $ silenceHeadersClientError e
    silenceHeadersBlockfrostClientError other                             = other

lovelacesToInteger :: Blockfrost.Lovelaces -> Integer
lovelacesToInteger = fromIntegral

gyAddressToBlockfrost :: GYAddress -> Blockfrost.Address
gyAddressToBlockfrost = Blockfrost.mkAddress . addressToText

-- | Creates a 'GYValue' from a 'Blockfrost.Amount', may fail parsing blockfrost returned asset class.
amountToValue :: Blockfrost.Amount -> Either Text GYValue
amountToValue (Blockfrost.AdaAmount lovelaces) = pure . valueSingleton GYLovelace $ lovelacesToInteger lovelaces
amountToValue (Blockfrost.AssetAmount sdiscr) = do
    cs <- Web.parseUrlPiece csPart
    tkName <- Web.parseUrlPiece tkNamePart
    pure . valueSingleton (GYToken cs tkName) $ Money.someDiscreteAmount sdiscr
  where
    csAndTkname = Money.someDiscreteCurrency sdiscr
    -- Blockfrost uses no separator between CS and TkName.
    (csPart, tkNamePart) = Text.splitAt 56 csAndTkname

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

blockfrostSubmitTx :: Blockfrost.Project -> GYSubmitTx
blockfrostSubmitTx proj tx = do
    txId <- handleBlockfrostError locationIdent <=< Blockfrost.runBlockfrost proj
        . Blockfrost.submitTx
        . Blockfrost.CBORString
        . LBS.fromStrict
        . Api.serialiseToCBOR
        $ txToApi tx
    either
        (throwIO . BlpvDeserializeFailure locationIdent . DeserializeErrorHex . Text.pack)
        pure
        . txIdFromHexE . Text.unpack $ Blockfrost.unTxHash txId
  where
    locationIdent = "SubmitTx"

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

blockfrostSlotActions :: Blockfrost.Project -> GYSlotActions
blockfrostSlotActions proj = GYSlotActions
    { gyGetCurrentSlot'   = getCurrentSlot
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault getCurrentSlot
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault getCurrentSlot
    }
  where
    getCurrentSlot = blockfrostGetCurrentSlot proj

blockfrostGetCurrentSlot :: Blockfrost.Project -> IO GYSlot
blockfrostGetCurrentSlot proj = do
    Blockfrost.Block {_blockSlot=slotMaybe, _blockHash=hash} <-
        Blockfrost.runBlockfrost proj Blockfrost.getLatestBlock >>= handleBlockfrostError "Slot"
    case slotMaybe of
        Nothing -> throwIO $ BlpvNoSlotInfo hash
        Just x  -> pure . slotFromApi . Api.SlotNo . fromInteger $ Blockfrost.unSlot x

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

blockfrostQueryUtxo :: Blockfrost.Project -> GYQueryUTxO
blockfrostQueryUtxo proj = GYQueryUTxO
    { gyQueryUtxosAtTxOutRefs'           = blockfrostUtxosAtTxOutRefs proj
    , gyQueryUtxoAtTxOutRef'             = blockfrostUtxosAtTxOutRef proj
    , gyQueryUtxoRefsAtAddress'          = gyQueryUtxoRefsAtAddressDefault $ blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtAddresses'           = gyQueryUtxoAtAddressesDefault $ blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtAddressesWithDatums' = Nothing  -- Will use the default implementation.
    }

blockfrostUtxosAtAddress :: Blockfrost.Project -> GYAddress -> IO GYUTxOs
blockfrostUtxosAtAddress proj addr = do
    {- 'Blockfrost.getAddressUtxos' doesn't return all utxos at that address, only the first 100 or so.
    Have to handle paging manually for all. -}
    addrUtxos  <- handler <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged ->
            Blockfrost.getAddressUtxos' (gyAddressToBlockfrost addr) paged Blockfrost.Ascending
    addrUtxos' <- mapM (\x -> lookupScriptHashIO proj (Blockfrost._addressUtxoReferenceScriptHash x) >>= \mrs -> return (x, mrs)) addrUtxos
    case traverse transformUtxo addrUtxos' of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right x  -> pure $ utxosFromList x
  where
    transformUtxo :: (Blockfrost.AddressUtxo, Maybe (Some GYScript)) -> Either SomeDeserializeError GYUTxO
    transformUtxo (Blockfrost.AddressUtxo {..}, ms) = do
        val  <- bimap DeserializeErrorAssetClass fold $ traverse amountToValue _addressUtxoAmount
        ref  <- first DeserializeErrorHex . Web.parseUrlPiece
                    $ Blockfrost.unTxHash _addressUtxoTxHash <> Text.pack ('#' : show _addressUtxoOutputIndex)
        d    <- outDatumFromBlockfrost _addressUtxoDataHash _addressUtxoInlineDatum
        pure GYUTxO
            { utxoRef       = ref
            , utxoAddress   = addr
            , utxoValue     = val
            , utxoOutDatum  = d
            , utxoRefScript = ms
            }
    locationIdent = "AddressUtxos"
    -- This particular error is fine in this case, we can just return empty list.
    handler (Left Blockfrost.BlockfrostNotFound) = pure []
    handler other                                = handleBlockfrostError locationIdent other

blockfrostUtxosAtTxOutRef :: Blockfrost.Project -> GYTxOutRef -> IO (Maybe GYUTxO)
blockfrostUtxosAtTxOutRef proj ref = do
    let (Api.serialiseToRawBytesHexText -> txId, utxoIdx) = first txIdToApi $ txOutRefToTuple ref
    -- Get all UTxO outputs created by the tx id within the given tx out ref.
    txOutMaybe <- handler
        <=< Blockfrost.runBlockfrost proj . Blockfrost.getTxUtxos $ Blockfrost.TxHash txId
    -- Get the specific UTxO for the given index.
    let res = txOutMaybe >>=
            find
                (\(Blockfrost._utxoOutputOutputIndex -> idx) -> idx == toInteger utxoIdx)
                . Blockfrost._transactionUtxosOutputs
    case res of
      Nothing -> pure Nothing
      Just Blockfrost.UtxoOutput {..} -> do
        val  <- either
                    (throwIO . BlpvDeserializeFailure locationIdent . DeserializeErrorAssetClass)
                    (pure . fold)
                    $ traverse amountToValue _utxoOutputAmount
        addr <- maybe
                    (throwIO $ BlpvDeserializeFailure locationIdent DeserializeErrorAddress)
                    pure
                    . addressFromTextMaybe $ Blockfrost.unAddress _utxoOutputAddress
        d    <- either
                    (throwIO . BlpvDeserializeFailure locationIdent)
                    return
                    $ outDatumFromBlockfrost _utxoOutputDataHash _utxoOutputInlineDatum
        ms   <- lookupScriptHashIO proj _utxoOutputReferenceScriptHash
        pure $ Just GYUTxO
            { utxoRef       = ref
            , utxoAddress   = addr
            , utxoValue     = val
            , utxoOutDatum  = d
            , utxoRefScript = ms
            }
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError locationIdent $ Just <$> other
    locationIdent = "TxUtxos(single)"

blockfrostUtxosAtTxOutRefs :: Blockfrost.Project -> [GYTxOutRef] -> IO GYUTxOs
blockfrostUtxosAtTxOutRefs proj refs = do
    {- This combines utxo refs with the same tx id, yielding a 'Map Api.TxId (Set Integer)'.

    That is, a map from transaction hash to a set of utxo indices within that transaction,
    that the caller is interested in.
    -}
    let refMap =
            Map.fromListWith (<>)
            $ map ((\(!txId, !utxoIdx) -> (txIdToApi txId, Set.singleton $ toInteger utxoIdx)) . txOutRefToTuple) refs
    {- For each tx id, query blockfrost for the utxo outputs produced by said tx.

    Once all the outputs are obtained, filter to only end up with the utxo indices the caller
    is interested in.
    -}
    txUtxoMap <- handleBlockfrostError locationIndent <=< Blockfrost.runBlockfrost proj . flip Map.traverseWithKey refMap
        $ \txId idxs -> do
            res <- Blockfrost.tryError
                $ Blockfrost.getTxUtxos . Blockfrost.TxHash $ Api.serialiseToRawBytesHexText txId
            case res of
                Left Blockfrost.BlockfrostNotFound -> pure []
                Left err                           -> throwError err
                Right (Blockfrost._transactionUtxosOutputs -> outs) -> pure $
                    filter (\(Blockfrost._utxoOutputOutputIndex -> idx) -> idx `Set.member` idxs) outs
    -- Create a 'GYUTxOs' map from the 'Map Api.TxId [Blockfrost.UtxoOutput]', covering for deserialize failures.
    txUtxoMap' <- foldM f Map.empty $ Map.toList txUtxoMap
    case Map.traverseWithKey (traverse . transformUtxoOutput) txUtxoMap' of
      Left err  -> throwIO $ BlpvDeserializeFailure locationIndent err
      Right res -> pure . utxosFromList . concat $ Map.elems res
  where
    locationIndent = "TxUtxos"

    f :: Map Api.S.TxId [(Blockfrost.UtxoOutput, Maybe (Some GYScript))]
      -> (Api.S.TxId, [Blockfrost.UtxoOutput])
      -> IO (Map Api.S.TxId [(Blockfrost.UtxoOutput, Maybe (Some GYScript))])
    f m (tid, os) = do
        xs <- forM os $ \o -> lookupScriptHashIO proj (Blockfrost._utxoOutputReferenceScriptHash o) >>= \ms -> return (o, ms)
        return $ Map.insert tid xs m

-- | Helper to transform a 'Blockfrost.UtxoOutput' into a 'GYUTxO'.
transformUtxoOutput :: Api.S.TxId -> (Blockfrost.UtxoOutput, Maybe (Some GYScript)) -> Either SomeDeserializeError GYUTxO
transformUtxoOutput txId (Blockfrost.UtxoOutput {..}, ms) = do
    val  <- bimap DeserializeErrorAssetClass fold $ traverse amountToValue _utxoOutputAmount
    addr <- maybeToRight DeserializeErrorAddress . addressFromTextMaybe $ Blockfrost.unAddress _utxoOutputAddress
    d    <- outDatumFromBlockfrost _utxoOutputDataHash _utxoOutputInlineDatum
    pure GYUTxO
        { utxoRef       = txOutRefFromApi . Api.TxIn txId . Api.TxIx $ fromInteger _utxoOutputOutputIndex
        , utxoAddress   = addr
        , utxoValue     = val
        , utxoOutDatum  = d
        , utxoRefScript = ms
        }

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

blockfrostProtocolParams :: Blockfrost.Project -> IO Api.S.ProtocolParameters
blockfrostProtocolParams proj = do
    Blockfrost.ProtocolParams {..} <- Blockfrost.runBlockfrost proj Blockfrost.getLatestEpochProtocolParams
        >>= handleBlockfrostError "ProtocolParams"
    let majorProtVers = fromInteger _protocolParamsProtocolMajorVer
    pure $ Api.S.ProtocolParameters
        { protocolParamProtocolVersion     = (majorProtVers, fromInteger _protocolParamsProtocolMinorVer)
        , protocolParamDecentralization    = Nothing  -- Also known as `d`, got deprecated in Babbage.
        , protocolParamExtraPraosEntropy   = Nothing  -- Also known as `extraEntropy`, got deprecated in Babbage.
        , protocolParamMaxBlockHeaderSize  = fromInteger _protocolParamsMaxBlockHeaderSize
        , protocolParamMaxBlockBodySize    = fromInteger _protocolParamsMaxBlockSize
        , protocolParamMaxTxSize           = fromInteger _protocolParamsMaxTxSize
        , protocolParamTxFeeFixed          = fromInteger _protocolParamsMinFeeB
        , protocolParamTxFeePerByte        = fromInteger _protocolParamsMinFeeA
        , protocolParamMinUTxOValue        = Nothing  -- Deprecated in Alonzo.
        , protocolParamStakeAddressDeposit = Api.Lovelace $ lovelacesToInteger _protocolParamsKeyDeposit
        , protocolParamStakePoolDeposit    = Api.Lovelace $ lovelacesToInteger _protocolParamsPoolDeposit
        , protocolParamMinPoolCost         = Api.Lovelace $ lovelacesToInteger _protocolParamsMinPoolCost
        , protocolParamPoolRetireMaxEpoch  = Api.EpochNo $ fromInteger _protocolParamsEMax
        , protocolParamStakePoolTargetNum  = fromInteger _protocolParamsNOpt
        , protocolParamPoolPledgeInfluence = _protocolParamsA0
        , protocolParamMonetaryExpansion   = _protocolParamsRho
        , protocolParamTreasuryCut         = _protocolParamsTau
        , protocolParamUTxOCostPerWord     = Nothing  -- Deprecated in Babbage.
        -- , protocolParamUTxOCostPerWord     = if majorProtVers < babbageProtocolVersion
        --                                         -- This is only used for pre-babbage protocols.
        --                                         then Just . Api.Lovelace $ lovelacesToInteger _protocolParamsCoinsPerUtxoWord
        --                                         else Nothing
        , protocolParamPrices              = Just $ Api.S.ExecutionUnitPrices _protocolParamsPriceStep _protocolParamsPriceMem
        , protocolParamMaxTxExUnits        = Just $ Api.ExecutionUnits (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExSteps) (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExMem)
        , protocolParamMaxBlockExUnits     = Just $ Api.ExecutionUnits (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExSteps) (fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExMem)
        , protocolParamMaxValueSize        = Just . fromInteger . Blockfrost.unQuantity $ _protocolParamsMaxValSize
        , protocolParamCollateralPercent   = Just $ fromInteger _protocolParamsCollateralPercent
        , protocolParamMaxCollateralInputs = Just $ fromInteger _protocolParamsMaxCollateralInputs
        , protocolParamCostModels          = toApiCostModel _protocolParamsCostModels
        , protocolParamUTxOCostPerByte     = Just . Api.Lovelace $ lovelacesToInteger _protocolParamsCoinsPerUtxoSize
        }
  where
    toApiCostModel = Map.fromList
        . Map.foldlWithKey (\acc k x -> case k of
            Blockfrost.PlutusV1 -> (Api.S.AnyPlutusScriptVersion Api.PlutusScriptV1, Api.CostModel x) : acc
            Blockfrost.PlutusV2 -> (Api.S.AnyPlutusScriptVersion Api.PlutusScriptV2, Api.CostModel x) : acc
            -- Don't care about non plutus cost models.
            Blockfrost.Timelock -> acc
        )
        []
        . Blockfrost.unCostModels

blockfrostStakePools :: Blockfrost.Project -> IO (Set Api.S.PoolId)
blockfrostStakePools proj = do
    {- 'Blockfrost.listPools' doesn't actually return all pools, only the first 100 or so.
    Have to handle paging manually for all. -}
    stkPools <- handleBlockfrostError locationIdent <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged -> Blockfrost.listPools' paged Blockfrost.Ascending
    -- The pool ids returned by blockfrost are in bech32.
    let poolIdsEith = traverse
            (Api.deserialiseFromBech32 (Api.proxyToAsType $ Proxy @Api.S.PoolId) . Blockfrost.unPoolId)
            stkPools
    case poolIdsEith of
        -- Deserialization failure shouldn't happen on blockfrost returned pool id.
        Left err  -> throwIO . BlpvDeserializeFailure locationIdent $ DeserializeErrorBech32 err
        Right has -> pure $ Set.fromList has
  where
    locationIdent = "ListPools"

blockfrostSystemStart :: Blockfrost.Project -> IO CTime.SystemStart
blockfrostSystemStart proj = do
  genesisParams <- Blockfrost.runBlockfrost proj Blockfrost.getLedgerGenesis >>= handleBlockfrostError "LedgerGenesis"
  pure . CTime.SystemStart . Time.posixSecondsToUTCTime $ Blockfrost._genesisSystemStart genesisParams

blockfrostEraHistory :: Blockfrost.Project -> IO (Api.EraHistory Api.CardanoMode)
blockfrostEraHistory proj = do
  eraSumms <- Blockfrost.runBlockfrost proj Blockfrost.getNetworkEras >>= handleBlockfrostError "EraHistory"
  maybe (throwIO $ BlpvIncorrectEraHistoryLength eraSumms) pure $ parseEraHist mkEra eraSumms
  where
    mkBound Blockfrost.NetworkEraBound {_boundEpoch, _boundSlot, _boundTime} = Ouroboros.Bound
        { boundTime = CTime.RelativeTime _boundTime
        , boundSlot = CSlot.SlotNo $ fromIntegral _boundSlot
        , boundEpoch = CSlot.EpochNo $ fromIntegral _boundEpoch
        }
    mkEraParams Blockfrost.NetworkEraParameters {_parametersEpochLength, _parametersSlotLength, _parametersSafeZone} = Ouroboros.EraParams
        { eraEpochSize = CSlot.EpochSize $ fromIntegral _parametersEpochLength
        , eraSlotLength = CTime.mkSlotLength _parametersSlotLength
        , eraSafeZone = Ouroboros.StandardSafeZone _parametersSafeZone
        }
    mkEra Blockfrost.NetworkEraSummary {_networkEraStart, _networkEraEnd, _networkEraParameters} = Ouroboros.EraSummary
        { eraStart = mkBound _networkEraStart
        , eraEnd = Ouroboros.EraEnd $ mkBound _networkEraEnd
        , eraParams = mkEraParams _networkEraParameters
        }

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

blockfrostLookupDatum :: Blockfrost.Project -> GYLookupDatum
blockfrostLookupDatum p dh = do
    datumMaybe <- handler <=< Blockfrost.runBlockfrost p
        . Blockfrost.getScriptDatum . Blockfrost.DatumHash . Text.pack . show $ datumHashToPlutus dh
    sequence $ datumMaybe <&> \(Blockfrost.ScriptDatum v) -> case fromJson @Plutus.BuiltinData (Aeson.encode v) of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right bd -> pure $ datumFromPlutus' bd
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError locationIdent $ Just <$> other
    locationIdent = "LookupDatum"

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Constructs a Blockfrost client.
--
networkIdToProject :: GYNetworkId              -- ^ The network identifier.
                   -> Text                     -- ^ The Blockfrost project identifier.
                   -> Blockfrost.Project
networkIdToProject nid pid = Blockfrost.Project
    { projectEnv = networkIdToBlockfrost nid
    , projectId  = pid
    }

networkIdToBlockfrost :: GYNetworkId -> Blockfrost.Env
networkIdToBlockfrost GYMainnet        = Blockfrost.Mainnet
networkIdToBlockfrost GYTestnetPreprod = Blockfrost.Preprod
networkIdToBlockfrost GYTestnetPreview = Blockfrost.Preview
networkIdToBlockfrost GYTestnetLegacy  = Blockfrost.Testnet
-- TODO: we need another mechanism to query private network data
networkIdToBlockfrost GYPrivnet        = error "Private network is not supported by Blockfrost"

datumHashFromBlockfrost :: Blockfrost.DatumHash -> Either SomeDeserializeError GYDatumHash
datumHashFromBlockfrost = first (DeserializeErrorHex . Text.pack) . datumHashFromHexE . Text.unpack . Blockfrost.unDatumHash

datumFromBlockfrostCBOR :: Blockfrost.ScriptDatumCBOR -> Either SomeDeserializeError GYDatum
datumFromBlockfrostCBOR d = do
    bs  <- fromEither $ BS16.decode $ Text.encodeUtf8 t
    api <- fromEither $ Api.deserialiseFromCBOR Api.AsScriptData bs
    return $ datumFromApi' api
  where
    t = Blockfrost._scriptDatumCborCbor d
    e = DeserializeErrorHex t

    fromEither :: Either e a -> Either SomeDeserializeError a
    fromEither = first $ const e

outDatumFromBlockfrost :: Maybe Blockfrost.DatumHash -> Maybe Blockfrost.InlineDatum -> Either SomeDeserializeError GYOutDatum
outDatumFromBlockfrost mdh mind = do
    mdh'  <- mapM datumHashFromBlockfrost mdh
    mind' <- mapM (datumFromBlockfrostCBOR . Blockfrost.unInlineDatum) mind
    return $ case (mind', mdh') of
        (Just ind, _      ) -> GYOutDatumInline ind
        (Nothing , Just h ) -> GYOutDatumHash h
        (Nothing , Nothing) -> GYOutDatumNone

lookupScriptHash :: Blockfrost.ScriptHash -> Blockfrost.BlockfrostClient (Maybe (Some GYScript))
lookupScriptHash h = do
    t <- Blockfrost._scriptType <$> Blockfrost.getScript h
    case t of
        Blockfrost.Timelock -> return Nothing
        _                   -> do
            mcbor <- Blockfrost._scriptCborCbor <$> Blockfrost.getScriptCBOR h
            case mcbor of
                Nothing   -> return Nothing
                Just cbor -> return $
                    if t == Blockfrost.PlutusV1
                        then Some <$> scriptFromCBOR @PlutusV1 cbor
                        else Some <$> scriptFromCBOR @PlutusV2 cbor

lookupScriptHashIO :: Blockfrost.Project -> Maybe Blockfrost.ScriptHash -> IO (Maybe (Some GYScript))
lookupScriptHashIO _ Nothing  = return Nothing
lookupScriptHashIO p (Just h) = do
    e <- Blockfrost.runBlockfrost p $ lookupScriptHash h
    handleBlockfrostError "lookupScriptHash" e
