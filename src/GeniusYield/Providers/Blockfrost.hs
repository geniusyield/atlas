module GeniusYield.Providers.Blockfrost
    ( Blockfrost.Project
    , blockfrostProtocolParams
    , blockfrostStakePools
    , blockfrostSystemStart
    , blockfrostEraHistory
    , blockfrostQueryUtxo
    , blockfrostLookupDatum
    , blockfrostGetSlotOfCurrentBlock
    , blockfrostSubmitTx
    , blockfrostAwaitTxConfirmed
    , blockfrostStakeAddressInfo
    , networkIdToProject
    ) where

import qualified Blockfrost.Client                    as Blockfrost
import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Ledger                   as Api.L
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Ledger.Alonzo.PParams        as Ledger
import qualified Cardano.Ledger.BaseTypes             as Ledger
import qualified Cardano.Ledger.Coin                  as Ledger
import           Cardano.Ledger.Conway.PParams        (ConwayPParams (..),
                                                       THKD (..))
import qualified Cardano.Ledger.Core                  as Ledger
import qualified Cardano.Ledger.Plutus                as Ledger
import qualified Cardano.Slotting.Slot                as CSlot
import qualified Cardano.Slotting.Time                as CTime
import           Control.Concurrent                   (threadDelay)
import           Control.Monad                        ((<=<))
import           Control.Monad.Except                 (throwError)
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Base16               as BS16
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Either.Combinators              (maybeToRight)
import           Data.Foldable                        (fold)
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
import           GeniusYield.Types.ProtocolParameters (GYProtocolParameters,
                                                       protocolParametersFromApi)
import           GeniusYield.Utils                    (serialiseToBech32WithPrefix)
import           Ouroboros.Consensus.HardFork.History (EraParams (eraGenesisWin))

data BlockfrostProviderException
    = BlpvApiError !Text !Blockfrost.BlockfrostError
    | BlpvDeserializeFailure !Text !SomeDeserializeError            -- ^ This error should never actually happen (unless there's a bug).
    | BlpvNoSlotInfo !Blockfrost.BlockHash
    | BlpvUnsupportedOperation !Text
    | BlpvIncorrectEraHistoryLength ![Blockfrost.NetworkEraSummary]
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

throwBlpvApiError :: Text -> Blockfrost.BlockfrostError -> IO a
throwBlpvApiError locationInfo =
    throwIO . BlpvApiError locationInfo . silenceHeadersBlockfrostClientError

handleBlockfrostError :: Text -> Either Blockfrost.BlockfrostError a -> IO a
handleBlockfrostError locationInfo = either (throwBlpvApiError locationInfo) pure

silenceHeadersBlockfrostClientError :: Blockfrost.BlockfrostError -> Blockfrost.BlockfrostError
silenceHeadersBlockfrostClientError (Blockfrost.ServantClientError e) = Blockfrost.ServantClientError $ silenceHeadersClientError e
silenceHeadersBlockfrostClientError other                             = other

lovelacesToInteger :: Blockfrost.Lovelaces -> Integer
lovelacesToInteger = fromIntegral

gyAddressToBlockfrost :: GYAddress -> Blockfrost.Address
gyAddressToBlockfrost = Blockfrost.mkAddress . addressToText

gyPaymentCredentialToBlockfrost :: GYPaymentCredential -> Blockfrost.Address
gyPaymentCredentialToBlockfrost cred = Blockfrost.mkAddress $ case cred of
   GYPaymentCredentialByKey _ -> paymentCredentialToBech32 cred
   GYPaymentCredentialByScript sh -> serialiseToBech32WithPrefix "addr_vkh" $ scriptHashToApi sh  -- A bug in BF.

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
    txId <- handleBlockfrostSubmitError <=< Blockfrost.runBlockfrost proj
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
    handleBlockfrostSubmitError = either (throwIO . SubmitTxException . Text.pack . show . silenceHeadersBlockfrostClientError) pure

-------------------------------------------------------------------------------
-- Await tx confirmation
-------------------------------------------------------------------------------

-- | Awaits for the confirmation of a given 'GYTxId'
blockfrostAwaitTxConfirmed :: Blockfrost.Project -> GYAwaitTx
blockfrostAwaitTxConfirmed proj p@GYAwaitTxParameters{..} txId = blpAwaitTx 0
  where
    blpAwaitTx :: Int -> IO ()
    blpAwaitTx attempt | maxAttempts <= attempt = throwIO $ GYAwaitTxException p
    blpAwaitTx attempt = do
        eTxInfo <- blockfrostQueryTx proj txId
        case eTxInfo of
            Left Blockfrost.BlockfrostNotFound -> threadDelay checkInterval >>
                                                  blpAwaitTx (attempt + 1)
            Left err -> throwBlpvApiError "AwaitTx" err
            Right txInfo -> blpAwaitBlock attempt $
                            Blockfrost._transactionBlock txInfo

    blpAwaitBlock :: Int -> Blockfrost.BlockHash -> IO ()
    blpAwaitBlock attempt _ | maxAttempts <= attempt = throwIO $ GYAwaitTxException p
    blpAwaitBlock attempt blockHash = do
        eBlockInfo <- blockfrostQueryBlock proj blockHash
        case eBlockInfo of
            Left Blockfrost.BlockfrostNotFound -> threadDelay checkInterval >>
                                                  blpAwaitBlock (attempt + 1) blockHash
            Left err -> throwBlpvApiError "AwaitBlock" err

            Right blockInfo | attempt + 1 == maxAttempts ->
                when (Blockfrost._blockConfirmations blockInfo < toInteger confirmations) $
                throwIO $ GYAwaitTxException p

            Right blockInfo ->
                when (Blockfrost._blockConfirmations blockInfo < toInteger confirmations) $
                threadDelay checkInterval >> blpAwaitBlock (attempt + 1) blockHash

blockfrostQueryBlock
    :: Blockfrost.Project
    -> Blockfrost.BlockHash
    -> IO (Either Blockfrost.BlockfrostError Blockfrost.Block)
blockfrostQueryBlock proj = Blockfrost.runBlockfrost proj
                            . Blockfrost.getBlock . Right

blockfrostQueryTx
    :: Blockfrost.Project
    -> GYTxId
    -> IO (Either Blockfrost.BlockfrostError Blockfrost.Transaction)
blockfrostQueryTx proj = Blockfrost.runBlockfrost proj
                         . Blockfrost.getTx . Blockfrost.TxHash
                         . Api.serialiseToRawBytesHexText . txIdToApi

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

blockfrostGetSlotOfCurrentBlock :: Blockfrost.Project -> IO GYSlot
blockfrostGetSlotOfCurrentBlock proj = do
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
    { gyQueryUtxosAtTxOutRefs'             = blockfrostUtxosAtTxOutRefs proj
    , gyQueryUtxosAtTxOutRefsWithDatums'   = Nothing  -- Will use the default implementation.
    , gyQueryUtxoAtTxOutRef'               = blockfrostUtxosAtTxOutRef proj
    , gyQueryUtxoRefsAtAddress'            = gyQueryUtxoRefsAtAddressDefault $ blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtAddresses'             = gyQueryUtxoAtAddressesDefault $ blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtAddress'               = blockfrostUtxosAtAddress proj
    , gyQueryUtxosAtAddressWithDatums'     = Nothing
    , gyQueryUtxosAtAddressesWithDatums'   = Nothing  -- Will use the default implementation.
    , gyQueryUtxosAtPaymentCredential'     = blockfrostUtxosAtPaymentCredential proj
    , gyQueryUtxosAtPaymentCredWithDatums' = Nothing  -- Will use the default implementation.
    , gyQueryUtxosAtPaymentCredentials'    = gyQueryUtxoAtPaymentCredentialsDefault $ blockfrostUtxosAtPaymentCredential proj
    , gyQueryUtxosAtPaymentCredsWithDatums' = Nothing  -- Will use the default implementation.
    }

transformUtxo :: (Blockfrost.AddressUtxo, Maybe GYAnyScript) -> Either SomeDeserializeError GYUTxO
transformUtxo (Blockfrost.AddressUtxo {..}, ms) = do
    val  <- bimap DeserializeErrorAssetClass fold $ traverse amountToValue _addressUtxoAmount
    addr <- maybeToRight DeserializeErrorAddress $ addressFromTextMaybe $ Blockfrost.unAddress _addressUtxoAddress
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

blockfrostUtxosAtAddress :: Blockfrost.Project -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
blockfrostUtxosAtAddress proj addr mAssetClass = do
    let extractedAssetClass = extractAssetClass mAssetClass
    {- 'Blockfrost.getAddressUtxos' doesn't return all utxos at that address, only the first 100 or so.
    Have to handle paging manually for all. -}
    addrUtxos  <- handler <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged ->
            case extractedAssetClass of
                Nothing -> Blockfrost.getAddressUtxos' (gyAddressToBlockfrost addr) paged Blockfrost.Ascending
                Just (ac, tn) -> Blockfrost.getAddressUtxosAsset' (gyAddressToBlockfrost addr) (Blockfrost.mkAssetId $ ac <> tn) paged Blockfrost.Ascending
    addrUtxos' <- mapM (\x -> lookupScriptHashIO proj (Blockfrost._addressUtxoReferenceScriptHash x) >>= \mrs -> return (x, mrs)) addrUtxos
    case traverse transformUtxo addrUtxos' of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right x  -> pure $ utxosFromList x
  where
    locationIdent = "AddressUtxos"
    -- This particular error is fine in this case, we can just return empty list.
    handler (Left Blockfrost.BlockfrostNotFound) = pure []
    handler other                                = handleBlockfrostError locationIdent other

blockfrostUtxosAtPaymentCredential :: Blockfrost.Project -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
blockfrostUtxosAtPaymentCredential proj cred mAssetClass = do
    let extractedAssetClass = extractAssetClass mAssetClass
    {- 'Blockfrost.getAddressUtxos' doesn't return all utxos at that address, only the first 100 or so.
    Have to handle paging manually for all. -}
    credUtxos  <- handler <=< Blockfrost.runBlockfrost proj
        . Blockfrost.allPages $ \paged ->
            case extractedAssetClass of
                Nothing -> Blockfrost.getAddressUtxos' (gyPaymentCredentialToBlockfrost cred) paged Blockfrost.Ascending
                Just (ac, tn) -> Blockfrost.getAddressUtxosAsset' (gyPaymentCredentialToBlockfrost cred) (Blockfrost.mkAssetId $ ac <> tn) paged Blockfrost.Ascending
    credUtxos' <- mapM (\x -> lookupScriptHashIO proj (Blockfrost._addressUtxoReferenceScriptHash x) >>= \mrs -> return (x, mrs)) credUtxos
    case traverse transformUtxo credUtxos' of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right x  -> pure $ utxosFromList x
  where
    locationIdent = "PaymentCredentialUtxos"
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

    f :: Map Api.S.TxId [(Blockfrost.UtxoOutput, Maybe GYAnyScript)]
      -> (Api.S.TxId, [Blockfrost.UtxoOutput])
      -> IO (Map Api.S.TxId [(Blockfrost.UtxoOutput, Maybe GYAnyScript)])
    f m (tid, os) = do
        xs <- forM os $ \o -> lookupScriptHashIO proj (Blockfrost._utxoOutputReferenceScriptHash o) >>= \ms -> return (o, ms)
        return $ Map.insert tid xs m

-- | Helper to transform a 'Blockfrost.UtxoOutput' into a 'GYUTxO'.
transformUtxoOutput :: Api.S.TxId -> (Blockfrost.UtxoOutput, Maybe GYAnyScript) -> Either SomeDeserializeError GYUTxO
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

blockfrostProtocolParams :: GYNetworkId -> Blockfrost.Project -> IO GYProtocolParameters
blockfrostProtocolParams nid proj = do
    Blockfrost.ProtocolParams {..} <- Blockfrost.runBlockfrost proj Blockfrost.getLatestEpochProtocolParams
        >>= handleBlockfrostError "ProtocolParams"
    pure $ protocolParametersFromApi $ Ledger.PParams $ populateMissingProtocolParameters nid $
        ConwayPParams
        { cppMinFeeA        = THKD $ Ledger.Coin _protocolParamsMinFeeA
        , cppMinFeeB        = THKD $ Ledger.Coin _protocolParamsMinFeeB
        , cppMaxBBSize    = THKD $ fromIntegral _protocolParamsMaxBlockSize
        , cppMaxTxSize           = THKD $ fromIntegral _protocolParamsMaxTxSize
        , cppMaxBHSize  = THKD $ fromIntegral _protocolParamsMaxBlockHeaderSize
        , cppKeyDeposit = THKD $ Ledger.Coin $ lovelacesToInteger _protocolParamsKeyDeposit
        , cppPoolDeposit    = THKD $ Ledger.Coin $ lovelacesToInteger _protocolParamsPoolDeposit
        , cppEMax  = THKD $ Ledger.EpochInterval . fromIntegral
                                                $ _protocolParamsEMax
        , cppNOpt  = THKD $ fromIntegral _protocolParamsNOpt
        , cppA0 = THKD $ fromMaybe (error "GeniusYield.Providers.Maestro.maestroProtocolParams: pool influence received from Maestro is out of bounds")  $ Ledger.boundRational _protocolParamsA0
        , cppRho   = THKD $ fromMaybe (error "GeniusYield.Providers.Maestro.maestroProtocolParams: monetory expansion parameter received from Maestro is out of bounds")  $ Ledger.boundRational _protocolParamsRho
        , cppTau         = THKD $ fromMaybe (error "GeniusYield.Providers.Maestro.maestroProtocolParams: treasury expansion parameter received from Maestro is out of bounds") $ Ledger.boundRational _protocolParamsTau
        , cppProtocolVersion     = Ledger.ProtVer {
            Ledger.pvMajor = Ledger.mkVersion _protocolParamsProtocolMajorVer & fromMaybe (error "GeniusYield.Providers.Maestro.maestroProtocolParams: major version received from Maestro is out of bounds"),
            Ledger.pvMinor = fromIntegral _protocolParamsProtocolMinorVer
            }
        , cppMinPoolCost         = THKD $ Ledger.Coin $ lovelacesToInteger _protocolParamsMinPoolCost
        , cppCoinsPerUTxOByte     = THKD $ Api.L.CoinPerByte $ Ledger.Coin $ lovelacesToInteger _protocolParamsCoinsPerUtxoSize
        , cppCostModels = THKD $ Ledger.mkCostModels $ Map.fromList $ plutusV3CostModels errPath : Map.foldlWithKey' (\acc k x -> case k of
            Blockfrost.PlutusV1 -> (Ledger.PlutusV1, either (error (errPath <> "Couldn't build PlutusV1 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV1 $ fromInteger <$> Map.elems x) : acc
            Blockfrost.PlutusV2 -> (Ledger.PlutusV2, either (error (errPath <> "Couldn't build PlutusV2 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV2 $ fromInteger <$> Map.elems x) : acc
            -- Don't care about non plutus cost models.
            Blockfrost.Timelock -> acc

        ) [] (Blockfrost.unCostModels _protocolParamsCostModels)
        , cppPrices              = THKD $ Ledger.Prices {Ledger.prSteps = fromMaybe (error (errPath <> "Couldn't bound Blockfrost's cpu steps")) $ Ledger.boundRational _protocolParamsPriceStep, Ledger.prMem = fromMaybe (error (errPath <> "Couldn't bound Blockfrost's memory units")) $ Ledger.boundRational _protocolParamsPriceMem}
        , cppMaxTxExUnits        = THKD $ Ledger.OrdExUnits $ Ledger.ExUnits {
                                            Ledger.exUnitsSteps =
                                                fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExSteps,
                                            Ledger.exUnitsMem =
                                                fromInteger $ Blockfrost.unQuantity _protocolParamsMaxTxExMem
                                            }
        , cppMaxBlockExUnits     = THKD $ Ledger.OrdExUnits $ Ledger.ExUnits {
                                            Ledger.exUnitsSteps =
                                                fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExSteps,
                                            Ledger.exUnitsMem =
                                                fromInteger $ Blockfrost.unQuantity _protocolParamsMaxBlockExMem
                                            }
        , cppMaxValSize        = THKD $ fromIntegral $ Blockfrost.unQuantity _protocolParamsMaxValSize
        , cppCollateralPercentage   = THKD $ fromIntegral _protocolParamsCollateralPercent
        , cppMaxCollateralInputs = THKD $ fromIntegral _protocolParamsMaxCollateralInputs
        -- FIXME: Fetch these from provider.
        , cppPoolVotingThresholds = undefined
        , cppDRepVotingThresholds = undefined
        , cppCommitteeMinSize = undefined
        , cppCommitteeMaxTermLength = undefined
        , cppGovActionLifetime = undefined
        , cppGovActionDeposit = undefined
        , cppDRepDeposit = undefined
        , cppDRepActivity = undefined
        , cppMinFeeRefScriptCostPerByte = undefined
        }
  where
    errPath = "GeniusYield.Providers.Blockfrost.blockfrostProtocolParams: "

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

blockfrostEraHistory :: Blockfrost.Project -> IO Api.EraHistory
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
        , eraGenesisWin = 0
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
    mapM (\(Blockfrost.ScriptDatum v) -> case fromJson @Plutus.BuiltinData (Aeson.encode v) of
      Left err -> throwIO $ BlpvDeserializeFailure locationIdent err
      Right bd -> pure $ datumFromPlutus' bd) datumMaybe
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError locationIdent $ Just <$> other
    locationIdent = "LookupDatum"

-------------------------------------------------------------------------------
-- Account info
-------------------------------------------------------------------------------

blockfrostStakeAddressInfo :: Blockfrost.Project -> GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
blockfrostStakeAddressInfo p saddr = do
  Blockfrost.runBlockfrost p (Blockfrost.getAccount (Blockfrost.mkAddress $ stakeAddressToText saddr)) >>= handler
  where
    -- This particular error is fine.
    handler (Left Blockfrost.BlockfrostNotFound) = pure Nothing
    handler other                                = handleBlockfrostError "Account" $ other <&> \accInfo ->
        if Blockfrost._accountInfoActive accInfo then Just $
            GYStakeAddressInfo
                { gyStakeAddressInfoDelegatedPool = Blockfrost._accountInfoPoolId accInfo >>= stakePoolIdFromTextMaybe . Blockfrost.unPoolId
                , gyStakeAddressInfoAvailableRewards = fromInteger $ lovelacesToInteger $ Blockfrost._accountInfoWithdrawableAmount accInfo
                }
        else Nothing

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
networkIdToBlockfrost GYPrivnet{}      = error "Private network is not supported by Blockfrost"

datumHashFromBlockfrost :: Blockfrost.DatumHash -> Either SomeDeserializeError GYDatumHash
datumHashFromBlockfrost = first (DeserializeErrorHex . Text.pack) . datumHashFromHexE . Text.unpack . Blockfrost.unDatumHash

datumFromBlockfrostCBOR :: Blockfrost.ScriptDatumCBOR -> Either SomeDeserializeError GYDatum
datumFromBlockfrostCBOR d = do
    bs  <- fromEither $ BS16.decode $ Text.encodeUtf8 t
    api <- fromEither $ Api.deserialiseFromCBOR Api.AsHashableScriptData bs
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

lookupScriptHash :: Blockfrost.ScriptHash -> Blockfrost.BlockfrostClient (Maybe GYAnyScript)
lookupScriptHash h = do
    t <- Blockfrost._scriptType <$> Blockfrost.getScript h
    case t of
        Blockfrost.Timelock -> do
            mjson <- Blockfrost._scriptJsonJson <$> Blockfrost.getScriptJSON h
            case mjson of
                Nothing   -> return Nothing
                Just json -> return $ GYSimpleScript <$> simpleScriptFromJSON json
        _                   -> do
            mcbor <- Blockfrost._scriptCborCbor <$> Blockfrost.getScriptCBOR h
            case mcbor of
                Nothing   -> return Nothing
                Just cbor -> return $
                    if t == Blockfrost.PlutusV1
                        then GYPlutusScript <$> scriptFromCBOR @'PlutusV1 cbor
                        else GYPlutusScript <$> scriptFromCBOR @'PlutusV2 cbor

lookupScriptHashIO :: Blockfrost.Project -> Maybe Blockfrost.ScriptHash -> IO (Maybe GYAnyScript)
lookupScriptHashIO _ Nothing  = return Nothing
lookupScriptHashIO p (Just h) = do
    e <- Blockfrost.runBlockfrost p $ lookupScriptHash h
    handleBlockfrostError "lookupScriptHash" e
