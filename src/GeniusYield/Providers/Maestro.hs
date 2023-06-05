{-|
Module      : GeniusYield.Providers.Maestro
Description : Providers using the Maestro blockchain API.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.Maestro
  ( networkIdToMaestroEnv
  , maestroSubmitTx
  , maestroSlotActions
  , maestroGetCurrentSlot
  , utxoFromMaestro
  , maestroQueryUtxo
  , maestroProtocolParams
  , maestroStakePools
  , maestroSystemStart
  , maestroEraHistory
  , maestroLookupDatum
  ) where

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Slotting.Slot                as CSlot
import qualified Cardano.Slotting.Time                as CTime
import           Control.Exception                    (try)
import           Control.Monad                        ((<=<))
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Base16               as BS16
import           Data.Either.Combinators              (maybeToRight)
import           Data.Functor                         ((<&>))
import qualified Data.Map.Strict                      as M
import qualified Data.Set                             as Set
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified Data.Time                            as Time
import           Data.Traversable                     (for)
import           GeniusYield.Imports
import           GeniusYield.Providers.Common
import           GeniusYield.Types
import qualified Maestro.Client                       as Maestro
import qualified Maestro.Types                        as Maestro
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import qualified PlutusTx.Builtins                    as Plutus
import qualified Web.HttpApiData                      as Web

-- | Convert our representation of Network ID to Maestro's.
networkIdToMaestroEnv :: Text -> GYNetworkId -> IO Maestro.MaestroEnv
networkIdToMaestroEnv key nid = Maestro.mkMaestroEnv key $ fromMaybe (error "Only preprod and mainnet networks are supported by Maestro") $ M.lookup nid $ M.fromList [(GYMainnet, Maestro.Mainnet), (GYTestnetPreprod, Maestro.Preprod)]

-- | Exceptions.
data MaestroProviderException
  = MspvApiError !Text !Maestro.MaestroError
    -- ^ Error from the Maestro API.
  | MspvDeserializeFailure !Text !SomeDeserializeError
    -- ^ This error should never actually happen (unless there's a bug).
  | MspvMultiUtxoPerRef !GYTxOutRef
    -- ^ The API returned several utxos for a single TxOutRef.
  | MspvIncorrectEraHistoryLength ![Maestro.EraSummary]
    -- ^ The API returned an unexpected number of era summaries.
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

-- | Utility function to handle Maestro errors, which also removes header (if present) so as to conceal API key.
handleMaestroError :: Text -> Either Maestro.MaestroError a -> IO a
handleMaestroError locationInfo = either (throwIO . MspvApiError locationInfo . silenceHeadersMaestroClientError) pure
  where
    silenceHeadersMaestroClientError (Maestro.ServantClientError e) = Maestro.ServantClientError $ silenceHeadersClientError e
    silenceHeadersMaestroClientError other                          = other

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

-- | Submits a 'GYTx'.
maestroSubmitTx :: Maestro.MaestroEnv -> GYSubmitTx
maestroSubmitTx env tx = do
  txId <- handleMaestroError locationIdent <=< try $ Maestro.submitTx env $ Api.serialiseToCBOR $ txToApi tx
  either
    (throwIO . MspvDeserializeFailure locationIdent . DeserializeErrorHex . Text.pack)
    pure
    $ txIdFromHexE $ Text.unpack txId
  where
    locationIdent = "SubmitTx"

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

-- | Definition of 'GYSlotActions' for the Maestro provider.
maestroSlotActions :: Maestro.MaestroEnv -> GYSlotActions
maestroSlotActions env = GYSlotActions
    { gyGetCurrentSlot'   = x
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault x
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault x
    }
  where
    x = maestroGetCurrentSlot env

-- | Returns the current 'GYSlot'.
maestroGetCurrentSlot :: Maestro.MaestroEnv -> IO GYSlot
maestroGetCurrentSlot env =
  try (Maestro.getChainTip env) >>= handleMaestroError "CurrentSlot" <&> slotFromApi . coerce . Maestro._chainTipSlot

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

-- | Convert Maestro's datum hash to our GY type.
datumHashFromMaestro :: Text -> Either SomeDeserializeError GYDatumHash
datumHashFromMaestro = first (DeserializeErrorHex . Text.pack) . datumHashFromHexE . Text.unpack

-- | Get datum from bytes.
datumFromMaestroCBOR :: Text -> Either SomeDeserializeError GYDatum
datumFromMaestroCBOR d = do
  bs  <- fromEither $ BS16.decode $ Text.encodeUtf8 d
  api <- fromEither $ Api.deserialiseFromCBOR Api.AsScriptData bs
  return $ datumFromApi' api
  where
    e = DeserializeErrorHex d

    fromEither :: Either e a -> Either SomeDeserializeError a
    fromEither = first $ const e

-- | Get datum from JSON representation. Though we don't make use of it.
_datumFromMaestroJSON :: Aeson.Value -> Either SomeDeserializeError GYDatum
_datumFromMaestroJSON datumJson = datumFromPlutus' <$> fromJson @Plutus.BuiltinData (Aeson.encode datumJson)

-- | Convert datum present in UTxO to our GY type, `GYOutDatum`.
outDatumFromMaestro :: Maybe Maestro.DatumOption -> Either SomeDeserializeError GYOutDatum
outDatumFromMaestro Nothing                         = Right GYOutDatumNone
outDatumFromMaestro (Just Maestro.DatumOption {..}) =
  case _datumOptionType of
    Maestro.Hash -> GYOutDatumHash <$> datumHashFromMaestro _datumOptionHash
    Maestro.Inline -> case _datumOptionBytes of
      Nothing                     -> Left $ DeserializeErrorImpossibleBranch "Datum type is inline but datum bytestring is missing"
      Just db -> GYOutDatumInline <$> datumFromMaestroCBOR db

-- | Convert Maestro's asset class to our GY type.
assetClassFromMaestro :: Text -> Either SomeDeserializeError GYAssetClass
assetClassFromMaestro t = first (DeserializeErrorAssetClass . Text.pack) $ parseAssetClassWithSep '#' t

-- | Convert Maestro's asset to our GY type.
valueFromMaestro :: Maestro.Asset -> Either SomeDeserializeError GYValue
valueFromMaestro Maestro.Asset {..} = do
  asc <- assetClassFromMaestro _assetUnit
  pure $ valueSingleton asc $ toInteger _assetQuantity

-- | Convert Maestro's script to our GY type.
scriptFromMaestro :: Maestro.Script -> Either SomeDeserializeError (Maybe (Some GYScript))
scriptFromMaestro Maestro.Script {..} = case _scriptType of
  Maestro.Native   -> pure Nothing
  Maestro.PlutusV1 -> case _scriptBytes of
    Nothing -> Left $ DeserializeErrorImpossibleBranch "UTxO has PlutusV1 script but still no script bytes are present"
    Just sb -> pure $ Some <$> scriptFromCBOR  @'PlutusV1 sb
  Maestro.PlutusV2 -> case _scriptBytes of
    Nothing -> Left $ DeserializeErrorImpossibleBranch "UTxO has PlutusV2 script but still no script bytes are present"
    Just sb -> pure $ Some <$> scriptFromCBOR  @'PlutusV2 sb

-- | Convert Maestro's UTxO to our GY type.
utxoFromMaestro :: Maestro.Utxo -> Either SomeDeserializeError GYUTxO
utxoFromMaestro Maestro.Utxo {..} = do
  ref <- first DeserializeErrorHex . Web.parseUrlPiece $ _utxoTxHash <> Text.pack ('#' : show _utxoIndex)
  addr <- maybeToRight DeserializeErrorAddress $ addressFromTextMaybe _utxoAddress
  d <- outDatumFromMaestro _utxoDatum
  vs <- mapM valueFromMaestro _utxoAssets
  s <- maybe (pure Nothing) scriptFromMaestro _utxoReferenceScript
  pure $
    GYUTxO
      { utxoRef       = ref
      , utxoAddress   = addr
      , utxoValue     = mconcat vs
      , utxoOutDatum  = d
      , utxoRefScript = s
      }

-- | Query UTxOs present at multiple addresses.
maestroUtxosAtAddresses :: Maestro.MaestroEnv -> [GYAddress] -> IO GYUTxOs
maestroUtxosAtAddresses env addrs = do
  let addrsInText = map addressToText addrs
  -- Here one would not get `MaestroNotFound` error.
  addrUtxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (flip (Maestro.utxosAtMultiAddresses env (Just False) (Just False)) addrsInText)

  either
    (throwIO . MspvDeserializeFailure locationIdent)
    pure
    $ utxosFromList <$> traverse utxoFromMaestro addrUtxos
  where
    locationIdent = "AddressesUtxo"

-- | Returns a list containing all 'GYTxOutRef' for a given 'GYAddress'.
maestroRefsAtAddress :: Maestro.MaestroEnv -> GYAddress -> IO [GYTxOutRef]
maestroRefsAtAddress env addr = do
  -- Here one would not get `MaestroNotFound` error.
  mTxRefs <- handleMaestroError locationIdent <=< try $ Maestro.allPages (Maestro.getRefsAtAddress env (addressToText addr))
  either
      (throwIO . MspvDeserializeFailure locationIdent . DeserializeErrorHex)
      pure
      $ traverse
          (\Maestro.UtxoRef{..} ->
              Web.parseUrlPiece $ _utxoRefTxHash <> Text.pack ('#' : show _utxoRefIndex)
          )
          mTxRefs
  where
    locationIdent = "RefsAtAddress"

-- | Query UTxO present at a output reference.
maestroUtxoAtTxOutRef :: Maestro.MaestroEnv -> GYTxOutRef -> IO (Maybe GYUTxO)
maestroUtxoAtTxOutRef env ref = do
  res <- maestroUtxosAtTxOutRefs' env [ref]
  case res of
    []               -> pure Nothing
    [x]              -> pure $ Just x
    -- This shouldn't happen.
    _anyOtherFailure -> throwIO $ MspvMultiUtxoPerRef ref

-- | Query UTxO in case of multiple output references.
maestroUtxosAtTxOutRefs :: Maestro.MaestroEnv -> [GYTxOutRef] -> IO GYUTxOs
maestroUtxosAtTxOutRefs env = fmap utxosFromList . maestroUtxosAtTxOutRefs' env

-- | Query UTxO in case of multiple output references.
maestroUtxosAtTxOutRefs' :: Maestro.MaestroEnv -> [GYTxOutRef] -> IO [GYUTxO]
maestroUtxosAtTxOutRefs' env refs = do
  -- Here we would like the behaviour that if UTxO corresponding to one of the reference is not found, whole call should not fail which is why we have sub exception catching.
  res <- handleMaestroError locationIdent <=< try $ for refs $ \ref -> do
      let (Api.serialiseToRawBytesHexText -> txId, utxoIdx) = bimap txIdToApi toInteger $ txOutRefToTuple ref
      (Just <$> Maestro.txUtxo env (coerce txId) (fromInteger utxoIdx) (Just False) (Just False)) `catch` handler

  either
      (throwIO . MspvDeserializeFailure locationIdent)
      pure
      . traverse utxoFromMaestro $ catMaybes res
  where
    handler (Maestro.MaestroNotFound _) = pure Nothing
    handler other                   = throwIO other

    locationIdent = "UtxoByRefs"

-- | Definition of 'GYQueryUTxO' for the Maestro provider.
maestroQueryUtxo :: Maestro.MaestroEnv -> GYQueryUTxO
maestroQueryUtxo env = GYQueryUTxO
  { gyQueryUtxosAtAddresses'           = maestroUtxosAtAddresses env
  , gyQueryUtxosAtTxOutRefs'           = maestroUtxosAtTxOutRefs env
  , gyQueryUtxoAtTxOutRef'             = maestroUtxoAtTxOutRef env
  , gyQueryUtxoRefsAtAddress'          = maestroRefsAtAddress env
  , gyQueryUtxosAtAddressesWithDatums' = Nothing
  }

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Returns the 'Api.S.ProtocolParameters' queried from Maestro.
maestroProtocolParams :: Maestro.MaestroEnv -> IO Api.S.ProtocolParameters
maestroProtocolParams env = do
  Maestro.ProtocolParameters {..} <- handleMaestroError "ProtocolParams" <=< try $ Maestro.getProtocolParameters env
  pure $
    Api.S.ProtocolParameters
      { protocolParamProtocolVersion     = (Maestro._protocolVersionMajor _protocolParametersProtocolVersion, Maestro._protocolVersionMinor _protocolParametersProtocolVersion)
      , protocolParamDecentralization    = Nothing -- Also known as `d`, got deprecated in Babbage.
      , protocolParamExtraPraosEntropy   = Nothing -- Also known as `extraEntropy`, got deprecated in Babbage.
      , protocolParamMaxBlockHeaderSize  = _protocolParametersMaxBlockHeaderSize
      , protocolParamMaxBlockBodySize    = _protocolParametersMaxBlockBodySize
      , protocolParamMaxTxSize           = _protocolParametersMaxTxSize
      , protocolParamTxFeeFixed          = _protocolParametersMinFeeConstant
      , protocolParamTxFeePerByte        = _protocolParametersMinFeeCoefficient
      , protocolParamMinUTxOValue        = Nothing -- Deprecated in Alonzo.
      , protocolParamStakeAddressDeposit = Api.Lovelace $ toInteger _protocolParametersStakeKeyDeposit
      , protocolParamStakePoolDeposit    = Api.Lovelace $ toInteger _protocolParametersPoolDeposit
      , protocolParamMinPoolCost         = Api.Lovelace $ toInteger _protocolParametersMinPoolCost
      , protocolParamPoolRetireMaxEpoch  = Api.EpochNo $ Maestro.unEpochNo _protocolParametersPoolRetirementEpochBound
      , protocolParamStakePoolTargetNum  = _protocolParametersDesiredNumberOfPools
      , protocolParamPoolPledgeInfluence = Maestro.unMaestroRational _protocolParametersPoolInfluence
      , protocolParamMonetaryExpansion   = Maestro.unMaestroRational _protocolParametersMonetaryExpansion
      , protocolParamTreasuryCut         = Maestro.unMaestroRational _protocolParametersTreasuryExpansion
      , protocolParamPrices              = Just $ Api.S.ExecutionUnitPrices
                                              (Maestro.unMaestroRational $ Maestro._memoryStepsWithSteps _protocolParametersPrices)
                                              (Maestro.unMaestroRational $ Maestro._memoryStepsWithMemory _protocolParametersPrices)
      , protocolParamMaxTxExUnits        = Just $ Api.ExecutionUnits
                                              (Maestro._memoryStepsWithSteps _protocolParametersMaxExecutionUnitsPerTransaction)
                                              (Maestro._memoryStepsWithMemory _protocolParametersMaxExecutionUnitsPerTransaction)
      , protocolParamMaxBlockExUnits     = Just $ Api.ExecutionUnits
                                              (Maestro._memoryStepsWithSteps _protocolParametersMaxExecutionUnitsPerBlock)
                                              (Maestro._memoryStepsWithMemory _protocolParametersMaxExecutionUnitsPerBlock)
      , protocolParamMaxValueSize        = Just _protocolParametersMaxValueSize
      , protocolParamCollateralPercent   = Just _protocolParametersCollateralPercentage
      , protocolParamMaxCollateralInputs = Just _protocolParametersMaxCollateralInputs
      , protocolParamCostModels          = M.fromList
                                              [ ( Api.S.AnyPlutusScriptVersion Api.PlutusScriptV1
                                                , coerce $ Maestro._costModelsPlutusV1 _protocolParametersCostModels
                                                )
                                              , ( Api.S.AnyPlutusScriptVersion Api.PlutusScriptV2
                                                , coerce $ Maestro._costModelsPlutusV2 _protocolParametersCostModels
                                                )
                                              ]
      , protocolParamUTxOCostPerByte     = Just . Api.Lovelace $ toInteger _protocolParametersCoinsPerUtxoByte
      , protocolParamUTxOCostPerWord     = Nothing -- Deprecated in Babbage.
      }

-- | Returns a set of all Stake Pool's 'Api.S.PoolId'.
maestroStakePools :: Maestro.MaestroEnv -> IO (Set Api.S.PoolId)
maestroStakePools env = do
  stkPoolsWithTicker <- handleMaestroError locationIdent <=< try $ Maestro.allPages (Maestro.listPools env)
  let stkPools = map (\(Maestro.PoolListInfo poolId _ticker) -> coerce poolId :: Text) stkPoolsWithTicker
  -- The pool ids returned by Maestro are in bech32.
  let poolIdsEith = traverse
          (Api.deserialiseFromBech32 (Api.proxyToAsType $ Proxy @Api.S.PoolId))
          stkPools
  case poolIdsEith of
      -- Deserialization failure shouldn't happen on Maestro returned pool id.
      Left err  -> throwIO . MspvDeserializeFailure locationIdent $ DeserializeErrorBech32 err
      Right has -> pure $ Set.fromList has
  where
    locationIdent = "ListPools"

-- | Returns the 'CTime.SystemStart' queried from Maestro.
maestroSystemStart :: Maestro.MaestroEnv -> IO CTime.SystemStart
maestroSystemStart env = fmap (CTime.SystemStart . Time.localTimeToUTC Time.utc . Maestro._systemStartTime) . handleMaestroError "SystemStart"
    <=< try $ Maestro.getSystemStart env

-- | Returns the 'Api.EraHistory' queried from Maestro.
maestroEraHistory :: Maestro.MaestroEnv -> IO (Api.EraHistory Api.CardanoMode)
maestroEraHistory env = do
  eraSumms <- handleMaestroError "EraHistory" =<< try (Maestro.getEraHistory env)
  maybe (throwIO $ MspvIncorrectEraHistoryLength eraSumms) pure $ parseEraHist mkEra eraSumms
  where
    mkBound Maestro.EraBound {_eraBoundEpoch, _eraBoundSlot, _eraBoundTime} = Ouroboros.Bound
        { boundTime = CTime.RelativeTime _eraBoundTime
        , boundSlot = CSlot.SlotNo $ fromIntegral _eraBoundSlot
        , boundEpoch = CSlot.EpochNo $ fromIntegral _eraBoundEpoch
        }
    mkEraParams Maestro.EraParameters {_eraParametersEpochLength, _eraParametersSlotLength, _eraParametersSafeZone} = Ouroboros.EraParams
        { eraEpochSize = CSlot.EpochSize $ fromIntegral _eraParametersEpochLength
        , eraSlotLength = CTime.mkSlotLength _eraParametersSlotLength
        , eraSafeZone = Ouroboros.StandardSafeZone _eraParametersSafeZone
        }
    mkEra Maestro.EraSummary {_eraSummaryStart, _eraSummaryEnd, _eraSummaryParameters} = Ouroboros.EraSummary
        { eraStart = mkBound _eraSummaryStart
        , eraEnd = maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . mkBound) _eraSummaryEnd
        , eraParams = mkEraParams _eraSummaryParameters
        }

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

-- | Given a 'GYDatumHash' returns the corresponding 'GYDatum' if found.
maestroLookupDatum :: Maestro.MaestroEnv -> GYLookupDatum
maestroLookupDatum env dh = do
  datumMaybe <- handler =<< try (Maestro.getDatumByHash env . Text.pack . show $ datumHashToPlutus dh)
  sequence $ datumMaybe <&> \(Maestro.Datum datumBytes _datumJson) -> case datumFromMaestroCBOR datumBytes of  -- NOTE: `datumFromMaestroJSON datumJson` also gives the same result.
    Left err -> throwIO $ MspvDeserializeFailure locationIdent err
    Right bd -> pure bd
  where
    locationIdent = "LookupDatum"
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left (Maestro.MaestroNotFound _)) = pure Nothing
    handler other = handleMaestroError locationIdent $ Just <$> other
