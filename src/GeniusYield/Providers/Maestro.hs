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
  , maestroAwaitTxConfirmed
  , maestroGetSlotOfCurrentBlock
  , utxoFromMaestro
  , maestroQueryUtxo
  , maestroProtocolParams
  , maestroStakePools
  , maestroSystemStart
  , maestroEraHistory
  , maestroLookupDatum
  , maestroUtxosAtAddressesWithDatums
  , maestroUtxosAtPaymentCredentialsWithDatums
  , maestroStakeAddressInfo
  ) where

import qualified Cardano.Api                                    as Api
import qualified Cardano.Api.Ledger                             as Api.L
import qualified Cardano.Api.Ledger                             as Ledger
import qualified Cardano.Api.Shelley                            as Api.S
import qualified Cardano.Ledger.Alonzo.PParams                  as Ledger
import           Cardano.Ledger.Conway.PParams                  (ConwayPParams (..),
                                                                 THKD (..))
import qualified Cardano.Ledger.Plutus                          as Ledger
import qualified Cardano.Slotting.Slot                          as CSlot
import qualified Cardano.Slotting.Time                          as CTime
import           Control.Concurrent                             (threadDelay)
import           Control.Exception                              (try)
import           Control.Monad                                  ((<=<))
import qualified Data.Aeson                                     as Aeson
import           Data.Default                                   (def)
import           Data.Either.Combinators                        (maybeToRight)
import           Data.Int                                       (Int64)
import qualified Data.Map.Strict                                as M
import           Data.Maybe                                     (fromJust)
import qualified Data.Set                                       as Set
import qualified Data.Text                                      as Text
import qualified Data.Time                                      as Time
import           GeniusYield.Imports
import           GeniusYield.Providers.Common
import           GeniusYield.Types
import           GeniusYield.Types.ProtocolParameters           (GYProtocolParameters,
                                                                 protocolParametersFromApi)
import           GHC.Natural                                    (wordToNatural)
import qualified Maestro.Client.V1                              as Maestro
import qualified Maestro.Client.V1.Accounts                     as Maestro
import qualified Maestro.Types.V1                               as Maestro
import qualified Ouroboros.Consensus.HardFork.History           as Ouroboros
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (eraGenesisWin))
import qualified PlutusTx.Builtins                              as Plutus
import qualified Web.HttpApiData                                as Web

-- | Convert our representation of Network ID to Maestro's.
networkIdToMaestroEnv :: Text -> GYNetworkId -> IO (Maestro.MaestroEnv 'Maestro.V1)
networkIdToMaestroEnv key nid = Maestro.mkMaestroEnv @'Maestro.V1 key (fromMaybe (error "Only preview, preprod and mainnet networks are supported by Maestro") $ M.lookup nid $ M.fromList [(GYMainnet, Maestro.Mainnet), (GYTestnetPreprod, Maestro.Preprod), (GYTestnetPreview, Maestro.Preview)]) Maestro.defaultBackoff

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

throwMspvApiError :: Text -> Maestro.MaestroError -> IO a
throwMspvApiError locationInfo =
    throwIO . MspvApiError locationInfo . silenceHeadersMaestroClientError

-- | Utility function to handle Maestro errors, which also removes header (if present) so as to conceal API key.
handleMaestroError :: Text -> Either Maestro.MaestroError a -> IO a
handleMaestroError locationInfo = either (throwMspvApiError locationInfo) pure

-- | Remove headers (if `MaestroError` contains `ClientError`).
silenceHeadersMaestroClientError :: Maestro.MaestroError -> Maestro.MaestroError
silenceHeadersMaestroClientError (Maestro.ServantClientError e) = Maestro.ServantClientError $ silenceHeadersClientError e
silenceHeadersMaestroClientError other                          = other

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

-- | Submits a 'GYTx'.
maestroSubmitTx :: Bool -> Maestro.MaestroEnv 'Maestro.V1 -> GYSubmitTx
maestroSubmitTx useTurboSubmit env tx = do
  txId <- handleMaestroSubmitError <=< try $ let txCbor = Api.serialiseToCBOR $ txToApi tx in if useTurboSubmit then Maestro.turboSubmitAndMonitorTx env txCbor else Maestro.submitAndMonitorTx env txCbor
  either
    (throwIO . MspvDeserializeFailure "SubmitTx" . DeserializeErrorHex . Text.pack)
    pure
    $ txIdFromHexE $ Text.unpack txId
  where
    handleMaestroSubmitError :: Either Maestro.MaestroError a -> IO a
    handleMaestroSubmitError = either (throwIO . SubmitTxException . Text.pack . show . silenceHeadersMaestroClientError) pure

-------------------------------------------------------------------------------
-- Await tx confirmation
-------------------------------------------------------------------------------

-- | Awaits for the confirmation of a given 'GYTxId'
maestroAwaitTxConfirmed :: Maestro.MaestroEnv 'Maestro.V1 -> GYAwaitTx
maestroAwaitTxConfirmed env p@GYAwaitTxParameters{..} txId = mspvAwaitTx 0
  where
    mspvAwaitTx :: Int -> IO ()
    mspvAwaitTx attempt | maxAttempts <= attempt = throwIO $ GYAwaitTxException p
    mspvAwaitTx attempt = do
        eTxInfo <- maestroQueryTx env txId
        case eTxInfo of
            Left Maestro.MaestroNotFound -> threadDelay checkInterval >>
                                            mspvAwaitTx (attempt + 1)
            Left err -> throwMspvApiError "AwaitTx" err
            Right txInfo -> msvpAwaitBlock attempt $
                            Maestro.txDetailsBlockHash $
                            Maestro.getTimestampedData txInfo

    msvpAwaitBlock :: Int -> Maestro.BlockHash -> IO ()
    msvpAwaitBlock attempt _ | maxAttempts <= attempt = throwIO $ GYAwaitTxException p
    msvpAwaitBlock attempt blockHash = do
        eBlockInfo <- maestroQueryBlock env blockHash
        case eBlockInfo of
            Left Maestro.MaestroNotFound -> threadDelay checkInterval >>
                                            msvpAwaitBlock (attempt + 1) blockHash
            Left err -> throwMspvApiError "AwaitBlock" err

            Right (Maestro.getTimestampedData -> blockInfo) | attempt + 1 == maxAttempts ->
                when (toInteger (Maestro.blockDetailsConfirmations blockInfo)
                      <
                      toInteger confirmations) $ throwIO $ GYAwaitTxException p

            Right (Maestro.getTimestampedData -> blockInfo) ->
                when (toInteger (Maestro.blockDetailsConfirmations blockInfo)
                      <
                      toInteger confirmations) $
                threadDelay checkInterval >> msvpAwaitBlock (attempt + 1) blockHash

maestroQueryBlock
    :: Maestro.MaestroEnv 'Maestro.V1
    -> Maestro.BlockHash
    -> IO (Either Maestro.MaestroError Maestro.TimestampedBlockDetails)
maestroQueryBlock env = try . Maestro.blockDetailsByHash env

maestroQueryTx
    :: Maestro.MaestroEnv 'Maestro.V1
    -> GYTxId
    -> IO (Either Maestro.MaestroError Maestro.TimestampedTxDetails)
maestroQueryTx env = try . Maestro.txInfo env . Maestro.TxHash .
                     Api.serialiseToRawBytesHexText . txIdToApi

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

-- | Returns the current 'GYSlot'.
maestroGetSlotOfCurrentBlock :: Maestro.MaestroEnv 'Maestro.V1 -> IO GYSlot
maestroGetSlotOfCurrentBlock env =
  try (Maestro.getChainTip env) >>= handleMaestroError "SlotOfCurrentBlock" <&> slotFromApi . coerce . Maestro.chainTipSlot . Maestro.getTimestampedData

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

-- | Convert Maestro's datum hash to our GY type.
datumHashFromMaestro :: Text -> Either SomeDeserializeError GYDatumHash
datumHashFromMaestro = first (DeserializeErrorHex . Text.pack) . datumHashFromHexE . Text.unpack

-- | Get datum from JSON representation. Though we don't make use of it.
_datumFromMaestroJSON :: Aeson.Value -> Either SomeDeserializeError GYDatum
_datumFromMaestroJSON datumJson = datumFromPlutus' <$> fromJson @Plutus.BuiltinData (Aeson.encode datumJson)

-- | Convert datum present in UTxO to our GY type, `GYOutDatum`.
outDatumFromMaestro :: Maybe Maestro.DatumOption -> Either SomeDeserializeError GYOutDatum
outDatumFromMaestro Nothing                         = Right GYOutDatumNone
outDatumFromMaestro (Just Maestro.DatumOption {..}) =
  case datumOptionType of
    Maestro.Hash -> GYOutDatumHash <$> datumHashFromMaestro datumOptionHash
    Maestro.Inline -> case datumOptionBytes of
      Nothing                     -> Left $ DeserializeErrorImpossibleBranch "Datum type is inline but datum bytestring is missing"
      Just db -> GYOutDatumInline <$> datumFromCBOR db

-- | Convert Maestro's asset class to our GY type.
assetClassFromMaestro :: Maestro.AssetUnit -> Either SomeDeserializeError GYAssetClass
assetClassFromMaestro Maestro.Lovelace = pure GYLovelace
assetClassFromMaestro (Maestro.UserMintedToken (Maestro.NonAdaNativeToken policyId tokenName)) = first (DeserializeErrorAssetClass . Text.pack) $ parseAssetClassWithSep '#' (coerce policyId <> "#" <> coerce tokenName)

-- | Convert Maestro's asset to our GY type.
valueFromMaestro :: Maestro.Asset -> Either SomeDeserializeError GYValue
valueFromMaestro Maestro.Asset {..} = do
  asc <- assetClassFromMaestro assetUnit
  pure $ valueSingleton asc $ toInteger assetAmount

-- | Convert Maestro's script to our GY type.
scriptFromMaestro :: Maestro.Script -> Either SomeDeserializeError (Maybe GYAnyScript)
scriptFromMaestro Maestro.Script {..} = case scriptType of
  Maestro.Native   -> case scriptJson of
    Nothing -> Left $ DeserializeErrorImpossibleBranch "UTxO has native script but no script JSON is present"
    Just sj -> pure $ GYSimpleScript <$> simpleScriptFromJSON sj
  Maestro.PlutusV1 -> case scriptBytes of
    Nothing -> Left $ DeserializeErrorImpossibleBranch "UTxO has PlutusV1 script but still no script bytes are present"
    Just sb -> pure $ GYPlutusScript <$> scriptFromCBOR  @'PlutusV1 sb
  Maestro.PlutusV2 -> case scriptBytes of
    Nothing -> Left $ DeserializeErrorImpossibleBranch "UTxO has PlutusV2 script but still no script bytes are present"
    Just sb -> pure $ GYPlutusScript <$> scriptFromCBOR  @'PlutusV2 sb

-- | Convert Maestro's UTxO to our GY type.
utxoFromMaestro :: Maestro.IsUtxo a => a -> Either SomeDeserializeError GYUTxO
utxoFromMaestro utxo = do
  ref <- first DeserializeErrorHex . Web.parseUrlPiece $ Web.toUrlPiece (Maestro.getTxHash utxo) <> "#" <> Web.toUrlPiece (Maestro.getIndex utxo)
  addr <- maybeToRight DeserializeErrorAddress $ addressFromTextMaybe $ coerce $ Maestro.getAddress utxo
  d <- outDatumFromMaestro $ Maestro.getDatum utxo
  vs <- mapM valueFromMaestro $ Maestro.getAssets utxo
  s <- maybe (pure Nothing) scriptFromMaestro $ Maestro.getReferenceScript utxo
  pure $
    GYUTxO
      { utxoRef       = ref
      , utxoAddress   = addr
      , utxoValue     = mconcat vs
      , utxoOutDatum  = d
      , utxoRefScript = s
      }

-- | Convert Maestro's UTxO (with datum resolved) to our GY types.
utxoFromMaestroWithDatum :: Maestro.IsUtxo a => a -> Either SomeDeserializeError (GYUTxO, Maybe GYDatum)
utxoFromMaestroWithDatum u = do
  gyUtxo <- utxoFromMaestro u
  case utxoOutDatum gyUtxo of
    GYOutDatumNone -> pure (gyUtxo, Nothing)
    GYOutDatumInline d -> pure (gyUtxo, Just d)
    GYOutDatumHash _ ->
      case Maestro.datumOptionBytes $ fromJust (Maestro.getDatum u) of
        Nothing -> pure (gyUtxo, Nothing)
        Just db -> do
          d <- datumFromCBOR db
          pure (gyUtxo, Just d)

extractedAssetClassToMaestro :: Maybe (Text, Text) -> Maybe Maestro.NonAdaNativeToken
extractedAssetClassToMaestro = fmap (\(mp, tn) -> Maestro.NonAdaNativeToken (coerce mp) (coerce tn))

maestroUtxosAtAddress :: Maestro.MaestroEnv 'Maestro.V1 -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
maestroUtxosAtAddress env addr mAssetClass = do
  let addrAsText = addressToText addr
      extractedAssetClass = extractAssetClass mAssetClass
  -- Here one would not get `MaestroNotFound` error.
  addrUtxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (Maestro.utxosAtAddress env (coerce addrAsText) (Just False) (Just False) (extractedAssetClassToMaestro extractedAssetClass))

  either (throwIO . MspvDeserializeFailure locationIdent) (pure . utxosFromList) (traverse utxoFromMaestro addrUtxos)
  where
    locationIdent = "AddressUtxos"

-- | Query UTxOs present at given address with datums.
maestroUtxosAtAddressWithDatums :: Maestro.MaestroEnv 'Maestro.V1 -> GYAddress -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]
maestroUtxosAtAddressWithDatums env addr mAssetClass = do
  let addrAsText = addressToText addr
      extractedAssetClass = extractAssetClass mAssetClass
  -- Here one would not get `MaestroNotFound` error.
  addrUtxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (Maestro.utxosAtAddress env (coerce addrAsText) (Just True) (Just False) (extractedAssetClassToMaestro extractedAssetClass))

  either
    (throwIO . MspvDeserializeFailure locationIdent)
    pure
    $ traverse utxoFromMaestroWithDatum addrUtxos
  where
    locationIdent = "AddressUtxosWithDatums"

-- | Query UTxOs present at multiple addresses.
maestroUtxosAtAddresses :: Maestro.MaestroEnv 'Maestro.V1 -> [GYAddress] -> IO GYUTxOs
maestroUtxosAtAddresses env addrs = do
  let addrsInText = map addressToText addrs
  -- Here one would not get `MaestroNotFound` error.
  addrUtxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (flip (Maestro.utxosAtMultiAddresses env (Just False) (Just False)) $ coerce addrsInText)

  either (throwIO . MspvDeserializeFailure locationIdent) (pure . utxosFromList) (traverse utxoFromMaestro addrUtxos)
  where
    locationIdent = "AddressesUtxos"

-- | Query UTxOs present at multiple addresses with datums.
maestroUtxosAtAddressesWithDatums :: Maestro.MaestroEnv 'Maestro.V1 -> [GYAddress] -> IO [(GYUTxO, Maybe GYDatum)]
maestroUtxosAtAddressesWithDatums env addrs = do
  let addrsInText = map addressToText addrs
  -- Here one would not get `MaestroNotFound` error.
  addrUtxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (flip (Maestro.utxosAtMultiAddresses env (Just True) (Just False)) $ coerce addrsInText)

  either
    (throwIO . MspvDeserializeFailure locationIdent)
    pure
    $ traverse utxoFromMaestroWithDatum addrUtxos
  where
    locationIdent = "AddressesUtxosWithDatums"

-- | Query UTxOs present at payment credential.
maestroUtxosAtPaymentCredential :: Maestro.MaestroEnv 'Maestro.V1 -> GYPaymentCredential -> Maybe GYAssetClass -> IO GYUTxOs
maestroUtxosAtPaymentCredential env paymentCredential mAssetClass = do
  let paymentCredentialBech32 :: Maestro.Bech32StringOf Maestro.PaymentCredentialAddress = coerce $ paymentCredentialToBech32 paymentCredential
      extractedAssetClass = extractAssetClass mAssetClass
  -- Here one would not get `MaestroNotFound` error.
  utxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages $ Maestro.utxosByPaymentCredential env paymentCredentialBech32 (Just False) (Just False) (extractedAssetClassToMaestro extractedAssetClass)

  either (throwIO . MspvDeserializeFailure locationIdent) (pure . utxosFromList) (traverse utxoFromMaestro utxos)
  where
    locationIdent = "PaymentCredentialUtxos"

-- | Query UTxOs present at payment credential with their associated datum fetched (under best effort basis).
maestroUtxosAtPaymentCredentialWithDatums :: Maestro.MaestroEnv 'Maestro.V1 -> GYPaymentCredential -> Maybe GYAssetClass -> IO [(GYUTxO, Maybe GYDatum)]
maestroUtxosAtPaymentCredentialWithDatums env paymentCredential mAssetClass = do
  let paymentCredentialBech32 :: Maestro.Bech32StringOf Maestro.PaymentCredentialAddress = coerce $ paymentCredentialToBech32 paymentCredential
      extractedAssetClass = extractAssetClass mAssetClass
  -- Here one would not get `MaestroNotFound` error.
  utxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages $ Maestro.utxosByPaymentCredential env paymentCredentialBech32 (Just True) (Just False) (extractedAssetClassToMaestro extractedAssetClass)

  either
    (throwIO . MspvDeserializeFailure locationIdent)
    pure
    $ traverse utxoFromMaestroWithDatum utxos
  where
    locationIdent = "PaymentCredentialUtxosWithDatums"

-- | Query UTxOs present at multiple payment credentials.
maestroUtxosAtPaymentCredentials :: Maestro.MaestroEnv 'Maestro.V1 -> [GYPaymentCredential] -> IO GYUTxOs
maestroUtxosAtPaymentCredentials env pcs = do
  let paymentCredentialsBech32 = map paymentCredentialToBech32 pcs
  -- Here one would not get `MaestroNotFound` error.
  utxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (flip (Maestro.utxosByMultiPaymentCredentials env (Just False) (Just False)) $ coerce paymentCredentialsBech32)

  either (throwIO . MspvDeserializeFailure locationIdent) (pure . utxosFromList) (traverse utxoFromMaestro utxos)
  where
    locationIdent = "PaymentCredentialsUtxos"

-- | Query UTxOs present at multiple payment credentials with datums.
maestroUtxosAtPaymentCredentialsWithDatums :: Maestro.MaestroEnv 'Maestro.V1 -> [GYPaymentCredential] -> IO [(GYUTxO, Maybe GYDatum)]
maestroUtxosAtPaymentCredentialsWithDatums env pcs = do
  let paymentCredentialsBech32 = map paymentCredentialToBech32 pcs
  -- Here one would not get `MaestroNotFound` error.
  utxos <- handleMaestroError locationIdent <=< try $ Maestro.allPages (flip (Maestro.utxosByMultiPaymentCredentials env (Just True) (Just False)) $ coerce paymentCredentialsBech32)

  either
    (throwIO . MspvDeserializeFailure locationIdent)
    pure
    $ traverse utxoFromMaestroWithDatum utxos
  where
    locationIdent = "PaymentCredentialsUtxosWithDatums"

-- | Returns a list containing all 'GYTxOutRef' for a given 'GYAddress'.
maestroRefsAtAddress :: Maestro.MaestroEnv 'Maestro.V1 -> GYAddress -> IO [GYTxOutRef]
maestroRefsAtAddress env addr = do
  -- Here one would not get `MaestroNotFound` error.
  mTxRefs <- handleMaestroError locationIdent <=< try $ Maestro.allPages (Maestro.getRefsAtAddress env $ coerce (addressToText addr))
  either
      (throwIO . MspvDeserializeFailure locationIdent . DeserializeErrorHex)
      pure
      $ traverse
          (\Maestro.OutputReferenceObject {..} ->
              Web.parseUrlPiece $ Web.toUrlPiece outputReferenceObjectTxHash <> "#" <> Web.toUrlPiece outputReferenceObjectIndex
          )
          mTxRefs
  where
    locationIdent = "RefsAtAddress"

-- | Query UTxO present at a output reference.
maestroUtxoAtTxOutRef :: Maestro.MaestroEnv 'Maestro.V1 -> GYTxOutRef -> IO (Maybe GYUTxO)
maestroUtxoAtTxOutRef env ref = do
  res <- maestroUtxosAtTxOutRefs' env [ref]
  case res of
    []               -> pure Nothing
    [x]              -> pure $ Just x
    -- This shouldn't happen.
    _anyOtherFailure -> throwIO $ MspvMultiUtxoPerRef ref

-- | Query UTxOs in case of multiple `GYTxOutRef`, i.e., multiple output references.
maestroUtxosAtTxOutRefs :: Maestro.MaestroEnv 'Maestro.V1 -> [GYTxOutRef] -> IO GYUTxOs
maestroUtxosAtTxOutRefs env = fmap utxosFromList . maestroUtxosAtTxOutRefs' env

toMaestroOutputReference :: GYTxOutRef -> Maestro.OutputReference
toMaestroOutputReference oref =
  let (txId, txIx) = txOutRefToTuple' oref
  in Maestro.OutputReference (coerce txId) (coerce $ wordToNatural txIx)

-- | Query UTxO in case of multiple output references.
maestroUtxosAtTxOutRefs' :: Maestro.MaestroEnv 'Maestro.V1 -> [GYTxOutRef] -> IO [GYUTxO]
maestroUtxosAtTxOutRefs' env refs = do
  -- NOTE: Earlier we had preferred the behaviour where if UTxO corresponding to one of the reference is not found, whole call would not fail with 404 but NOW it would.
  let refs' = map toMaestroOutputReference refs
  res <- handler <=< try $ Maestro.allPages (flip (Maestro.outputsByReferences env (Just False) (Just False)) refs')

  either
      (throwIO . MspvDeserializeFailure locationIdent)
      pure
      $ traverse utxoFromMaestro res
  where
    -- This particular error is fine in this case, we can just return @mempty@.
    handler (Left Maestro.MaestroNotFound) = pure []
    handler other = handleMaestroError locationIdent other

    locationIdent = "UtxoByRefs"

-- | Query UTxOs present at multiple `GYTxOutRef` with datums.
maestroUtxosAtTxOutRefsWithDatums :: Maestro.MaestroEnv 'Maestro.V1 -> [GYTxOutRef] -> IO [(GYUTxO, Maybe GYDatum)]
maestroUtxosAtTxOutRefsWithDatums env refs = do
  -- NOTE: Earlier we had preferred the behaviour where if UTxO corresponding to one of the reference is not found, whole call would not fail with 404 but NOW it would.
  let refs' = map toMaestroOutputReference refs
  res <- handler <=< try $ Maestro.allPages (flip (Maestro.outputsByReferences env (Just True) (Just False)) refs')

  either
      (throwIO . MspvDeserializeFailure locationIdent)
      pure
      $ traverse utxoFromMaestroWithDatum res
  where
    -- This particular error is fine in this case, we can just return @mempty@.
    handler (Left Maestro.MaestroNotFound) = pure []
    handler other = handleMaestroError locationIdent other

    locationIdent = "UtxoByRefsWithDatums"

-- | Definition of 'GYQueryUTxO' for the Maestro provider.
maestroQueryUtxo :: Maestro.MaestroEnv 'Maestro.V1 -> GYQueryUTxO
maestroQueryUtxo env = GYQueryUTxO
  { gyQueryUtxosAtAddresses'             = maestroUtxosAtAddresses env
  , gyQueryUtxosAtAddress'               = maestroUtxosAtAddress env
  , gyQueryUtxosAtAddressWithDatums'     = Just $ maestroUtxosAtAddressWithDatums env
  , gyQueryUtxosAtTxOutRefs'             = maestroUtxosAtTxOutRefs env
  , gyQueryUtxosAtTxOutRefsWithDatums'   = Just $ maestroUtxosAtTxOutRefsWithDatums env
  , gyQueryUtxoAtTxOutRef'               = maestroUtxoAtTxOutRef env
  , gyQueryUtxoRefsAtAddress'            = maestroRefsAtAddress env
  , gyQueryUtxosAtAddressesWithDatums'   = Just $ maestroUtxosAtAddressesWithDatums env
  , gyQueryUtxosAtPaymentCredential'     = maestroUtxosAtPaymentCredential env
  , gyQueryUtxosAtPaymentCredWithDatums' = Just $ maestroUtxosAtPaymentCredentialWithDatums env
  , gyQueryUtxosAtPaymentCredentials'    = maestroUtxosAtPaymentCredentials env
  , gyQueryUtxosAtPaymentCredsWithDatums' = Just $ maestroUtxosAtPaymentCredentialsWithDatums env
  }

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Returns the 'GYProtocolParameters' queried from Maestro.
maestroProtocolParams :: GYNetworkId -> Maestro.MaestroEnv 'Maestro.V1 -> IO GYProtocolParameters
maestroProtocolParams nid env = do
  Maestro.ProtocolParameters {..} <- handleMaestroError "ProtocolParams" <=< try $ Maestro.getTimestampedData <$> Maestro.getProtocolParameters env
  pure $ protocolParametersFromApi $ Ledger.PParams $ populateMissingProtocolParameters nid $
    ConwayPParams
      { cppMinFeeA        = THKD $ Ledger.Coin $ toInteger protocolParametersMinFeeCoefficient
      , cppMinFeeB        = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersMinFeeConstant
      , cppMaxBBSize    = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxBlockBodySize
      , cppMaxTxSize           = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxTransactionSize
      , cppMaxBHSize  = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxBlockHeaderSize
      , cppKeyDeposit = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersStakeCredentialDeposit
      , cppPoolDeposit    = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersStakePoolDeposit
      , cppEMax  = THKD $ Ledger.EpochInterval . fromIntegral
                                              $ Maestro.unEpochNo protocolParametersStakePoolRetirementEpochBound
      , cppNOpt  = THKD $ fromIntegral protocolParametersDesiredNumberOfStakePools
      , cppA0 = THKD $ fromMaybe (error (errPath <> "Pool influence received from Maestro is out of bounds"))  $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersStakePoolPledgeInfluence
      , cppRho   = THKD $ fromMaybe (error (errPath <> "Monetory expansion parameter received from Maestro is out of bounds"))  $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersMonetaryExpansion
      , cppTau         = THKD $ fromMaybe (error (errPath <> "Treasury expansion parameter received from Maestro is out of bounds")) $ Ledger.boundRational $ Maestro.unMaestroRational protocolParametersTreasuryExpansion
      , cppProtocolVersion     = Ledger.ProtVer {
          Ledger.pvMajor = Ledger.mkVersion (Maestro.protocolVersionMajor protocolParametersVersion) & fromMaybe (error (errPath <> "Major version received from Maestro is out of bounds")),
          Ledger.pvMinor = Maestro.protocolVersionMinor protocolParametersVersion
        }
      , cppMinPoolCost         = THKD $ Ledger.Coin $ toInteger $ Maestro.asLovelaceLovelace $ Maestro.asAdaAda protocolParametersMinStakePoolCost
      , cppCoinsPerUTxOByte     = THKD $ Api.L.CoinPerByte $ Ledger.Coin $ toInteger protocolParametersMinUtxoDepositCoefficient
      , cppCostModels = THKD $ Ledger.mkCostModels $ M.fromList
                                              [ ( Ledger.PlutusV1
                                                , either (error (errPath <> "Couldn't build PlutusV1 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV1 $ coerce @_ @[Int64] (Maestro.costModelsPlutusV1 protocolParametersPlutusCostModels)
                                                )
                                              , ( Ledger.PlutusV2
                                                , either (error (errPath <> "Couldn't build PlutusV2 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV2 $ coerce @_ @[Int64] (Maestro.costModelsPlutusV2 protocolParametersPlutusCostModels)
                                                )
                                              , plutusV3CostModels errPath
                                              ]
      , cppPrices              = THKD $ Ledger.Prices {Ledger.prSteps = fromMaybe (error (errPath <> "Couldn't bound Maestro's cpu steps")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithCpu protocolParametersScriptExecutionPrices, Ledger.prMem = fromMaybe (error (errPath <> "Couldn't bound Maestro's memory units")) $ Ledger.boundRational $ Maestro.unMaestroRational $ Maestro.memoryCpuWithMemory protocolParametersScriptExecutionPrices}
      , cppMaxTxExUnits        = THKD $ Ledger.OrdExUnits $ Ledger.ExUnits {
                                          Ledger.exUnitsSteps =
                                              Maestro.memoryCpuWithCpu protocolParametersMaxExecutionUnitsPerTransaction,
                                          Ledger.exUnitsMem =
                                              Maestro.memoryCpuWithMemory protocolParametersMaxExecutionUnitsPerTransaction
                                        }
      , cppMaxBlockExUnits     = THKD $ Ledger.OrdExUnits $ Ledger.ExUnits {
                                          Ledger.exUnitsSteps =
                                              Maestro.memoryCpuWithCpu protocolParametersMaxExecutionUnitsPerBlock,
                                          Ledger.exUnitsMem =
                                              Maestro.memoryCpuWithMemory protocolParametersMaxExecutionUnitsPerBlock
                                        }
      , cppMaxValSize        = THKD $ fromIntegral $ Maestro.asBytesBytes protocolParametersMaxValueSize
      , cppCollateralPercentage   = THKD $ fromIntegral protocolParametersCollateralPercentage
      , cppMaxCollateralInputs = THKD $ fromIntegral protocolParametersMaxCollateralInputs
      -- FIXME: Fetch these from provider.
      , cppPoolVotingThresholds = THKD def
      , cppDRepVotingThresholds = THKD def
      , cppCommitteeMinSize = THKD 0
      , cppCommitteeMaxTermLength = THKD (Ledger.EpochInterval 0)
      , cppGovActionLifetime = THKD (Ledger.EpochInterval 0)
      , cppGovActionDeposit = THKD $ Ledger.Coin 0
      , cppDRepDeposit = THKD $ Ledger.Coin 0
      , cppDRepActivity = THKD (Ledger.EpochInterval 0)
      , cppMinFeeRefScriptCostPerByte = THKD minBound
      }
  where
    errPath = "GeniusYield.Providers.Maestro.maestroProtocolParams: "

-- | Returns a set of all Stake Pool's 'Api.S.PoolId'.
maestroStakePools :: Maestro.MaestroEnv 'Maestro.V1 -> IO (Set Api.S.PoolId)
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
maestroSystemStart :: Maestro.MaestroEnv 'Maestro.V1 -> IO CTime.SystemStart
maestroSystemStart env = fmap (CTime.SystemStart . Time.localTimeToUTC Time.utc) . handleMaestroError "SystemStart"
    <=< try $ Maestro.getTimestampedData <$> Maestro.getSystemStart env

-- | Returns the 'Api.EraHistory' queried from Maestro.
maestroEraHistory :: Maestro.MaestroEnv 'Maestro.V1 -> IO Api.EraHistory
maestroEraHistory env = do
  eraSumms <- handleMaestroError "EraHistory" =<< try (Maestro.getTimestampedData <$> Maestro.getEraHistory env)
  maybe (throwIO $ MspvIncorrectEraHistoryLength eraSumms) pure $ parseEraHist mkEra eraSumms
  where
    mkBound Maestro.EraBound {eraBoundEpoch, eraBoundSlot, eraBoundTime} = Ouroboros.Bound
        { boundTime = CTime.RelativeTime $ Maestro.eraBoundTimeSeconds eraBoundTime
        , boundSlot = CSlot.SlotNo $ fromIntegral eraBoundSlot
        , boundEpoch = CSlot.EpochNo $ fromIntegral eraBoundEpoch
        }
    mkEraParams Maestro.EraParameters {eraParametersEpochLength, eraParametersSlotLength, eraParametersSafeZone} = Ouroboros.EraParams
        { eraEpochSize = CSlot.EpochSize $ fromIntegral eraParametersEpochLength
        , eraSlotLength = CTime.mkSlotLength $ Maestro.epochSlotLengthMilliseconds eraParametersSlotLength / 1000
        , eraSafeZone = Ouroboros.StandardSafeZone $ fromJust eraParametersSafeZone
        , eraGenesisWin = 0
        }
    mkEra Maestro.EraSummary {eraSummaryStart, eraSummaryEnd, eraSummaryParameters} = Ouroboros.EraSummary
        { eraStart = mkBound eraSummaryStart
        , eraEnd = maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . mkBound) eraSummaryEnd
        , eraParams = mkEraParams eraSummaryParameters
        }

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

-- | Given a 'GYDatumHash' returns the corresponding 'GYDatum' if found.
maestroLookupDatum :: Maestro.MaestroEnv 'Maestro.V1 -> GYLookupDatum
maestroLookupDatum env dh = do
  datumMaybe <- handler =<< try (Maestro.getTimestampedData <$> (Maestro.getDatumByHash env . coerce . Api.serialiseToRawBytesHexText $ datumHashToApi dh))
  mapM (\(Maestro.Datum datumBytes _datumJson) -> case datumFromCBOR datumBytes of  -- NOTE: `datumFromMaestroJSON datumJson` also gives the same result.
    Left err -> throwIO $ MspvDeserializeFailure locationIdent err
    Right bd -> pure bd) datumMaybe
  where
    locationIdent = "LookupDatum"
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left Maestro.MaestroNotFound) = pure Nothing
    handler other = handleMaestroError locationIdent $ Just <$> other

-------------------------------------------------------------------------------
-- Account info
-------------------------------------------------------------------------------

-- | Returns the 'GYStakeAddressInfo' queried from Maestro.
maestroStakeAddressInfo :: Maestro.MaestroEnv 'Maestro.V1 -> GYStakeAddress -> IO (Maybe GYStakeAddressInfo)
maestroStakeAddressInfo env saddr = do
  handler <=< try $ Maestro.getTimestampedData <$> Maestro.accountInfo env (coerce stakeAddressToText saddr)
  where
    -- This particular error is fine.
    handler (Left Maestro.MaestroNotFound) = pure Nothing
    handler other = handleMaestroError "AccountInfo" $ other <&> \accInfo ->
      if Maestro.accountInfoRegistered accInfo then Just $
        GYStakeAddressInfo
          { gyStakeAddressInfoDelegatedPool = Maestro.accountInfoDelegatedPool accInfo >>= stakePoolIdFromTextMaybe . coerce
          , gyStakeAddressInfoAvailableRewards = fromIntegral $ Maestro.accountInfoRewardsAvailable accInfo
          }
      else Nothing
