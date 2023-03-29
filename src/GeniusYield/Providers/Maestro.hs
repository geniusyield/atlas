{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

{-|
Module      : GeniusYield.Providers.Maestro
Description : Providers using the Maestro blockchain API.
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.Maestro
    ( -- * Maestro types
      MaestroUtxo(..)
    , MaestroAsset(..)
    , MaestroAssetClass(..)
    , MaestroDatumOption(..)
    , MaestroDatumOptionType(..)
    , ScriptDataDetailed(..)
    , MaestroScript(..)
    , MaestroScriptType(..)
      -- * Type translations
    , transformUtxo
      -- * Maestro Queries
    , maestroSubmitTx
    , maestroSlotActions
    , maestroGetCurrentSlot
    , maestroQueryUtxo
    , maestroLookupDatum
    , maestroProtocolParams
    , maestroEraHistory
    , maestroSystemStart
    , maestroStakePools
    , newMaestroApiEnv
    , maestroRefsAtAddress
    , networkIdToMaestroUrl
    ) where

import           Control.Exception                    (Exception, throwIO)
import           Control.Monad                        (unless, when)
import           Control.Monad.Except                 (MonadError (catchError, throwError), (<=<))
import           Data.Bifunctor                       (bimap, first)
import           Data.ByteString                      (ByteString)
import           Data.Char                            (toLower)
import           Data.Either.Combinators              (maybeToRight)
import           Data.Functor                         ((<&>))
import           Data.Maybe                           (catMaybes)
import           Data.Proxy                           (Proxy (Proxy))
import           Data.Ratio                           (denominator, numerator, (%))
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as Txt
import qualified Data.Text.Read                       as TxtRead
import           Data.Time                            (LocalTime, NominalDiffTime)
import qualified Data.Time                            as Time
import           Data.Traversable                     (for)
import           Data.Word                            (Word64)
import qualified Web.HttpApiData                      as Web

import           Data.Aeson                           (FromJSON (parseJSON), toEncoding, toJSON)
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.TH                        (Options (fieldLabelModifier), defaultOptions, deriveFromJSON)
import qualified Data.Aeson.Types                     as Aeson
import           Deriving.Aeson
import           Network.HTTP.Types.Status            (status404)
import           Servant.API                          (Capture, Get, Header', JSON, Post, QueryParam, ReqBody, Required,
                                                       Strict, type (:<|>) (..), type (:>))
import           Servant.Client                       (ClientEnv, ClientError (FailureResponse), ClientM,
                                                       ResponseF (Response, responseStatusCode), client, runClientM)

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Cardano.Slotting.Slot                as CSlot
import qualified Cardano.Slotting.Time                as CTime
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros

import           GeniusYield.Types
import           GeniusYield.Utils                    (fieldNamePrefixStrip3)

import qualified Data.Map.Strict                      as M
import           Data.Some                            (Some (..))
import           GeniusYield.Providers.Common
import           GeniusYield.Providers.SubmitApi

-- Will lower all characters for your type.
data LowerAll
instance StringModifier LowerAll where
  getStringModifier cs = toLower <$> cs

-- Will lower the first character for your type.
data LowerFirst
instance StringModifier LowerFirst where
  getStringModifier ""       = ""
  getStringModifier (c : cs) = toLower c : cs

type HexStringOf a = Text

type Bech32StringOf a = Text

type ScriptCbor = Text

-- Type for datum.
newtype ScriptDataDetailed = ScriptDataDetailed Api.ScriptData deriving stock (Eq, Show)

instance FromJSON ScriptDataDetailed where
    parseJSON = either (Aeson.parseFail . show) (pure . ScriptDataDetailed)
        . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

newtype MaestroAssetClass = MaestroAssetClass GYAssetClass deriving stock (Eq, Show)

instance FromJSON MaestroAssetClass where
    parseJSON = Aeson.withText "MaestroAssetClass"
        $ either fail (pure . MaestroAssetClass) . parseAssetClassWithSep '#'

data MaestroAsset = MaestroAsset { maQuantity :: !Word, maUnit :: !MaestroAssetClass }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ma", LowerFirst]] MaestroAsset

data MaestroScriptType = MstNative | MstPlutusV1 | MstPlutusV2
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "Mst", LowerAll]] MaestroScriptType

data MaestroScript = MaestroScript
  { msType  :: !MaestroScriptType
  , msHash  :: !(HexStringOf GYValidatorHash)
  , msBytes :: !ScriptCbor
  , msJson  :: !(Maybe Aeson.Value)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ms", LowerFirst]] MaestroScript

data MaestroPoolListInfo = MaestroPoolListInfo
  { mpliPoolIdBech32 :: !(Bech32StringOf Api.S.PoolId)
  , mpliTicker       :: !(Maybe Text)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mpli", CamelToSnake]] MaestroPoolListInfo

-- { bytes: string; json: object }
data MaestroDatum = MaestroDatum
  { mdBytes :: !(HexStringOf GYDatum) -- ^ CBOR of the datum.
  , mdJson  :: !ScriptDataDetailed   -- ^ 'Api.ScriptData' JSON in 'Api.ScriptDataJsonDetailedSchema' format.
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "md", LowerFirst]] MaestroDatum

data MaestroDatumOptionType = Hash | Inline
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] MaestroDatumOptionType

data MaestroDatumOption = MaestroDatumOption
  { mdoType  :: !MaestroDatumOptionType
  , mdoHash  :: !(HexStringOf GYDatumHash)
  , mdoBytes :: !(Maybe (HexStringOf GYDatum))
  , mdoJson  :: !(Maybe ScriptDataDetailed)
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mdo", LowerFirst]] MaestroDatumOption

data MaestroUtxo = MaestroUtxo
  { muTxHash          :: !(HexStringOf GYTxId)
  , muIndex           :: !Word
  , muAssets          :: ![MaestroAsset]
  , muAddress         :: !(Bech32StringOf GYAddress)
  , muDatum           :: !(Maybe MaestroDatumOption)
  , muReferenceScript :: !(Maybe MaestroScript)
  , muTxoutCbor       :: !(Maybe (HexStringOf GYUTxO))  -- CBOR of the Transaction output.
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mu", CamelToSnake]] MaestroUtxo

-- { number: int }
newtype MaestroSlotNo = MaestroSlotNo { msnNumber :: Word64 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "msn", LowerFirst]] MaestroSlotNo

-- { time: string }
newtype MaestroSystemStart = MaestroSystemStart { mssTime :: LocalTime }
    deriving stock (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mss", LowerFirst]] MaestroSystemStart

-- { epoch_length: CSlot.EpochSize; slot_length: NominalDiffTime; safe_zone: Word64 }
data MaestroEraParameters = MaestroEraParameters
    { mepEpochLength :: !CSlot.EpochSize
    , mepSlotLength  :: !NominalDiffTime
    , mepSafeZone    :: !Word64
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mep", CamelToSnake]] MaestroEraParameters

-- { start: MaestroEraBound; end?: MaestroEraBound; parameters: MaestroEraParameters }
data MaestroEraSummary = MaestroEraSummary
    { mesStart      :: !MaestroEraBound
    , mesEnd        :: !(Maybe MaestroEraBound)
    , mesParameters :: !MaestroEraParameters
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mes", LowerFirst]] MaestroEraSummary

-- { epoch: CSlot.EpochNo; slot: CSlot.SlotNo; time: NominalDiffTime }
data MaestroEraBound = MaestroEraBound
    { mebEpoch :: !CSlot.EpochNo
    , mebSlot  :: !CSlot.SlotNo
    , mebTime  :: !NominalDiffTime
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "meb", LowerFirst]] MaestroEraBound

data MaestroCostModel = MaestroCostModel
    { mcmPlutusV1 :: !Api.CostModel
    , mcmPlutusV2 :: !Api.CostModel
    }
    deriving stock (Eq, Show)

$( deriveFromJSON
    defaultOptions
      { fieldLabelModifier = \x -> case fieldNamePrefixStrip3 x of
            "plutusV1" -> "plutus:v1"
            "plutusV2" -> "plutus:v2"
            a          -> a
      }
    ''MaestroCostModel
 )

data MaestroProtocolParameters = MaestroProtocolParameters
    { mppProtocolVersion                 :: !MaestroProtocolVersion
    , mppMinFeeConstant                  :: !Natural
    -- ^ AKA min_fee_a; AKA tx_fee_fixed
    , mppMinFeeCoefficient               :: !Natural
    -- ^ AKA min_fee_b; AKA tx_fee_per_byte
    , mppMaxBlockBodySize                :: !Natural
    , mppMaxBlockHeaderSize              :: !Natural
    , mppMaxTxSize                       :: !Natural
    , mppStakeKeyDeposit                 :: !Natural
    -- ^ AKA stake_address_deposit
    , mppPoolDeposit                     :: !Natural
    -- ^ AKA stake_pool_deposit
    , mppPoolRetirementEpochBound        :: !Word64
    -- ^ AKA pool_retire_max_epoch; AKA e_max
    , mppDesiredNumberOfPools            :: !Natural
    -- ^ AKA stake_pool_target_num; AKA n_opt
    , mppPoolInfluence                   :: !MaestroRational
    -- ^ AKA pool_pledge_influence; AKA A0
    , mppMonetaryExpansion               :: !MaestroRational
    -- ^ AKA Rho
    , mppTreasuryExpansion               :: !MaestroRational
    -- ^ AKA treasury_cut; AKA Tau
    , mppMinPoolCost                     :: !Natural
    , mppPrices                          :: !(MaestroMemoryStepsWith MaestroRational)
    , mppMaxExecutionUnitsPerTransaction :: !(MaestroMemoryStepsWith Natural)
    , mppMaxExecutionUnitsPerBlock       :: !(MaestroMemoryStepsWith Natural)
    , mppMaxValueSize                    :: !Natural
    , mppCollateralPercentage            :: !Natural
    , mppMaxCollateralInputs             :: !Natural
    , mppCoinsPerUtxoByte                :: !Natural
    , mppCostModels                      :: !MaestroCostModel
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mpp", CamelToSnake]] MaestroProtocolParameters

data MaestroMemoryStepsWith i = MaestroMemoryStepsWith
    { mmswMemory :: !i
    , mmswSteps  :: !i
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mmsw", LowerFirst]] (MaestroMemoryStepsWith i)

data MaestroUtxoRef = MaestroUtxoRef
    { murIndex  :: !Integer
    , murTxHash :: !(HexStringOf GYTxId)
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mur", CamelToSnake]] MaestroUtxoRef

-- The rational serialization is weird; it's encoded as a string: "1/3"
newtype MaestroRational = MaestroRational { getMaestroRational :: Rational }
  deriving stock (Eq, Show)

maestroRatToTxt :: MaestroRational -> Text
maestroRatToTxt (MaestroRational rat) = Txt.pack $ show (numerator rat) ++ '/':show (denominator rat)

instance ToJSON MaestroRational where
    toEncoding = toEncoding . maestroRatToTxt
    toJSON = toJSON . maestroRatToTxt

instance FromJSON MaestroRational where
    parseJSON = Aeson.withText "MaestroRational" $ \ratTxt -> case TxtRead.signed rationalReader ratTxt of
      Left s -> fail s
      Right (rat, remainingTxt) -> if Txt.null remainingTxt
        then pure $ MaestroRational rat
        else fail "Expected full string to be parsed"
      where
        rationalReader :: TxtRead.Reader Rational
        rationalReader ratTxt = do
            (numr, remaining) <- TxtRead.decimal ratTxt
            (nextChar, denmrTxt) <- maybe
                (Left "Unexpected end of string after parsing numerator")
                pure
                $ Txt.uncons remaining
            unless (nextChar == '/')
                . Left
                $ "Expected numerator to be immediately followed by '/', but it was followed by: " ++ show nextChar
            (denmr, finalRemaining) <- TxtRead.decimal denmrTxt
            when (denmr == 0)
                $ Left "Expected non-zero denominator"
            pure (numr % denmr, finalRemaining)

data MaestroProtocolVersion = MaestroProtocolVersion
    { mpvMajor :: !Natural
    , mpvMinor :: !Natural
    }
    deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "mpv", LowerFirst]] MaestroProtocolVersion

type MaestroApi
    = Header' '[Required, Strict] "api-key" Text :> "addresses"
        :> Capture "addr" (Bech32StringOf GYAddress) :> "utxos" :> Get '[JSON] [MaestroUtxo]
    :<|> Header' '[Required, Strict] "api-key" Text
        :> "current-slot" :> Get '[JSON] MaestroSlotNo
    :<|> Header' '[Required, Strict] "api-key" Text
        :> "era-history" :> Get '[JSON] [MaestroEraSummary]
    :<|> Header' '[Required, Strict] "api-key" Text
        :> "protocol-params" :> Get '[JSON] MaestroProtocolParameters
    :<|> Header' '[Required, Strict] "api-key" Text :> "datum"
        :> Capture "datumHash" (HexStringOf GYDatumHash) :> Get '[JSON] MaestroDatum
    :<|> Header' '[Required, Strict] "api-key" Text :> "transactions"
        :> Capture "tx_hash" (HexStringOf GYTxId) :> "outputs"
        :> Capture "index" Integer :> "utxo" :> Get '[JSON] MaestroUtxo
    :<|> Header' '[Required, Strict] "api-key" Text :> "system-start" :> Get '[JSON] MaestroSystemStart
    :<|> Header' '[Required, Strict] "api-key" Text
        :> "pools"
        :> QueryParam "count" Int
        :> QueryParam "page" Int
        :> Get '[JSON] [MaestroPoolListInfo]
    :<|> Header' '[Required, Strict] "api-key" Text :> "submit" :> "tx" :> TxSubmitApiCore
    :<|> Header' '[Required, Strict] "api-key" Text
           :> "addresses" :> Capture "address" (Bech32StringOf GYAddress) :>  "utxo_refs"
           :> QueryParam "count" Int :> QueryParam "page" Int
           :> Get '[JSON] [MaestroUtxoRef]
    :<|> Header' '[Required, Strict] "api-key" Text
           :> "addresses" :> "utxos"
           :> QueryParam "resolve_datums" Bool
           :> QueryParam "with_cbor" Bool
           :> QueryParam "count" Int
           :> QueryParam "page" Int
           :> ReqBody '[JSON][Text]
           :> Post '[JSON] [MaestroUtxo]

fetchUtxosMultiAddress :: Text -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> [Bech32StringOf GYAddress] -> ClientM [MaestroUtxo]
getUtxoByRef :: Text -> HexStringOf GYTxId -> Integer -> ClientM MaestroUtxo
findDatumByHash :: Text -> HexStringOf GYDatumHash -> ClientM MaestroDatum
getCurrentSlot :: Text -> ClientM MaestroSlotNo
getEraHistory :: Text -> ClientM [MaestroEraSummary]
getProtocolParams :: Text -> ClientM MaestroProtocolParameters
getSystemStart :: Text -> ClientM MaestroSystemStart
getStakePools :: Text -> Maybe Int -> Maybe Int -> ClientM [MaestroPoolListInfo]
txSubmitPost :: Text -> ByteString -> ClientM Api.S.TxId
getRefsAtAddress :: Text -> Bech32StringOf GYAddress -> Maybe Int -> Maybe Int -> ClientM [MaestroUtxoRef]

_
    :<|> getCurrentSlot
    :<|> getEraHistory
    :<|> getProtocolParams
    :<|> findDatumByHash
    :<|> getUtxoByRef
    :<|> getSystemStart
    :<|> getStakePools
    :<|> txSubmitPost
    :<|> getRefsAtAddress
    :<|> fetchUtxosMultiAddress
     = client @MaestroApi Proxy

data MaestroProviderException
    = MspvApiError !Text !ClientError
    | MspvDeserializeFailure !Text !SomeDeserializeError
    | MspvMultiUtxoPerRef !GYTxOutRef -- ^ The API returned several utxos for a single TxOutRef.
    | MspvIncorrectEraHistoryLength ![MaestroEraSummary]
    -- ^ The API returned an unexpected number of era summaries.
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

-- | The Maestro blockchain API env, containing the Servant client and the Maestro API key.
data MaestroApiEnv = MaestroApiEnv !ClientEnv !Text

-- | Returns a new 'MaestroApiEnv' given the base URL to query and the API Token.
newMaestroApiEnv :: String -> Text -> IO MaestroApiEnv
newMaestroApiEnv baseUrl apiToken = flip MaestroApiEnv apiToken <$> newServantClientEnv baseUrl

runMaestroClient :: (Text -> ClientM a) -> MaestroApiEnv -> IO (Either ClientError a)
runMaestroClient x (MaestroApiEnv cEnv apiToken) = runClientM (x apiToken) cEnv

handleMaestroError :: Text -> Either ClientError a -> IO a
handleMaestroError locationInfo = either (throwIO . MspvApiError locationInfo . silenceHeadersClientError) pure

assetToValue :: MaestroAsset -> GYValue
assetToValue MaestroAsset{maQuantity, maUnit=MaestroAssetClass asc} = valueSingleton asc $ toInteger maQuantity

-------------------------------------------------------------------------------
-- Submit
-------------------------------------------------------------------------------

-- | Submits a 'GYTx'.
maestroSubmitTx :: MaestroApiEnv -> GYSubmitTx
maestroSubmitTx (MaestroApiEnv env apiToken) = txSubmitPost apiToken `submitApiSubmitTxCore` env

-------------------------------------------------------------------------------
-- Slot actions
-------------------------------------------------------------------------------

-- | Definition of 'GYSlotActions' for the Maestro provider.
maestroSlotActions :: MaestroApiEnv -> GYSlotActions
maestroSlotActions env = GYSlotActions
    { gyGetCurrentSlot'   = x
    , gyWaitForNextBlock' = gyWaitForNextBlockDefault x
    , gyWaitUntilSlot'    = gyWaitUntilSlotDefault x
    }
  where
    x = maestroGetCurrentSlot env

-- | Returns the current 'GYSlot'.
maestroGetCurrentSlot :: MaestroApiEnv -> IO GYSlot
maestroGetCurrentSlot env =
    runMaestroClient getCurrentSlot env >>= handleMaestroError "CurrentSlot" <&> slotFromApi . Api.SlotNo . msnNumber

-------------------------------------------------------------------------------
-- Query UTxO
-------------------------------------------------------------------------------

-- | Definition of 'GYQueryUTxO' for the Maestro provider.
maestroQueryUtxo :: MaestroApiEnv -> GYQueryUTxO
maestroQueryUtxo env = GYQueryUTxO
    { gyQueryUtxosAtAddresses'  = maestroUtxoAtAddresses env
    , gyQueryUtxosAtTxOutRefs'  = maestroUtxosAtTxOutRefs env
    , gyQueryUtxoAtTxOutRef'    = maestroUtxosAtTxOutRef env
    , gyQueryUtxoRefsAtAddress' = maestroRefsAtAddress env
    }

-- Utility for querying all results from a paged endpoint.
maestroAllPages :: (Foldable t, Monoid (t a)) => (Int -> Int -> ClientM (t a)) -> ClientM (t a)
maestroAllPages act = fetch 1
  where
    fetch pageNo = do
      xs <- act maxPageSize pageNo
      if length xs < maxPageSize then
        pure xs
      else do
        next <- fetch $ pageNo + 1
        pure $ xs <> next
    maxPageSize = 100

datumHashFromMaestro :: HexStringOf GYDatum -> Either SomeDeserializeError GYDatumHash
datumHashFromMaestro = first (DeserializeErrorHex . Txt.pack) . datumHashFromHexE . Txt.unpack

outDatumFromMaestro :: Maybe MaestroDatumOption -> Either SomeDeserializeError GYOutDatum
outDatumFromMaestro Nothing                     = Right GYOutDatumNone
outDatumFromMaestro (Just MaestroDatumOption{..}) = case mdoType of
         Hash -> do
           mdoHash' <- datumHashFromMaestro mdoHash
           return $ GYOutDatumHash mdoHash'
         Inline -> case mdoJson of
           Nothing                     -> pure GYOutDatumNone
           Just (ScriptDataDetailed d) -> pure $ GYOutDatumInline (datumFromApi' d)

scriptFromMaestro :: MaestroScript -> Maybe (Some GYScript)
scriptFromMaestro MaestroScript{..} = case msType of
  MstNative   -> Nothing
  MstPlutusV1 -> Some <$> scriptFromCBOR @PlutusV1 msBytes
  MstPlutusV2 -> Some <$> scriptFromCBOR @PlutusV2 msBytes

transformUtxo :: MaestroUtxo -> Either SomeDeserializeError GYUTxO
transformUtxo MaestroUtxo {muTxHash, muIndex, muAssets, muAddress, muDatum, muReferenceScript} = do
    ref <- first DeserializeErrorHex . Web.parseUrlPiece $ muTxHash <> Txt.pack ('#' : show muIndex)
    addr <- maybeToRight DeserializeErrorAddress $ addressFromTextMaybe muAddress
    d <- outDatumFromMaestro muDatum
    pure $ GYUTxO
        { utxoRef       = ref
        , utxoAddress   = addr
        , utxoValue     = foldMap assetToValue muAssets
        , utxoOutDatum  = d
        , utxoRefScript = scriptFromMaestro =<< muReferenceScript
        }

maestroUtxoAtAddresses :: MaestroApiEnv -> [GYAddress] -> IO GYUTxOs
maestroUtxoAtAddresses env addrs = do
    let addrsInText = map addressToText addrs
    addrUtxos <- handler <=< flip runMaestroClient env
        $ \token -> maestroAllPages $ \c n -> fetchUtxosMultiAddress token (Just False) (Just False) (Just c) (Just n) addrsInText
    either
        (throwIO . MspvDeserializeFailure locationIdent)
        pure
        $ utxosFromList <$> traverse transformUtxo addrUtxos
  where
    -- This particular error is fine in this case, we can just return empty list.
    handler (Left (FailureResponse _ Response{responseStatusCode}))
        | responseStatusCode == status404 = pure []
    handler other = handleMaestroError locationIdent other

    locationIdent = "AddressesUtxo"


-- | Returns a list containing all 'GYTxOutRef' for a given 'GYAddress'.
maestroRefsAtAddress :: MaestroApiEnv -> GYAddress -> IO [GYTxOutRef]
maestroRefsAtAddress env addr = do
    mTxRefs <- handleMaestroError locationIdent <=< flip runMaestroClient env
        $ \token -> maestroAllPages $ \c n -> getRefsAtAddress token (addressToText addr) (Just c) (Just n)
    either
        (throwIO . MspvDeserializeFailure locationIdent . DeserializeErrorHex)
        pure
        $ traverse
            (\MaestroUtxoRef{..} ->
                Web.parseUrlPiece $ murTxHash <> Txt.pack ('#' : show murIndex)
            )
            mTxRefs
  where
    locationIdent = "RefsAtAddress"

maestroUtxosAtTxOutRef :: MaestroApiEnv -> GYTxOutRef -> IO (Maybe GYUTxO)
maestroUtxosAtTxOutRef env ref = do
    res <- maestroUtxosAtTxOutRefs' env [ref]
    case res of
        []               -> pure Nothing
        [x]              -> pure $ Just x
        -- This shouldn't happen.
        _anyOtherFailure -> throwIO $ MspvMultiUtxoPerRef ref

maestroUtxosAtTxOutRefs :: MaestroApiEnv -> [GYTxOutRef] -> IO GYUTxOs
maestroUtxosAtTxOutRefs env = fmap utxosFromList . maestroUtxosAtTxOutRefs' env

maestroUtxosAtTxOutRefs' :: MaestroApiEnv -> [GYTxOutRef] -> IO [GYUTxO]
maestroUtxosAtTxOutRefs' env refs = do
    res <- handleMaestroError locationIdent <=< flip runMaestroClient env $ \apiToken -> for refs $ \ref -> do
        let (Api.serialiseToRawBytesHexText -> txId, utxoIdx) = bimap txIdToApi toInteger $ txOutRefToTuple ref
        (Just <$> getUtxoByRef apiToken txId utxoIdx) `catchError` handler
    either
        (throwIO . MspvDeserializeFailure locationIdent)
        pure
        . traverse transformUtxo $ catMaybes res
  where
    handler (FailureResponse _ Response{responseStatusCode})
        | responseStatusCode == status404 = pure Nothing
    handler other = throwError other

    locationIdent = "UtxoByRefs"

-------------------------------------------------------------------------------
-- Parameters
-------------------------------------------------------------------------------

-- | Returns the 'Api.S.ProtocolParameters' queried from Maestro.
maestroProtocolParams :: MaestroApiEnv -> IO Api.S.ProtocolParameters
maestroProtocolParams env = do
    MaestroProtocolParameters {..} <- runMaestroClient getProtocolParams env >>= handleMaestroError "ProtocolParams"
    pure $ Api.S.ProtocolParameters
        { protocolParamProtocolVersion     = (mpvMajor mppProtocolVersion, mpvMinor mppProtocolVersion)
        , protocolParamDecentralization    = Nothing -- FIXME: This should be returned by Maestro?
        , protocolParamExtraPraosEntropy   = Nothing -- Q: Is this unnecessary?
        , protocolParamMaxBlockHeaderSize  = mppMaxBlockHeaderSize
        , protocolParamMaxBlockBodySize    = mppMaxBlockBodySize
        , protocolParamMaxTxSize           = mppMaxTxSize
        , protocolParamTxFeeFixed          = mppMinFeeConstant
        , protocolParamTxFeePerByte        = mppMinFeeCoefficient
        , protocolParamMinUTxOValue        = Nothing -- FIXME: This should be returned by Maestro?
        , protocolParamStakeAddressDeposit = Api.Lovelace $ toInteger mppStakeKeyDeposit
        , protocolParamStakePoolDeposit    = Api.Lovelace $ toInteger mppPoolDeposit
        , protocolParamMinPoolCost         = Api.Lovelace $ toInteger mppMinPoolCost
        , protocolParamPoolRetireMaxEpoch  = Api.EpochNo mppPoolRetirementEpochBound
        , protocolParamStakePoolTargetNum  = mppDesiredNumberOfPools
        , protocolParamPoolPledgeInfluence = getMaestroRational mppPoolInfluence
        , protocolParamMonetaryExpansion   = getMaestroRational mppMonetaryExpansion
        , protocolParamTreasuryCut         = getMaestroRational mppTreasuryExpansion
        , protocolParamPrices              = Just $ Api.S.ExecutionUnitPrices
                                                (getMaestroRational $ mmswSteps mppPrices)
                                                (getMaestroRational $ mmswMemory mppPrices)
        , protocolParamMaxTxExUnits        = Just $ Api.ExecutionUnits
                                                (mmswSteps mppMaxExecutionUnitsPerTransaction)
                                                (mmswMemory mppMaxExecutionUnitsPerTransaction)
        , protocolParamMaxBlockExUnits     = Just $ Api.ExecutionUnits
                                                (mmswSteps mppMaxExecutionUnitsPerBlock)
                                                (mmswMemory mppMaxExecutionUnitsPerBlock)
        , protocolParamMaxValueSize        = Just mppMaxValueSize
        , protocolParamCollateralPercent   = Just mppCollateralPercentage
        , protocolParamMaxCollateralInputs = Just mppMaxCollateralInputs
        , protocolParamCostModels          = M.fromList
                                                [ ( Api.S.AnyPlutusScriptVersion Api.PlutusScriptV1
                                                  , mcmPlutusV1 mppCostModels
                                                  )
                                                , ( Api.S.AnyPlutusScriptVersion Api.PlutusScriptV2
                                                  , mcmPlutusV2 mppCostModels
                                                  )
                                                ]
        , protocolParamUTxOCostPerByte     = Just . Api.Lovelace $ toInteger mppCoinsPerUtxoByte
        , protocolParamUTxOCostPerWord     = Nothing -- Deprecated post babbage
        }

-- | Returns a set of all Stake Pool's 'Api.S.PoolId'.
maestroStakePools :: MaestroApiEnv -> IO (Set Api.S.PoolId)
maestroStakePools env = do
    stkPoolsWithTicker <- handleMaestroError locationIdent <=< flip runMaestroClient env
        $ \token -> maestroAllPages $ \c n -> getStakePools token (Just c) (Just n)
    let stkPools = map (\(MaestroPoolListInfo pool _ticker) -> pool) stkPoolsWithTicker
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
maestroSystemStart :: MaestroApiEnv -> IO CTime.SystemStart
maestroSystemStart = fmap (CTime.SystemStart . Time.localTimeToUTC Time.utc . mssTime) . handleMaestroError "SystemStart"
    <=< runMaestroClient getSystemStart

-- | Returns the 'Api.EraHistory' queried from Maestro.
maestroEraHistory :: MaestroApiEnv -> IO (Api.EraHistory Api.CardanoMode)
maestroEraHistory env = do
    eraSumms <- runMaestroClient getEraHistory env >>= handleMaestroError "EraHistory"
    maybe (throwIO $ MspvIncorrectEraHistoryLength eraSumms) pure $ parseEraHist mkEra eraSumms
  where
    mkBound MaestroEraBound {mebEpoch, mebSlot, mebTime} = Ouroboros.Bound
        { boundTime = CTime.RelativeTime mebTime
        , boundSlot = mebSlot
        , boundEpoch = mebEpoch
        }
    mkEraParams MaestroEraParameters {mepEpochLength, mepSlotLength, mepSafeZone} = Ouroboros.EraParams
        { eraEpochSize = mepEpochLength
        , eraSlotLength = CTime.mkSlotLength mepSlotLength
        , eraSafeZone = Ouroboros.StandardSafeZone mepSafeZone
        }
    mkEra MaestroEraSummary {mesStart, mesEnd, mesParameters} = Ouroboros.EraSummary
        { eraStart = mkBound mesStart
        , eraEnd = maybe Ouroboros.EraUnbounded (Ouroboros.EraEnd . mkBound) mesEnd
        , eraParams = mkEraParams mesParameters
        }

-------------------------------------------------------------------------------
-- Datum lookup
-------------------------------------------------------------------------------

-- | Given a 'GYDatumHash' returns the corresponding 'GYDatum' if found.
maestroLookupDatum :: MaestroApiEnv -> GYLookupDatum
maestroLookupDatum env dh = do
    datumMaybe <- handler <=< flip runMaestroClient env
        . flip findDatumByHash . Txt.pack . show $ datumHashToPlutus dh
    pure $ datumMaybe <&> \(MaestroDatum _ (ScriptDataDetailed scriptData)) -> datumFromApi' scriptData
  where
    -- This particular error is fine in this case, we can just return 'Nothing'.
    handler (Left (FailureResponse _ Response{responseStatusCode}))
        | responseStatusCode == status404 = pure Nothing
    handler other = handleMaestroError "LookupDatum" $ Just <$> other

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Constructs the Maestro base URL from network id.
networkIdToMaestroUrl :: GYNetworkId -> String
networkIdToMaestroUrl GYMainnet        = "https://mainnet.gomaestro-api.org/"
networkIdToMaestroUrl GYTestnetPreprod = "https://preprod.gomaestro-api.org/"
networkIdToMaestroUrl GYTestnetPreview = error "Only preprod and mainnet networks are supported by Maestro"
networkIdToMaestroUrl GYTestnetLegacy  = error "Only preprod and mainnet networks are supported by Maestro"
networkIdToMaestroUrl GYPrivnet        = error "Only preprod and mainnet networks are supported by Maestro"
