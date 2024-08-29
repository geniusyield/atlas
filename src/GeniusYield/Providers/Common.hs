{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : GeniusYield.Providers.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.Common (
      SomeDeserializeError (..)
    , SubmitTxException (..)
    , plutusV3CostModels
    , populateMissingProtocolParameters
    , datumFromCBOR
    , newServantClientEnv
    , fromJson
    , parseEraHist
    , preprodEraHist
    , previewEraHist
    , mainnetEraHist
    , silenceHeadersClientError
    , extractAssetClass
) where

import qualified Data.Aeson                                     as Aeson
import qualified Data.ByteString.Base16                         as BS16
import qualified Data.ByteString.Lazy                           as LBS
import           Data.Maybe                                     (fromJust)
import           Data.Text                                      (Text)
import qualified Data.Text                                      as Text
import qualified Data.Text.Encoding                             as Text

import qualified Network.HTTP.Client                            as HttpClient
import qualified Network.HTTP.Client.TLS                        as HttpClientTLS
import           PlutusTx                                       (FromData,
                                                                 fromData)
import qualified Servant.Client                                 as Servant
import qualified Servant.Client.Core                            as Servant

import qualified Cardano.Api                                    as Api
import qualified Cardano.Api.Ledger                             as Ledger
import qualified Cardano.Api.Shelley                            as Api
import           Cardano.Ledger.Conway.PParams                  (ConwayPParams (..),
                                                                 THKD (..))
import qualified Cardano.Ledger.Conway.PParams                  as Ledger
import qualified Cardano.Ledger.Plutus                          as Ledger
import           Cardano.Slotting.Slot                          (EpochNo (..),
                                                                 EpochSize (..))
import           Cardano.Slotting.Time                          (RelativeTime (RelativeTime),
                                                                 mkSlotLength)
import           Control.Exception                              (Exception)
import           Data.Bifunctor                                 (first)
import           Data.Ratio                                     ((%))
import           Data.SOP.NonEmpty                              (NonEmpty (NonEmptyCons, NonEmptyOne))
import           GeniusYield.Imports                            (Identity)
import           GeniusYield.Types                              (GYNetworkId (..))
import           GeniusYield.Types.Datum                        (GYDatum,
                                                                 datumFromApi')
import           GeniusYield.Types.Script                       (mintingPolicyIdToText)
import           GeniusYield.Types.Value                        (GYAssetClass (..),
                                                                 tokenNameToHex)
import qualified Ouroboros.Consensus.Cardano.Block              as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History           as Ouroboros
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (eraGenesisWin))
import           Test.Cardano.Ledger.Core.Rational              (unsafeBoundRational)

deriving newtype instance Num EpochSize
deriving newtype instance Num EpochNo

data SomeDeserializeError
    = DeserializeErrorBech32 !Api.Bech32DecodeError
    | DeserializeErrorAeson !Text
    | DeserializeErrorAssetClass !Text
    | DeserializeErrorScriptDataJson !Api.ScriptDataJsonError
    -- Api.RawBytesHexError isn't exported; use that if it gets exported
    -- https://github.com/input-output-hk/cardano-node/issues/4579
    | DeserializeErrorHex !Text
    | DeserializeErrorAddress
    | DeserializeErrorImpossibleBranch !Text
    deriving stock (Eq, Show)

newtype SubmitTxException = SubmitTxException Text
  deriving stock    (Show)
  deriving anyclass (Exception)

-- FIXME: Temporary, until remote providers us with it.
plutusV3CostModels :: [Char] -> (Ledger.Language, Ledger.CostModel)
plutusV3CostModels errPath =
    ( Ledger.PlutusV3
    , either (error (errPath <> "Couldn't build PlutusV3 cost models")) id $ Ledger.mkCostModel Ledger.PlutusV3 [100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1])


-- FIXME: Temporary, until remote providers us with it.
populateMissingProtocolParameters :: GYNetworkId -> Ledger.ConwayPParams Identity era -> Ledger.ConwayPParams Identity era
populateMissingProtocolParameters nid pp =
  pp {
      cppPoolVotingThresholds = THKD $ Ledger.PoolVotingThresholds {
        pvtPPSecurityGroup = commonPoolVotingThreshold, pvtMotionNoConfidence = commonPoolVotingThreshold, pvtHardForkInitiation = commonPoolVotingThreshold, pvtCommitteeNormal = commonPoolVotingThreshold, pvtCommitteeNoConfidence = commonPoolVotingThreshold}
    , cppDRepVotingThresholds = THKD $ Ledger.DRepVotingThresholds {dvtUpdateToConstitution = unsafeBoundRational (75 % 100), dvtTreasuryWithdrawal = unsafeBoundRational (67 % 100), dvtPPTechnicalGroup = unsafeBoundRational (67 % 100), dvtPPNetworkGroup = unsafeBoundRational (67 % 100), dvtPPGovGroup = unsafeBoundRational (75 % 100), dvtPPEconomicGroup = unsafeBoundRational (67 % 100), dvtMotionNoConfidence = unsafeBoundRational (67 % 100), dvtHardForkInitiation = unsafeBoundRational (6 % 10), dvtCommitteeNormal = unsafeBoundRational (67 % 100), dvtCommitteeNoConfidence = unsafeBoundRational (6 % 10)}
    , cppCommitteeMinSize = THKD $ case nid of
        GYMainnet        -> 7
        GYTestnetPreprod -> 7
        GYTestnetPreview -> 0
        _anyOther        -> error "cppCommitteeMinSize: unsupported network id"
    , cppCommitteeMaxTermLength = THKD $ Ledger.EpochInterval $ case nid of
        GYMainnet -> 146
        GYTestnetPreprod -> 146
        GYTestnetPreview -> 365
        _anyOther -> error "cppCommitteeMaxTermLength: unsupported network id"
    , cppGovActionLifetime = THKD $ Ledger.EpochInterval $ case nid of
        GYMainnet        -> 6
        GYTestnetPreprod -> 6
        GYTestnetPreview -> 30
        _anyOther        -> error "cppGovActionLifetime: unsupported network id"
    , cppGovActionDeposit = THKD $ Ledger.Coin 100000000000
    , cppDRepDeposit = THKD $ Ledger.Coin 500000000
    , cppDRepActivity = THKD $ Ledger.EpochInterval 20
    , cppMinFeeRefScriptCostPerByte = THKD $ unsafeBoundRational 15
    }
  where
    commonPoolVotingThreshold = unsafeBoundRational (51 % 100)

-- | Get datum from bytes.
datumFromCBOR :: Text -> Either SomeDeserializeError GYDatum
datumFromCBOR d = do
  bs  <- fromEither $ BS16.decode $ Text.encodeUtf8 d
  api <- fromEither $ Api.deserialiseFromCBOR Api.AsHashableScriptData bs
  return $ datumFromApi' api
  where
    e = DeserializeErrorHex d

    fromEither :: Either e a -> Either SomeDeserializeError a
    fromEither = first $ const e

{- | Remove request headers info from returned ClientError.

This is used as quick and simple way to hide confidential information such as API token.
-}
silenceHeadersClientError :: Servant.ClientError -> Servant.ClientError
silenceHeadersClientError (Servant.FailureResponse reqF res) = Servant.FailureResponse reqF { Servant.requestHeaders = mempty } res
silenceHeadersClientError other                              = other

-- | Creates a new Servant 'Servant.ClientEnv' from a base url.
newServantClientEnv :: String -> IO Servant.ClientEnv
newServantClientEnv baseUrl = do
    url     <- Servant.parseBaseUrl baseUrl
    manager <- if Servant.baseUrlScheme url == Servant.Https
        then HttpClient.newManager HttpClientTLS.tlsManagerSettings
        else HttpClient.newManager HttpClient.defaultManagerSettings
    pure $ Servant.mkClientEnv manager url

fromJson :: FromData a => LBS.ByteString -> Either SomeDeserializeError a
fromJson b = do
    v <- first (DeserializeErrorAeson . Text.pack) $ Aeson.eitherDecode b
    x <- first DeserializeErrorScriptDataJson $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v
    pure . fromJust . fromData $ Api.toPlutusData $ Api.getScriptData x

{- | Convert a regular list of era summaries (a la Ogmios) into a typed EraHistory (a la Ouroboros).

== NOTE ==

TODO: This must be updated with each hardfork.

See 'Ouroboros.NonEmptyCons'/'Ouroboros.Summary'/'Ouroboros.CardanoEras' types to understand
why one cannot trivially automate this.

Well, unless one uses vectors, from dependent type land.
-}
parseEraHist :: (t -> Ouroboros.EraSummary) -> [t] -> Maybe Api.EraHistory
parseEraHist mkEra [byronEra, shelleyEra, allegraEra, maryEra, alonzoEra, babbageEra, conwayEra] = Just
    . Api.EraHistory
    . Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons (mkEra byronEra)
    . NonEmptyCons (mkEra shelleyEra)
    . NonEmptyCons (mkEra allegraEra)
    . NonEmptyCons (mkEra maryEra)
    . NonEmptyCons (mkEra alonzoEra)
    . NonEmptyCons (mkEra babbageEra)
    $ NonEmptyOne (mkEra conwayEra)
parseEraHist mkEra [byronEra, shelleyEra, allegraEra, maryEra, alonzoEra, babbageEra] = Just
    . Api.EraHistory
    . Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons (mkEra byronEra)
    . NonEmptyCons (mkEra shelleyEra)
    . NonEmptyCons (mkEra allegraEra)
    . NonEmptyCons (mkEra maryEra)
    . NonEmptyCons (mkEra alonzoEra)
    $ NonEmptyOne (mkEra babbageEra)
parseEraHist _ _ = Nothing

-- FIXME: These hardcoded era histories have to be corrected to include conway.

{- | Hardcoded era history for preprod.

__NOTE:__ This is only to be used for testing.

Also see: "GeniusYield.CardanoApi.EraHistory.showEraHistory"
-}
preprodEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
preprodEraHist = Ouroboros.mkInterpreter . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    $ NonEmptyOne babbageEra
  where
    byronEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 1728000, boundSlot = 86400, boundEpoch = 4})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320, eraGenesisWin = 4320}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 1728000, boundSlot = 86400, boundEpoch = 4}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin =0}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }

previewEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
previewEraHist = Ouroboros.mkInterpreter . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    $ NonEmptyOne babbageEra
  where
    byronEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864, eraGenesisWin = 0}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 0}
            }

mainnetEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
mainnetEraHist = Ouroboros.mkInterpreter . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    $ NonEmptyOne babbageEra
  where
    byronEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 89856000, boundSlot = 4492800, boundEpoch = 208})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320, eraGenesisWin = 4320}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 89856000, boundSlot = 4492800, boundEpoch = 208}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
            }

-- | Extract currency symbol & token name part of an `GYAssetClass` when it is of such a form. When input is @Just GYLovelace@ or @Nothing@, this function returns @Nothing@.
extractAssetClass :: Maybe GYAssetClass -> Maybe (Text, Text)
extractAssetClass Nothing = Nothing
extractAssetClass (Just GYLovelace) = Nothing
extractAssetClass (Just (GYToken pid tn)) = Just (mintingPolicyIdToText pid, tokenNameToHex tn)
