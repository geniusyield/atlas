{-|
Module      : GeniusYield.Providers.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.Common (
    SomeDeserializeError (..)
    , newServantClientEnv
    , scriptDataToData
    , fromJson
    , parseEraHist
    , babbageProtocolVersion
    , preprodEraHist
    , previewEraHist
    , mainnetEraHist
    , silenceHeadersClientError
) where

import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Maybe                           (fromJust)
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import           Numeric.Natural                      (Natural)

import qualified Network.HTTP.Client                  as HttpClient
import qualified Network.HTTP.Client.TLS              as HttpClientTLS
import           PlutusTx                             (Data (..), FromData,
                                                       fromData)
import qualified Servant.Client                       as Servant
import qualified Servant.Client.Core                  as Servant

import qualified Cardano.Api                          as Api
import           Cardano.Slotting.Time                (RelativeTime (RelativeTime),
                                                       mkSlotLength)
import           Data.Bifunctor                       (first)
import qualified Ouroboros.Consensus.Cardano.Block    as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import           Ouroboros.Consensus.Util.Counting    (NonEmpty (NonEmptyCons, NonEmptyOne))

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

scriptDataToData :: Api.ScriptData -> Data
scriptDataToData (Api.ScriptDataConstructor n xs) = Constr n $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataMap xs)           = Map [(scriptDataToData x, scriptDataToData y) | (x, y) <- xs]
scriptDataToData (Api.ScriptDataList xs)          = List $ scriptDataToData <$> xs
scriptDataToData (Api.ScriptDataNumber n)         = I n
scriptDataToData (Api.ScriptDataBytes bs)         = B bs

fromJson :: FromData a => LBS.ByteString -> Either SomeDeserializeError a
fromJson b = do
    v <- first (DeserializeErrorAeson . Text.pack) $ Aeson.eitherDecode b
    x <- first DeserializeErrorScriptDataJson $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v
    pure . fromJust . fromData $ scriptDataToData x

{- | Convert a regular list of era summaries (a la Ogmios) into a typed EraHistory (a la Ouroboros).

== NOTE ==

This must be updated with each hardfork.

See 'Ouroboros.NonEmptyCons'/'Ouroboros.Summary'/'Ouroboros.CardanoEras' types to understand
why one cannot trivially automate this.

Well, unless one uses vectors, from dependent type land.
-}
parseEraHist :: (t -> Ouroboros.EraSummary) -> [t] -> Maybe (Api.EraHistory Api.CardanoMode)
parseEraHist mkEra [byronEra, shelleyEra, allegraEra, maryEra, alonzoEra, babbageEra] = Just
    . Api.EraHistory Api.CardanoMode
    . Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons (mkEra byronEra)
    . NonEmptyCons (mkEra shelleyEra)
    . NonEmptyCons (mkEra allegraEra)
    . NonEmptyCons (mkEra maryEra)
    . NonEmptyCons (mkEra alonzoEra)
    $ NonEmptyOne (mkEra babbageEra)
parseEraHist _ _ = Nothing

-- | Hardcoded babbage protocol Version
babbageProtocolVersion :: Natural
babbageProtocolVersion = 7

{- | Hardcoded era history for preprod.
FIXME: Remove this hack as it shouldn't be hardcoded.

See: "GeniusYield.CardanoApi.EraHistory.showEraHistory"
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
            , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 1728000, boundSlot = 86400, boundEpoch = 4}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2160000, boundSlot = 518400, boundEpoch = 5}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 2592000, boundSlot = 950400, boundEpoch = 6}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 3024000, boundSlot = 1382400, boundEpoch = 7}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
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
            , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
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
            , eraParams = Ouroboros.EraParams {eraEpochSize = 21600, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 4320}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 89856000, boundSlot = 4492800, boundEpoch = 208}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 101952000, boundSlot = 16588800, boundEpoch = 236}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 108432000, boundSlot = 23068800, boundEpoch = 251}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 125280000, boundSlot = 39916800, boundEpoch = 290}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
    babbageEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 157680000, boundSlot = 72316800, boundEpoch = 365}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600}
            }
