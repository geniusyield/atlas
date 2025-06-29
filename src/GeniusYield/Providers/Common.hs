{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GeniusYield.Providers.Common
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Providers.Common (
  SomeDeserializeError (..),
  SubmitTxException (..),
  datumFromCBOR,
  newServantClientEnv,
  newManager,
  fromJson,
  makeLastEraEndUnbounded,
  parseEraHist,
  preprodEraHist,
  previewEraHist,
  mainnetEraHist,
  silenceHeadersClientError,
  extractAssetClass,
  extractNonAdaToken,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTLS
import PlutusTx (
  FromData,
  fromData,
 )
import Servant.Client qualified as Servant
import Servant.Client.Core qualified as Servant

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api
import Cardano.Slotting.Slot (
  EpochNo (..),
  EpochSize (..),
 )
import Cardano.Slotting.Time (
  RelativeTime (RelativeTime),
  mkSlotLength,
 )
import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.SOP.NonEmpty (NonEmpty (NonEmptyCons, NonEmptyOne), nonEmptyFromList, nonEmptyToList)
import GeniusYield.CardanoApi.EraHistory (extractEraSummaries)
import GeniusYield.Types.Datum (
  GYDatum,
  datumFromApi',
 )
import GeniusYield.Types.Script (mintingPolicyIdToText)
import GeniusYield.Types.Value (
  GYAssetClass (..),
  GYNonAdaToken (..),
  nonAdaTokenFromAssetClass,
  tokenNameToHex,
 )
import Ouroboros.Consensus.Cardano.Block qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History.EraParams (EraParams (eraGenesisWin))

deriving newtype instance Num EpochSize
deriving newtype instance Num EpochNo

data SomeDeserializeError
  = DeserializeErrorBech32 !Api.Bech32DecodeError
  | DeserializeErrorAeson !Text
  | DeserializeErrorAssetClass !Text
  | DeserializeErrorScriptDataJson !Api.ScriptDataJsonError
  | -- Api.RawBytesHexError isn't exported; use that if it gets exported
    -- https://github.com/input-output-hk/cardano-node/issues/4579
    DeserializeErrorHex !Text
  | DeserializeErrorAddress
  | DeserializeErrorImpossibleBranch !Text
  deriving stock (Eq, Show)

newtype SubmitTxException = SubmitTxException Text
  deriving stock Show
  deriving anyclass Exception

-- | Get datum from bytes.
datumFromCBOR :: Text -> Either SomeDeserializeError GYDatum
datumFromCBOR d = do
  bs <- fromEither $ BS16.decode $ Text.encodeUtf8 d
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
silenceHeadersClientError (Servant.FailureResponse reqF res) = Servant.FailureResponse reqF {Servant.requestHeaders = mempty} res
silenceHeadersClientError other = other

-- | Creates a new Servant 'Servant.ClientEnv' from a base url.
newServantClientEnv :: String -> IO Servant.ClientEnv
newServantClientEnv baseUrl = do
  url <- Servant.parseBaseUrl baseUrl
  manager <- newManager url
  pure $ Servant.mkClientEnv manager url

newManager :: Servant.BaseUrl -> IO HttpClient.Manager
newManager url =
  if Servant.baseUrlScheme url == Servant.Https
    then HttpClient.newManager HttpClientTLS.tlsManagerSettings
    else HttpClient.newManager HttpClient.defaultManagerSettings

fromJson :: FromData a => LBS.ByteString -> Either SomeDeserializeError a
fromJson b = do
  v <- first (DeserializeErrorAeson . Text.pack) $ Aeson.eitherDecode b
  x <- first DeserializeErrorScriptDataJson $ Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema v
  pure . fromJust . fromData $ Api.toPlutusData $ Api.getScriptData x

makeLastEraEndUnbounded :: Api.EraHistory -> Api.EraHistory
makeLastEraEndUnbounded eh =
  let Ouroboros.Summary eraList = extractEraSummaries eh
   in Api.EraHistory $ Ouroboros.mkInterpreter $ Ouroboros.Summary $ g eraList
 where
  g eraList =
    let eraList' = nonEmptyToList eraList
        f [] = []
        f [x] =
          let oldEraParams = Ouroboros.eraParams x
           in [x {Ouroboros.eraEnd = Ouroboros.EraUnbounded, Ouroboros.eraParams = oldEraParams {Ouroboros.eraSafeZone = Ouroboros.UnsafeIndefiniteSafeZone}}]
        f (x : xs) = x : f xs
     in fromJust $ nonEmptyFromList $ f eraList'

{- | Convert a regular list of era summaries (a la Ogmios) into a typed EraHistory (a la Ouroboros).

== NOTE ==

TODO: This must be updated with each hardfork.

See 'Ouroboros.NonEmptyCons'/'Ouroboros.Summary'/'Ouroboros.CardanoEras' types to understand
why one cannot trivially automate this.

Well, unless one uses vectors, from dependent type land.
-}
parseEraHist :: (t -> Ouroboros.EraSummary) -> [t] -> Maybe Api.EraHistory
parseEraHist mkEra [byronEra, shelleyEra, allegraEra, maryEra, alonzoEra, babbageEra, conwayEra] =
  Just
    . makeLastEraEndUnbounded
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
parseEraHist _ _ = Nothing

{- | Hardcoded era history for preprod.

__NOTE:__ This is only to be used for testing.

Also see: "GeniusYield.CardanoApi.EraHistory.showEraHistory"
-}
preprodEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
preprodEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
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
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  babbageEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 5184000, boundSlot = 3542400, boundEpoch = 12}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 70416000, boundSlot = 68774400, boundEpoch = 163})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 70416000, boundSlot = 68774400, boundEpoch = 163}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }

previewEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
previewEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
 where
  byronEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864, eraGenesisWin = 864}
      }
  shelleyEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  allegraEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  maryEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  alonzoEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  babbageEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 259200, boundSlot = 259200, boundEpoch = 3}
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 55814400, boundSlot = 55814400, boundEpoch = 646})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 55814400, boundSlot = 55814400, boundEpoch = 646}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920, eraGenesisWin = 25920}
      }

mainnetEraHist :: Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
mainnetEraHist =
  Ouroboros.mkInterpreter
    . Ouroboros.Summary
    . NonEmptyCons byronEra
    . NonEmptyCons shelleyEra
    . NonEmptyCons allegraEra
    . NonEmptyCons maryEra
    . NonEmptyCons alonzoEra
    . NonEmptyCons babbageEra
    $ NonEmptyOne conwayEra
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
      , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 219024000, boundSlot = 133660800, boundEpoch = 507})
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }
  conwayEra =
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime = RelativeTime 219024000, boundSlot = 133660800, boundEpoch = 507}
      , eraEnd = Ouroboros.EraUnbounded
      , eraParams = Ouroboros.EraParams {eraEpochSize = 432000, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 129600, eraGenesisWin = 129600}
      }

-- | Extract currency symbol & token name part of an `GYAssetClass` when it is of such a form. When input is @Just GYLovelace@ or @Nothing@, this function returns @Nothing@.
extractAssetClass :: Maybe GYAssetClass -> Maybe (Text, Text)
extractAssetClass mac = do
  ac <- mac
  nat <- nonAdaTokenFromAssetClass ac
  pure $ extractNonAdaToken nat

-- | Extract currency symbol & token name part of a `GYNonAdaToken`.
extractNonAdaToken :: GYNonAdaToken -> (Text, Text)
extractNonAdaToken (GYNonAdaToken pid tn) = (mintingPolicyIdToText pid, tokenNameToHex tn)
