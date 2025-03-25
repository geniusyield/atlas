{- |
Module      : GeniusYield.Transaction.CoinSelection
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Transaction.CoinSelection (
  GYBalancedTx (..),
  GYTxInDetailed (..),
  GYCoinSelectionEnv (..),
  GYCoinSelectionStrategy (..),
  selectInputs,
) where

import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (eraProtVerHigh)
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (
  ExceptT (ExceptT),
  except,
 )
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Set qualified as S
import GHC.Word (Word64)
import GeniusYield.Imports
import GeniusYield.Transaction.CoinSelection.Balance
import GeniusYield.Transaction.CoinSelection.UTxOIndex (UTxOIndex)
import GeniusYield.Transaction.CoinSelection.UTxOIndex qualified as UTxOIndex
import GeniusYield.Transaction.CoinSelection.UTxOSelection qualified as UTxOSelection
import GeniusYield.Transaction.Common
import GeniusYield.Types
import GeniusYield.Utils

{- Note: The vast majority of partial functions in this module are fine since they are localized.

Essentially, they are supposed to work on trusted inputs and outputs. As such, we should pay more
attention in the input and output sites and ensure they are indeed constructed/deconstructed properly.
-}

data GYCoinSelectionEnv v = GYCoinSelectionEnv
  { existingInputs :: ![GYTxInDetailed v]
  -- ^ List of existing inputs that must be used.
  , requiredOutputs :: ![(GYAddress, GYValue)]
  -- ^ Outputs to pay for.
  , mintValue :: !GYValue
  -- ^ Value minted in the transaction.
  , changeAddr :: GYAddress
  -- ^ Address where change outputs will be sent to.
  , ownUtxos :: !GYUTxOs
  -- ^ Set of own utxos to select additional inputs from.
  , extraLovelace :: !Natural
  -- ^ Extra lovelace to look for on top of outputs, mainly for fee and any remaining would be given as change output by @makeTransactionBodyAutoBalance@, thus amount remaining besides fees, should satisfy minimum ada requirement else we would have to increase `extraLovelace` parameter.
  , minimumUTxOF :: GYTxOut v -> Natural
  , maxValueSize :: Natural
  , adaSource :: Natural
  , adaSink :: Natural
  , inputMapper :: GYUTxO -> GYTxInDetailed v
  }

data GYCoinSelectionStrategy
  = GYLargestFirstMultiAsset
  | GYRandomImproveMultiAsset
  | GYLegacy
  deriving stock (Eq, Show, Enum, Bounded)

instance Default GYCoinSelectionStrategy where
  def = GYRandomImproveMultiAsset

{- | Select additional inputs from the set of own utxos given, such that when combined with given existing inputs,
they cover for all the given outputs, as well as extraLovelace.

Return the list of additional inputs chosen and the change outputs created.

== Invariant ==

The 'ownUtxos' and 'requiredOutputs' arguments passed must contain non negative 'GYValue's, with each one containing
a positive amount of ada.
-}
selectInputs ::
  forall m v.
  MonadRandom m =>
  GYCoinSelectionEnv v ->
  GYCoinSelectionStrategy ->
  ExceptT GYBalancingError m ([GYTxInDetailed v], [GYTxOut v])
selectInputs
  GYCoinSelectionEnv
    { existingInputs = existingInputs'
    , requiredOutputs
    , mintValue
    , changeAddr
    , ownUtxos
    , extraLovelace
    , minimumUTxOF
    , adaSource
    , adaSink
    , inputMapper
    }
  GYLegacy = do
    additionalInputForReplayProtection <-
      except $
        if existingInputs' == mempty -- For replay protection, every transaction must spend at least one UTxO.
        -- We pick the UTxO having most value.
          then
            let ownUtxosList = utxosToList ownUtxos
             in case ownUtxosList of
                  [] -> Left GYBalancingErrorEmptyOwnUTxOs
                  _ -> pure . pure $ inputMapper $ maximumBy (compare `on` utxoValue) ownUtxosList
          else pure Nothing
    let
      additionalInputForReplayProtectionAsList = maybe [] pure additionalInputForReplayProtection
      existingInputs = additionalInputForReplayProtectionAsList <> existingInputs'
      valueIn, valueOut :: GYValue
      valueIn = foldMap gyTxInDetValue existingInputs <> valueFromLovelace (fromIntegral adaSource)
      valueOut = foldMap snd requiredOutputs <> valueFromLovelace (fromIntegral adaSink)
      valueMissing = missing (valueFromLovelace (fromIntegral extraLovelace) <> valueOut `valueMinus` (valueIn <> mintValue))
    (addIns, addVal) <-
      except $
        selectInputsLegacy
          ownUtxos
          valueMissing
          inputMapper
          existingInputs
    let valueIn' = valueIn <> addVal
        tokenChange = removeAda $ (valueIn' <> mintValue) `valueMinus` valueOut
        changeOuts =
          [ adjustTxOut minimumUTxOF (GYTxOut changeAddr tokenChange Nothing Nothing)
          | not $ isEmptyValue tokenChange
          ]
    pure (additionalInputForReplayProtectionAsList <> addIns, changeOuts)
   where
    missing :: GYValue -> Map GYAssetClass Natural
    missing v = foldl' f Map.empty $ valueToList v
     where
      f :: Map GYAssetClass Natural -> (GYAssetClass, Integer) -> Map GYAssetClass Natural
      f m (ac, n)
        | n <= 0 = m
        | otherwise = Map.insert ac (fromIntegral n) m

    removeAda :: GYValue -> GYValue
    removeAda = snd . valueSplitAda
selectInputs
  GYCoinSelectionEnv
    { existingInputs
    , requiredOutputs
    , mintValue
    , changeAddr
    , ownUtxos
    , extraLovelace
    , minimumUTxOF
    , maxValueSize
    , adaSource
    , adaSink
    , inputMapper
    }
  cstrat = do
    SelectionResult
      { inputsSelected
      , changeGenerated
      } <-
      modifyException fromWalletBalancingError
        . ExceptT
        $ performSelection
          selectionConstraints
          selectionParams
    let inRefs = S.fromList $ gyTxInTxOutRef . gyTxInDet <$> existingInputs
        changeOuts =
          map
            (\tokenChange -> GYTxOut changeAddr tokenChange Nothing Nothing)
            changeGenerated
        foldHelper acc (txIn, _)
          | txIn `S.member` inRefs = acc
          | otherwise = case utxosLookup txIn ownUtxos of
              {- Invariant: The balancer should only select inputs from 'existingInputs' or 'ownUtxos'
              Thus, if this txIn doesn't exist in ownUtxos, it must already be in 'existingInputs',
              so we don't need it in additional inputs -}
              Nothing -> acc
              Just utxo -> inputMapper utxo : acc
        -- Set of additional inputs chosen by the balancer that should be added to the transaction.
        addIns = foldl' foldHelper [] inputsSelected
    pure (addIns, changeOuts)
   where
    selectionConstraints =
      SelectionConstraints
        { valueSizeAssessor =
            valueSizeAssessor' maxValueSize
        , computeMinimumAdaQuantity = \addr tkMap -> do
            -- This function is ran for generated change outputs which do not have datum & reference script.
            -- This first parameter can actually be ignored as it will always be @toCWalletAddress changeAddr@.
            minimumUTxOF
              GYTxOut
                { gyTxOutAddress = addr
                , gyTxOutValue = tkMap
                , gyTxOutDatum = Nothing
                , gyTxOutRefS = Nothing
                }
        , {- This field essentially takes care of tx fees.

          For simplicity, we simply use the extraLovelace parameter.
          -}
          computeMinimumCost = const extraLovelace
        , maximumOutputAdaQuantity = 45_000_000_000_000_000
        , maximumOutputTokenQuantity = fromIntegral $ maxBound @Word64
        , changeAddress = changeAddr
        , nullAddress = addressFromCredential GYMainnet (GYCredentialByKey "00000000000000000000000000000000000000000000000000000000") Nothing
        }
    selectionParams =
      SelectionParams
        { assetsToMint = mintedVal
        , assetsToBurn = burnedVal
        , extraCoinSource = adaSource
        , extraCoinSink = adaSink
        , outputsToCover = requiredOutputs
        , utxoAvailable = UTxOSelection.fromIndexPair (ownUtxosIndex, existingInpsIndex) -- `fromIndexPair` would actually make first element to be @ownUtxosIndex `UTxOIndex.difference` existingInpsIndex@.
        , selectionStrategy = case cstrat of
            GYRandomImproveMultiAsset -> SelectionStrategyOptimal
            _ -> SelectionStrategyMinimal
        }
    (mintedVal, burnedVal) = valueSplitSign mintValue
    ownUtxosIndex = utxosToUtxoIndex ownUtxos
    existingInpsIndex = txInDetailedToUtxoIndex existingInputs

computeTokenBundleSerializedLengthBytes :: GYValue -> Natural
computeTokenBundleSerializedLengthBytes =
  safeCast
    . BS.length
    . CBOR.serialize' (eraProtVerHigh @Conway)
    . Api.S.toMaryValue
    . valueToApi
 where
  safeCast :: Int -> Natural
  safeCast = fromIntegral

selectInputsLegacy ::
  forall v.
  -- | Set of own utxos to select additional inputs from.
  GYUTxOs ->
  -- | Target value total inputs must sum up to.
  Map GYAssetClass Natural ->
  (GYUTxO -> GYTxInDetailed v) ->
  -- | List of existing inputs that must be used.
  [GYTxInDetailed v] ->
  Either GYBalancingError ([GYTxInDetailed v], GYValue)
selectInputsLegacy ownUtxos targetOut inputMapper existingIns = go targetOut [] mempty $ utxosToList ownUtxos
 where
  inRefs = map (gyTxInTxOutRef . gyTxInDet) existingIns
  ownValueMap :: Map GYTxOutRef GYValue
  ownValueMap = mapUTxOs utxoValue ownUtxos

  go :: Map GYAssetClass Natural -> [GYTxInDetailed v] -> GYValue -> [GYUTxO] -> Either GYBalancingError ([GYTxInDetailed v], GYValue)
  go m addIns addVal _
    | Map.null m = Right (addIns, addVal)
  go m _ _ [] = Left $ GYBalancingErrorInsufficientFunds $ valueFromList [(ac, toInteger n) | (ac, n) <- Map.toList m]
  go m addIns addVal (utxo : ys)
    | utxoRef utxo `elem` inRefs = go m addIns addVal ys
    | otherwise =
        let v = ownValueMap Map.! utxoRef utxo
            m' = foldl' f m $ valueToList v
             where
              f :: Map GYAssetClass Natural -> (GYAssetClass, Integer) -> Map GYAssetClass Natural
              f m'' (ac, n) =
                let
                  o = fromIntegral n
                 in
                  case Map.lookup ac m'' of
                    Nothing -> m''
                    Just n'
                      | n' <= o -> Map.delete ac m''
                      | otherwise -> Map.insert ac (n' - o) m''
         in if m' == m
              then go m addIns addVal ys
              else
                go
                  m'
                  (inputMapper utxo : addIns)
                  (addVal <> v)
                  ys

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

valueSizeAssessor' :: Natural -> ValueSizeAssessor
valueSizeAssessor' maxSize = assessTokenBundleSize
 where
  assessTokenBundleSize tb
    | serializedLengthBytes <= maxSize =
        ValueSizeWithinLimit
    | otherwise =
        ValueSizeExceedsLimit
   where
    serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

utxosToUtxoIndex :: GYUTxOs -> UTxOIndex UTxO
utxosToUtxoIndex = UTxOIndex.fromSequence . map utxoToTuple . utxosToList

utxoToTuple :: GYUTxO -> (UTxO, GYValue)
utxoToTuple
  GYUTxO
    { utxoRef
    , utxoValue
    } = (wUtxo, bundle)
   where
    wUtxo = utxoRef
    bundle = utxoValue

txInDetailedToUtxoIndex :: [GYTxInDetailed v] -> UTxOIndex UTxO
txInDetailedToUtxoIndex = UTxOIndex.fromSequence . map txInDetailedToTuple

txInDetailedToTuple :: GYTxInDetailed v -> (UTxO, GYValue)
txInDetailedToTuple
  GYTxInDetailed
    { gyTxInDet
    , gyTxInDetValue
    } = (wUtxo, bundle)
   where
    wUtxo = gyTxInTxOutRef gyTxInDet
    bundle = gyTxInDetValue

fromWalletBalancingError :: SelectionBalanceError -> GYBalancingError
fromWalletBalancingError (BalanceInsufficient (BalanceInsufficientError _ _ delta)) =
  GYBalancingErrorInsufficientFunds delta
fromWalletBalancingError (UnableToConstructChange (UnableToConstructChangeError _ n)) =
  GYBalancingErrorChangeShortFall n
fromWalletBalancingError EmptyUTxO = GYBalancingErrorEmptyOwnUTxOs
