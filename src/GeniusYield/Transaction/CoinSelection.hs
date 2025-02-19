-- To work on this module, module [@Cardano.CoinSelection.Balance@](https://github.com/cardano-foundation/cardano-wallet/blob/master/lib/coin-selection/lib/Cardano/CoinSelection/Balance.hs) should be understood.

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

import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (
  ExceptT (ExceptT),
  except,
 )
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Text.Class (
  ToText (toText),
  fromText,
 )
import GHC.IsList (fromList)

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.CoinSelection.Balance qualified as CBalance
import Cardano.CoinSelection.Context qualified as CCoinSelection
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Conway (Conway)

import Cardano.CoinSelection.Size qualified as CWallet
import Cardano.CoinSelection.UTxOIndex qualified as CWallet
import Cardano.CoinSelection.UTxOSelection qualified as CWallet
import Cardano.Wallet.Primitive.Types.Address qualified as CWallet
import Cardano.Wallet.Primitive.Types.AssetId qualified as CTokenBundle
import Cardano.Wallet.Primitive.Types.AssetName qualified as CWallet
import Cardano.Wallet.Primitive.Types.Coin qualified as CWallet
import Cardano.Wallet.Primitive.Types.Hash qualified as CWallet
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as CTokenBundle
import Cardano.Wallet.Primitive.Types.TokenMap qualified as CWTokenMap
import Cardano.Wallet.Primitive.Types.TokenPolicyId qualified as CWallet
import Cardano.Wallet.Primitive.Types.TokenQuantity qualified as CWallet
import Cardano.Wallet.Primitive.Types.Tx.Constraints qualified as CWallet

import Cardano.Ledger.Conway.Core (eraProtVerHigh)
import GeniusYield.Imports
import GeniusYield.Transaction.Common
import GeniusYield.Types
import GeniusYield.Utils

type GYCoinSelectionContext :: PlutusVersion -> Type
data GYCoinSelectionContext v

type WalletUTxO = GYTxOutRef

{- Note: The vast majority of partial functions in this module are fine since they are localized.

Essentially, they are supposed to work on trusted inputs and outputs. As such, we should pay more
attention in the input and output sites and ensure they are indeed constructed/deconstructed properly.
-}

{-
TODO:
- What should "max length address" be?
- Can we simply use a stripped down 'GYTxIn' for wallet utxo? maybe just GYTxOutRef even.
-}
instance CCoinSelection.SelectionContext (GYCoinSelectionContext v) where
  type Address (GYCoinSelectionContext v) = CWallet.Address
  type UTxO (GYCoinSelectionContext v) = WalletUTxO

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
                  _ -> pure . pure $ utxoAsPubKeyInp $ maximumBy (compare `on` utxoValue) ownUtxosList
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
    }
  cstrat = do
    CBalance.SelectionResult
      { inputsSelected
      , changeGenerated
      } <-
      modifyException fromCWalletBalancingError
        . ExceptT
        $ CBalance.performSelection @_ @(GYCoinSelectionContext v)
          selectionConstraints
          selectionParams
    let inRefs = S.fromList $ gyTxInTxOutRef . gyTxInDet <$> existingInputs
        changeOuts =
          map
            (\(fromTokenBundle -> tokenChange) -> GYTxOut changeAddr tokenChange Nothing Nothing)
            changeGenerated
        foldHelper acc (txIn, _)
          | txIn `S.member` inRefs = acc
          | otherwise = case utxosLookup txIn ownUtxos of
              {- Invariant: The balancer should only select inputs from 'existingInputs' or 'ownUtxos'
              Thus, if this txIn doesn't exist in ownUtxos, it must already be in 'existingInputs',
              so we don't need it in additional inputs -}
              Nothing -> acc
              Just utxo -> utxoAsPubKeyInp utxo : acc
        -- Set of additional inputs chosen by the balancer that should be added to the transaction.
        addIns = foldl' foldHelper [] inputsSelected
    pure (addIns, changeOuts)
   where
    selectionConstraints =
      CBalance.SelectionConstraints
        { tokenBundleSizeAssessor =
            tokenBundleSizeAssessor $
              CWallet.TxSize maxValueSize
        , computeMinimumAdaQuantity = \addr tkMap -> do
            -- This function is ran for generated change outputs which do not have datum & reference script.
            -- This first parameter can actually be ignored as it will always be @toCWalletAddress changeAddr@.
            CWallet.Coin $
              minimumUTxOF
                GYTxOut
                  { gyTxOutAddress = fromCWalletAddress addr
                  , gyTxOutValue = fromTokenMap tkMap
                  , gyTxOutDatum = Nothing
                  , gyTxOutRefS = Nothing
                  }
        , {- This field essentially takes care of tx fees.

          For simplicity, we simply use the extraLovelace parameter.
          -}
          computeMinimumCost = const $ CWallet.Coin extraLovelace
        , maximumOutputAdaQuantity = CWallet.txOutMaxCoin
        , maximumOutputTokenQuantity = CWallet.txOutMaxTokenQuantity
        , maximumLengthChangeAddress = toCWalletAddress changeAddr -- Since our change address is fixed.
        , nullAddress = CWallet.Address ""
        }
    selectionParams =
      CBalance.SelectionParams
        { assetsToMint = toTokenMap mintedVal
        , assetsToBurn = toTokenMap burnedVal
        , extraCoinSource = CWallet.Coin adaSource
        , extraCoinSink = CWallet.Coin adaSink
        , outputsToCover = map (bimap toCWalletAddress toTokenBundle) requiredOutputs
        , utxoAvailable = CWallet.fromIndexPair (ownUtxosIndex, existingInpsIndex) -- `fromIndexPair` would actually make first element to be @ownUtxosIndex `UTxOIndex.difference` existingInpsIndex@.
        , selectionStrategy = case cstrat of
            GYRandomImproveMultiAsset -> CBalance.SelectionStrategyOptimal
            _ -> CBalance.SelectionStrategyMinimal
        }
    (mintedVal, burnedVal) = valueSplitSign mintValue
    ownUtxosIndex = utxosToUtxoIndex ownUtxos
    existingInpsIndex = txInDetailedToUtxoIndex existingInputs

computeTokenBundleSerializedLengthBytes :: CTokenBundle.TokenBundle -> CWallet.TxSize
computeTokenBundleSerializedLengthBytes =
  CWallet.TxSize
    . safeCast
    . BS.length
    . CBOR.serialize' (eraProtVerHigh @Conway)
    . Api.S.toMaryValue
    . toCardanoValue
 where
  safeCast :: Int -> Natural
  safeCast = fromIntegral

selectInputsLegacy ::
  -- | Set of own utxos to select additional inputs from.
  GYUTxOs ->
  -- | Target value total inputs must sum up to.
  Map GYAssetClass Natural ->
  -- | List of existing inputs that must be used.
  [GYTxInDetailed v] ->
  Either GYBalancingError ([GYTxInDetailed v], GYValue)
selectInputsLegacy ownUtxos targetOut existingIns = go targetOut [] mempty $ utxosToList ownUtxos
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
                  (utxoAsPubKeyInp utxo : addIns)
                  (addVal <> v)
                  ys

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

utxoAsPubKeyInp :: GYUTxO -> GYTxInDetailed v
utxoAsPubKeyInp GYUTxO {utxoRef, utxoAddress, utxoValue, utxoOutDatum, utxoRefScript} =
  GYTxInDetailed
    { -- It is assumed the 'GYUTxOs' arg designates key wallet utxos.
      gyTxInDet = GYTxIn utxoRef GYTxInWitnessKey
    , gyTxInDetAddress = utxoAddress
    , gyTxInDetValue = fst $ valueSplitSign utxoValue
    , gyTxInDetDatum = utxoOutDatum
    , gyTxInDetScriptRef = utxoRefScript
    }

tokenBundleSizeAssessor :: CWallet.TxSize -> CWallet.TokenBundleSizeAssessor
tokenBundleSizeAssessor maxSize = CWallet.TokenBundleSizeAssessor {..}
 where
  assessTokenBundleSize tb
    | serializedLengthBytes <= maxSize =
        CWallet.TokenBundleSizeWithinLimit
    | otherwise =
        CWallet.TokenBundleSizeExceedsLimit
   where
    serializedLengthBytes :: CWallet.TxSize
    serializedLengthBytes = computeTokenBundleSerializedLengthBytes tb

toCardanoValue :: CTokenBundle.TokenBundle -> Api.S.Value
toCardanoValue tb =
  fromList $
    (Api.S.AdaAssetId, coinToQuantity coin)
      : map (bimap toCardanoAssetId toQuantity) bundle
 where
  (coin, bundle) = CTokenBundle.toFlatList tb
  toCardanoAssetId (CTokenBundle.AssetId pid name) =
    Api.S.AssetId (toCardanoPolicyId pid) (toCardanoAssetName name)

  toCardanoAssetName :: CWallet.AssetName -> Api.S.AssetName
  toCardanoAssetName (CWallet.UnsafeAssetName tn) =
    either (\e -> error $ "toCardanoValue: unable to deserialise, error: " <> show e) id $
      Api.S.deserialiseFromRawBytes Api.S.AsAssetName tn

  coinToQuantity = fromIntegral . CWallet.unCoin
  toQuantity = fromIntegral . CWallet.unTokenQuantity

toCardanoPolicyId :: CWallet.TokenPolicyId -> Api.S.PolicyId
toCardanoPolicyId (CWallet.UnsafeTokenPolicyId (CWallet.Hash pid)) =
  either (\e -> error $ "toCardanoPolicyId: unable to deserialise, error: " <> show e) id $
    Api.S.deserialiseFromRawBytes Api.S.AsPolicyId pid

toTokenMap :: GYValue -> CWTokenMap.TokenMap
toTokenMap value =
  CWTokenMap.fromFlatList $
    map
      (\(ac, n) -> (toWalletAssetId ac, CWallet.TokenQuantity $ fromIntegral n))
      (valueToList value)

fromTokenMap :: CWTokenMap.TokenMap -> GYValue
fromTokenMap =
  valueFromList
    . map (bimap fromWalletAssetId (\(CWallet.TokenQuantity n) -> toInteger n))
    . CWTokenMap.toFlatList

toWalletAssetId :: GYAssetClass -> CTokenBundle.AssetId
toWalletAssetId GYLovelace = error "toWalletAssetId: unable to deserialize"
toWalletAssetId tkn@(GYToken policyId (GYTokenName tokenName)) = CTokenBundle.AssetId tokenPolicy nTokenName
 where
  tokenPolicy = either (customError tkn) id $ fromText $ mintingPolicyIdToText policyId
  nTokenName = either (customError tkn) id $ CWallet.fromByteString tokenName
  customError t = error $ printf "toWalletAssetId: unable to deserialize \n %s" t

fromWalletAssetId :: CTokenBundle.AssetId -> GYAssetClass
fromWalletAssetId (CTokenBundle.AssetId tokenPolicy nTokenName) = GYToken policyId tkName
 where
  policyId = fromRight customError $ mintingPolicyIdFromText $ toText tokenPolicy
  tkName = fromMaybe customError $ tokenNameFromBS $ CWallet.unAssetName nTokenName
  customError = error "fromWalletAssetId: unable to deserialize"

toTokenBundle :: GYValue -> CTokenBundle.TokenBundle
toTokenBundle v = CTokenBundle.fromCoin coins `CTokenBundle.add` CTokenBundle.fromTokenMap (toTokenMap tokens)
 where
  coins = fromMaybe customError $ CWallet.fromIntegralMaybe lov
  (lov, tokens) = valueSplitAda v
  customError = error "toTokenBundle: unable to deserialize"

fromTokenBundle :: CTokenBundle.TokenBundle -> GYValue
fromTokenBundle (CTokenBundle.TokenBundle (CWallet.Coin n) tkMap) = valueFromLovelace (toInteger n) <> fromTokenMap tkMap

utxosToUtxoIndex :: GYUTxOs -> CWallet.UTxOIndex WalletUTxO
utxosToUtxoIndex = CWallet.fromSequence . map utxoToTuple . utxosToList

utxoToTuple :: GYUTxO -> (WalletUTxO, CTokenBundle.TokenBundle)
utxoToTuple
  GYUTxO
    { utxoRef
    , utxoAddress
    , utxoValue
    } = (wUtxo, bundle)
   where
    wUtxo = utxoRef
    bundle = toTokenBundle utxoValue

txInDetailedToUtxoIndex :: [GYTxInDetailed v] -> CWallet.UTxOIndex WalletUTxO
txInDetailedToUtxoIndex = CWallet.fromSequence . map txInDetailedToTuple

txInDetailedToTuple :: GYTxInDetailed v -> (WalletUTxO, CTokenBundle.TokenBundle)
txInDetailedToTuple
  GYTxInDetailed
    { gyTxInDet
    , gyTxInDetValue
    } = (wUtxo, bundle)
   where
    wUtxo = gyTxInTxOutRef gyTxInDet
    bundle = toTokenBundle gyTxInDetValue

toCWalletAddress :: GYAddress -> CWallet.Address
toCWalletAddress = CWallet.Address . Api.serialiseToRawBytes . addressToApi

fromCWalletAddress :: CWallet.Address -> GYAddress
fromCWalletAddress (CWallet.Address bs) = either customError addressFromApi $ Api.deserialiseFromRawBytes Api.AsAddressAny bs
 where
  customError e = error $ "fromCWalletAddress: unable to deserialize, error: " <> show e

fromCWalletBalancingError :: CBalance.SelectionBalanceError ctx -> GYBalancingError
fromCWalletBalancingError (CBalance.BalanceInsufficient (CBalance.BalanceInsufficientError _ _ delta)) =
  GYBalancingErrorInsufficientFunds $ fromTokenBundle delta
fromCWalletBalancingError (CBalance.UnableToConstructChange (CBalance.UnableToConstructChangeError _ n)) =
  GYBalancingErrorChangeShortFall $ CWallet.unCoin n
fromCWalletBalancingError CBalance.EmptyUTxO = GYBalancingErrorEmptyOwnUTxOs
