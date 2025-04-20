{- |
Module      : GeniusYield.Types.TxBody
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxBody (
  -- * Transaction body
  GYTxBody,

  -- * Conversions
  txBodyFromApi,
  txBodyToApi,

  -- * Transaction creation
  signGYTxBody,
  signGYTxBody',
  signTx,
  unsignedTx,
  makeSignedTransaction,
  makeSignedTransaction',
  appendWitnessGYTx,
  signGYTx,
  signGYTx',

  -- * Functions
  txBodyFromHex,
  txBodyFromHexBS,
  txBodyFromCBOR,
  txBodyToHex,
  txBodyToHexBS,
  txBodyFee,
  txBodyFeeValue,
  txBodyUTxOs,
  txBodyUTxOsWithDatums,
  txBodyTxIns,
  txBodyTxInsReference,
  txBodyTxId,
  txBodyToApiTxBodyContent,
  txBodyReqSignatories,
  txBodyMintValue,
  txBodyValidityRange,
  txBodyCollateral,
  txBodyCollateralReturnOutput,
  txBodyCollateralReturnOutputValue,
  txBodyTotalCollateralLovelace,
  getTxBody,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Coin qualified as Ledger
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BS8
import Data.Set qualified as Set

import GeniusYield.Imports
import GeniusYield.Types.Datum (GYDatum)
import GeniusYield.Types.Era
import GeniusYield.Types.Key (GYSomeSigningKey (GYSomeSigningKey))
import GeniusYield.Types.Key.Class (
  ToShelleyWitnessSigningKey,
  toShelleyWitnessSigningKey,
 )
import GeniusYield.Types.PubKeyHash (GYPubKeyHash, pubKeyHashFromApi)
import GeniusYield.Types.Slot
import GeniusYield.Types.Tx
import GeniusYield.Types.TxOutRef
import GeniusYield.Types.UTxO
import GeniusYield.Types.Value

-- | Transaction body: the part which is then signed.
newtype GYTxBody = GYTxBody (Api.TxBody ApiEra)
  deriving Show
  deriving newtype Eq

txBodyFromApi :: Api.TxBody ApiEra -> GYTxBody
txBodyFromApi = coerce

txBodyToApi :: GYTxBody -> Api.TxBody ApiEra
txBodyToApi = coerce

-- | Sign a transaction body with (potentially) multiple keys.
signGYTxBody :: ToShelleyWitnessSigningKey a => GYTxBody -> [a] -> GYTx
signGYTxBody = signTx

{-# DEPRECATED signTx "Use signGYTxBody." #-}
signTx :: ToShelleyWitnessSigningKey a => GYTxBody -> [a] -> GYTx
signTx (GYTxBody txBody) skeys =
  txFromApi
    $ Api.signShelleyTransaction
      Api.ShelleyBasedEraConway
      txBody
    $ map toShelleyWitnessSigningKey skeys

-- | Sign a transaction body with (potentially) multiple keys of potentially different nature.
signGYTxBody' :: GYTxBody -> [GYSomeSigningKey] -> GYTx
signGYTxBody' (txBodyToApi -> txBody) skeys =
  txFromApi
    $ Api.signShelleyTransaction
      Api.ShelleyBasedEraConway
      txBody
    $ map (\(GYSomeSigningKey a) -> toShelleyWitnessSigningKey a) skeys

-- | Make a signed transaction given the transaction body & list of key witnesses, represented in `GYTxWitness`.
makeSignedTransaction :: GYTxWitness -> GYTxBody -> GYTx
makeSignedTransaction txWit txBody = makeSignedTransaction' (txWitToKeyWitnessApi txWit) $ txBodyToApi txBody

-- | Make a signed transaction given the transaction body & list of key witnesses.
makeSignedTransaction' :: [Api.S.KeyWitness ApiEra] -> Api.TxBody ApiEra -> GYTx
makeSignedTransaction' = fmap txFromApi <$> Api.makeSignedTransaction

-- | Add a key witness(s) to a transaction, represented in `GYTxWitness`, which might already have previous key witnesses.
appendWitnessGYTx :: GYTxWitness -> GYTx -> GYTx
appendWitnessGYTx = appendWitnessGYTx' . txWitToKeyWitnessApi

-- | Add a key witness(s) to a transaction, which might already have previous key witnesses.
appendWitnessGYTx' :: [Api.S.KeyWitness ApiEra] -> GYTx -> GYTx
appendWitnessGYTx' appendKeyWitnessList previousTx =
  let (txBody, previousKeyWitnessesList) = Api.S.getTxBodyAndWitnesses $ txToApi previousTx
   in makeSignedTransaction' (previousKeyWitnessesList ++ appendKeyWitnessList) txBody

-- | Sign a transaction with (potentially) multiple keys and add your witness(s) among previous key witnesses, if any.
signGYTx :: ToShelleyWitnessSigningKey a => GYTx -> [a] -> GYTx
signGYTx previousTx skeys = signGYTx'' previousTx $ map toShelleyWitnessSigningKey skeys

-- | Sign a transaction with (potentially) multiple keys and add your witness(s) among previous key witnesses, if any.
signGYTx'' :: GYTx -> [Api.ShelleyWitnessSigningKey] -> GYTx
signGYTx'' previousTx skeys =
  -- Though could have been written in terms of `appendWitnessGYTx'`
  -- but that would duplicate work to obtain @txBody@ as it's also
  -- required here to get for `appendKeyWitnessList`.
  let (txBody, previousKeyWitnessesList) = Api.S.getTxBodyAndWitnesses $ txToApi previousTx
      appendKeyWitnessList = map (Api.makeShelleyKeyWitness Api.ShelleyBasedEraConway txBody) skeys
   in makeSignedTransaction' (previousKeyWitnessesList ++ appendKeyWitnessList) txBody

-- | Sign a transaction with (potentially) multiple keys of potentially different nature and add your witness(s) among previous key witnesses, if any.
signGYTx' :: GYTx -> [GYSomeSigningKey] -> GYTx
signGYTx' previousTx skeys = signGYTx'' previousTx (map (\(GYSomeSigningKey a) -> toShelleyWitnessSigningKey a) skeys)

-- | Create an unsigned transaction from the body.
unsignedTx :: GYTxBody -> GYTx
unsignedTx (GYTxBody body) = txFromApi (Api.Tx body [])

-- | Get `GYTxBody` from it's /hex/ CBOR encoding given as `String`. Note that the given serialized input is not of form @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyFromHex :: String -> Maybe GYTxBody
txBodyFromHex = rightToMaybe . txBodyFromHexBS . BS8.pack

-- | Get `GYTxBody` from it's /hex/ CBOR encoding given as `ByteString`. Note that the given serialized input is not of form @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyFromHexBS :: BS.ByteString -> Either String GYTxBody
txBodyFromHexBS bs = BS16.decode bs >>= txBodyFromCBOR

-- | Get `GYTxBody` from it's CBOR encoding. Note that the given serialized input is not of form @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyFromCBOR :: BS.ByteString -> Either String GYTxBody
txBodyFromCBOR = fmap txBodyFromApi . first show . Api.deserialiseFromCBOR (Api.AsTxBody Api.AsConwayEra)

-- | Serialise `GYTxBody` to get hex encoded CBOR string represented as `String`. Obtained result does not correspond to @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyToHex :: GYTxBody -> String
txBodyToHex = BS8.unpack . txBodyToHexBS

-- | Serialise `GYTxBody` to get hex encoded CBOR string represented as `ByteString`. Obtained result does not correspond to @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyToHexBS :: GYTxBody -> BS.ByteString
txBodyToHexBS = BS16.encode . txBodyToCBOR

-- | Serialise `GYTxBody` to get CBOR bytestring. Obtained result does not correspond to @transaction_body@ as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) but rather it's the serialisation of Cardano API library's `TxBody` type.
txBodyToCBOR :: GYTxBody -> BS.ByteString
txBodyToCBOR = Api.serialiseToCBOR . txBodyToApi

-- | Return the fees in lovelace.
txBodyFee :: GYTxBody -> Integer
txBodyFee (GYTxBody (Api.getTxBodyContent -> Api.TxBodyContent {Api.txFee = fee})) =
  case fee of
    Api.TxFeeExplicit _ (Ledger.Coin actual) -> actual

-- | Return the fees as 'GYValue'.
txBodyFeeValue :: GYTxBody -> GYValue
txBodyFeeValue = valueFromLovelace . txBodyFee

-- | Return utxos created by tx (body).
txBodyUTxOs :: GYTxBody -> GYUTxOs
txBodyUTxOs = utxosFromList . fmap fst . txBodyUTxOsWithDatums

txBodyUTxOsWithDatums :: GYTxBody -> [(GYUTxO, Maybe GYDatum)]
txBodyUTxOsWithDatums (GYTxBody body@(Api.getTxBodyContent -> Api.TxBodyContent {txOuts})) =
  zipWith f [0 ..] txOuts
 where
  txId = Api.getTxId body

  f :: Word -> Api.TxOut Api.CtxTx ApiEra -> (GYUTxO, Maybe GYDatum)
  f i = utxoFromApiWithDatum (Api.TxIn txId (Api.TxIx i))

-- | Returns the 'GYTxOutRef' consumed by the tx.
txBodyTxIns :: GYTxBody -> [GYTxOutRef]
txBodyTxIns (GYTxBody (Api.getTxBodyContent -> Api.TxBodyContent {txIns})) = map (txOutRefFromApi . fst) txIns

-- | Returns the 'GYTxOutRef' for the reference inputs present in the tx.
txBodyTxInsReference :: GYTxBody -> [GYTxOutRef]
txBodyTxInsReference (GYTxBody (Api.getTxBodyContent -> Api.TxBodyContent {txInsReference})) = case txInsReference of
  Api.TxInsReferenceNone -> []
  Api.TxInsReference Api.S.BabbageEraOnwardsConway inRefs -> map txOutRefFromApi inRefs

-- | Returns the 'GYTxId' of the given 'GYTxBody'.
txBodyTxId :: GYTxBody -> GYTxId
txBodyTxId = txIdFromApi . Api.getTxId . txBodyToApi

-- | Returns the 'GYTxBody' of the given 'GYTx'.
getTxBody :: GYTx -> GYTxBody
getTxBody = txBodyFromApi . Api.getTxBody . txToApi

txBodyToApiTxBodyContent :: GYTxBody -> Api.TxBodyContent Api.ViewTx ApiEra
txBodyToApiTxBodyContent body = let bc = txBodyToApi body & Api.getTxBodyContent in bc

-- | Returns the required signatories of the given 'GYTxBody'.
txBodyReqSignatories :: GYTxBody -> Set.Set GYPubKeyHash
txBodyReqSignatories body = case Api.txExtraKeyWits $ txBodyToApiTxBodyContent body of
  Api.TxExtraKeyWitnessesNone -> mempty
  Api.TxExtraKeyWitnesses _ reqPKHs -> Set.fromList $ pubKeyHashFromApi <$> reqPKHs

-- | Returns the mint 'GYValue' of the given 'GYTxBody'.
txBodyMintValue :: GYTxBody -> GYValue
txBodyMintValue body = valueFromApi $ Api.txMintValueToValue $ Api.txMintValue $ txBodyToApiTxBodyContent body

-- | Returns the validity range of the given 'GYTxBody'.
txBodyValidityRange :: GYTxBody -> (Maybe GYSlot, Maybe GYSlot)
txBodyValidityRange body =
  let cnt = txBodyToApiTxBodyContent body
   in case (Api.txValidityLowerBound cnt, Api.txValidityUpperBound cnt) of
        (lb, ub) -> (f lb, g ub)
 where
  f :: Api.TxValidityLowerBound ApiEra -> Maybe GYSlot
  f Api.TxValidityNoLowerBound = Nothing
  f (Api.TxValidityLowerBound _ sn) = Just $ slotFromApi sn

  g :: Api.TxValidityUpperBound ApiEra -> Maybe GYSlot
  g (Api.TxValidityUpperBound _ Nothing) = Nothing
  g (Api.TxValidityUpperBound _ (Just sn)) = Just $ slotFromApi sn

-- | Returns the set of 'GYTxOutRef' used as collateral in the given 'GYTxBody'.
txBodyCollateral :: GYTxBody -> Set GYTxOutRef
txBodyCollateral body = case Api.txInsCollateral $ txBodyToApiTxBodyContent body of
  Api.TxInsCollateralNone -> Set.empty
  Api.TxInsCollateral _ xs -> Set.fromList $ txOutRefFromApi <$> xs

-- | Returns the total collateral for the given transaction body.
txBodyTotalCollateralLovelace :: GYTxBody -> Natural
txBodyTotalCollateralLovelace body = case Api.txTotalCollateral $ txBodyToApiTxBodyContent body of
  Api.TxTotalCollateralNone -> 0
  Api.TxTotalCollateral _ (Ledger.Coin l)
    | l >= 0 -> fromInteger l
    | otherwise -> error $ "negative total collateral: " <> show l

txBodyCollateralReturnOutput :: GYTxBody -> Api.TxReturnCollateral Api.CtxTx ApiEra
txBodyCollateralReturnOutput body = Api.txReturnCollateral $ txBodyToApiTxBodyContent body

txBodyCollateralReturnOutputValue :: GYTxBody -> GYValue
txBodyCollateralReturnOutputValue body =
  case Api.txReturnCollateral $ txBodyToApiTxBodyContent body of
    Api.TxReturnCollateralNone -> mempty
    Api.TxReturnCollateral _ (Api.TxOut _ v _ _) -> valueFromApi $ Api.txOutValueToValue v
