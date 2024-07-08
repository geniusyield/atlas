{-|
Module      : GeniusYield.Transaction.CBOR
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

The transaction CBOR as obtained by Cardano API library may not be in format as desired by some hardware/browser wallets. This module attempts to simplify this obtained CBOR to the manner more acceptable.

Review this file whenever a hardfork occurs.

-}
module GeniusYield.Transaction.CBOR (
    CborSimplificationError (..)
  , simplifyGYTxBodyCbor
  , simplifyTxCbor
  ) where


import           Codec.CBOR.Read        (DeserialiseFailure,
                                         deserialiseFromBytes)
import           Codec.CBOR.Term        (Term (..), decodeTerm, encodeTerm)
import           Codec.CBOR.Write       (toStrictByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.Lazy         as LT (toStrict)

import           GeniusYield.Imports
import           GeniusYield.Types.Tx
import           GeniusYield.Types.TxBody (GYTxBody, unsignedTx, getTxBody)

-- TODO: Make a log before performing this simplification?

data CborSimplificationError =
    TransactionDeserialisationError !DeserialiseFailure
  | TransactionHasLeftOver !Text
  | TransactionIsAbsurd !Text
  | ModifiedTransactionDoesntDeserialise !Text
  deriving stock (Eq, Show)

{- | Function to simplify transaction CBOR.

We as of now, only modify @transaction_body@ (terms are as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl)) in it.

The modifications done is as follows:-

* All indefinite-length items are made definite.
* Wherever map occurs, it's keys are sorted as required by [CIP 21](https://cips.cardano.org/cips/cip21/).

-}
simplifyTxCbor :: GYTx -> Either CborSimplificationError GYTx
simplifyTxCbor tx = do
  let txCBOR = txToCBOR tx
  (leftOver, term) <- first TransactionDeserialisationError $ deserialiseFromBytes decodeTerm $ LBS.fromStrict txCBOR
  when (leftOver /= mempty) $ Left $ TransactionHasLeftOver $ TE.decodeUtf8 $ BS16.encode $ LBS.toStrict leftOver
  case term of
    TList (txBody : otherFields) -> do
      let txBody' = simplifyTxBodyCbor txBody
      first (ModifiedTransactionDoesntDeserialise . T.pack)  $ txFromCBOR $ toStrictByteString $ encodeTerm $ TList (txBody' : otherFields)
    _other                       -> Left $ TransactionIsAbsurd "Transaction is defined as list but received otherwise"

{- | Function to modify our CBOR tree according to the given function. If the given function returns `Nothing` it means, it is not applicable to the given `term` and thus we recurse down. Note that the given function (say @f@) must satisfy the following property:-

If @f a = Just b@ then @f b = Just b@ /OR/ @f b = Nothing@.
-}
recursiveTermModification :: (Term -> Maybe Term) -> Term -> Term
recursiveTermModification f term =
  case term of
    -- We handle recursive cases first, what is left then are the base cases.
    TList termList -> recursiveTermModificationHandler $ TList $ recursiveTermModification f <$> termList
    TListI termList -> recursiveTermModificationHandler $ TListI $ recursiveTermModification f <$> termList
    TMap termPairList -> recursiveTermModificationHandler $ TMap $ bimap (recursiveTermModification f) (recursiveTermModification f) <$> termPairList
    TMapI termPairList -> recursiveTermModificationHandler $ TMapI $ bimap (recursiveTermModification f) (recursiveTermModification f) <$> termPairList
    TTagged word otherTerm -> recursiveTermModificationHandler $ TTagged word $ recursiveTermModification f otherTerm
    _otherwise -> recursiveTermModificationHandler term
  where
    recursiveTermModificationHandler nothingHandler =
      case f term of
        Nothing -> nothingHandler
        Just termMod -> if term == termMod then nothingHandler else recursiveTermModification f termMod

-- | See `simplifyTxCbor`.
simplifyTxBodyCbor :: Term -> Term
simplifyTxBodyCbor txBody =
      -- First, we'll make indefinite-length items, definite.
  let txBodyDefinite = recursiveTermModification makeTermsDefinite txBody
      -- Second, we'll sort keys in any map.
      txBodySortedKeys = recursiveTermModification sortMapKeys txBodyDefinite
  in txBodySortedKeys

  where

    sortMapKeys :: Term -> Maybe Term
    sortMapKeys (TMap keyValsToSort) =
      if allSameType then
        Just $ TMap $ sortBy sortingFunction keyValsToSort
      else Nothing
      where
        sortingFunction :: forall b1 b2. (Term, b1) -> (Term, b2) -> Ordering
        sortingFunction (TInt a, _) (TInt b, _)         = compare a b
        sortingFunction (TInteger a, _) (TInteger b, _) = compare a b
        sortingFunction (TBytes a, _) (TBytes b, _)     = compare (B.length a) (B.length b) <> compare a b
        sortingFunction (TString a, _) (TString b, _)   = compare (T.length a) (T.length b) <> compare a b
        sortingFunction _ _                             = error "absurd - sortingFunction"  -- We verify that all keys are of the same appropriate type before calling this function.
        allSameType = any ($ keyValsToSort) [isTInt, isTInteger, isTBytes, isTString]
          where
            isTInt = all (\(k, _) -> case k of TInt _ -> True; _ow -> False)
            isTInteger = all (\(k, _) -> case k of TInteger _ -> True; _ow -> False)
            isTBytes = all (\(k, _) -> case k of TBytes _ -> True; _ow -> False)
            isTString = all (\(k, _) -> case k of TString _ -> True; _ow -> False)
    sortMapKeys _otherwise     = Nothing

    makeTermsDefinite :: Term -> Maybe Term
    makeTermsDefinite (TBytesI b)     = Just $ TBytes $ LBS.toStrict b
    makeTermsDefinite (TStringI s)    = Just $ TString $ LT.toStrict s
    makeTermsDefinite (TListI l)      = Just $ TList l
    makeTermsDefinite (TMapI keyVals) = Just $ TMap keyVals
    makeTermsDefinite _otherwise      = Nothing

{- | This `GYTxBody` doesn't represent @transaction_body@ as mentioned in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) specification, it's API's internal type to represent transaction without signing key witnesses. However `GYTx` does represent `transaction` as defined in specification. We therefore obtain `GYTx` and work with it. Here we need an invariant, which is if we receive our simplified `GYTx` transaction, then obtaining `GYTxBody` via `getTxBody` and obtaining `GYTx` back via `unsignedTx` should have the same serialisation for the modifications to CBOR encoding we do here.
-}
simplifyGYTxBodyCbor :: GYTxBody -> Either CborSimplificationError GYTxBody
simplifyGYTxBodyCbor txBody =
  let tx = unsignedTx txBody
  in getTxBody <$> simplifyTxCbor tx
