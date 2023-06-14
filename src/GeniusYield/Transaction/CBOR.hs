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
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           GeniusYield.Imports
import           GeniusYield.Types

-- TODO: Make a log before performing this simplification.

data CborSimplificationError =
    TransactionDeserialisationError !DeserialiseFailure
  | TransactionHasLeftOver !Text
  | TransactionIsAbsurd !Text
  | ModifiedTransactionDoesntDeserialise !Text
  deriving stock (Eq, Show)

{- | Function to simplify transaction CBOR.

We as of now, only modify @transaction_body@ (terms are as defined in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl)) in it.

The modifications done is as follows:-

* @transaction_output@ is put up into @legacy_transaction_output@ format when possible.
* Wherever map occurs, it's keys are sorted as required by [CIP 21](https://cips.cardano.org/cips/cip21/).

-}
simplifyTxCbor :: GYTx -> Either CborSimplificationError GYTx
simplifyTxCbor tx = do
  let txCBOR = txToCBOR tx
  (leftOver, term) <- first TransactionDeserialisationError $ deserialiseFromBytes decodeTerm $ LBS.fromStrict txCBOR
  when (leftOver /= mempty) $ Left $ TransactionHasLeftOver $ TE.decodeUtf8 $ BS16.encode $ LBS.toStrict leftOver
  case term of
    TList (txBody : otherFields) -> do
      txBody' <- simplifyTxBodyCbor txBody
      first (ModifiedTransactionDoesntDeserialise . T.pack)  $ txFromCBOR $ toStrictByteString $ encodeTerm $ TList (txBody' : otherFields)
    _other                       -> Left $ TransactionIsAbsurd "Transaction is defined as list but received otherwise"

{- | Function to modify our CBOR tree according to the given function. If the given function returns `Nothing` it means, it is not applicable to the given `term` and thus we recurse down. Note that the given function (say @f@) must satisfy the following property:-

If @f a = Just b@ then @f b = Just b@.
-}
recursiveTermModification :: (Term -> Maybe Term) -> Term -> Term
recursiveTermModification f term =
  case term of
    -- We handle recursive cases first, what is left then are the base cases.
    TList termList -> recursiveTermModificationHandler $ TList $ recursiveTermModification f <$> termList
    TListI termList -> recursiveTermModificationHandler $ TListI $ recursiveTermModification f <$> termList
    TMap termPairList -> recursiveTermModificationHandler $ TMap $ bimap (recursiveTermModification f) (recursiveTermModification f) <$> termPairList
    TTagged word otherTerm -> recursiveTermModificationHandler $ TTagged word $ recursiveTermModification f otherTerm
    _otherwise -> recursiveTermModificationHandler term
  where
    recursiveTermModificationHandler nothingHandler =
      case f term of
        Nothing -> nothingHandler
        Just termMod -> if term == termMod then nothingHandler else recursiveTermModification f termMod

simplifyTxBodyCbor :: Term -> Either CborSimplificationError Term
simplifyTxBodyCbor (TMap keyVals) = do
      -- First we'll simplify the outputs.
  let txBodySimplifiedOutputs = TMap $ findAndSimplifyOutputs keyVals
      -- Second, we'll sort keys in any map.
      txBodySortedKeys = recursiveTermModification sortMapKeys txBodySimplifiedOutputs
      txBodyFinal = txBodySortedKeys
  pure txBodyFinal

  where

    findAndSimplifyOutputs :: [(Term, Term)] -> [(Term, Term)]
    findAndSimplifyOutputs [] = []
    findAndSimplifyOutputs ((key, value) : remainingKeyVals) =
      (key,
            if key == TInt 1 then simplifyOutputs value
            else if key == TInt 16 then simplifyOutput value
            else value
      ) : findAndSimplifyOutputs remainingKeyVals
      where

        simplifyOutputs :: Term -> Term
        simplifyOutputs (TList outputs) = TList $ map simplifyOutput outputs
        simplifyOutputs notAList        = notAList  -- We can return error here.

        simplifyOutput :: Term -> Term
        simplifyOutput (TMap [(TInt 0, addr), (TInt 1, amount)]) = TList [addr, amount]
        simplifyOutput ow = ow

    sortMapKeys :: Term -> Maybe Term
    sortMapKeys (TMap keyValsToSort) =
      if all (\(k, _) -> case k of TInt _ -> True; _ow -> False) keyValsToSort then
        Just $ TMap $ sortBy sortingFunction keyValsToSort
      else Nothing
      where
        sortingFunction :: forall b1 b2. (Term, b1) -> (Term, b2) -> Ordering
        sortingFunction (TInt a, _) (TInt b, _) = compare a b
        sortingFunction _ _                     = error "absurd"  -- We verify that all keys are of the form @TInt _@ before calling this function.
    sortMapKeys _otherwise     = Nothing

simplifyTxBodyCbor _otherwise            = Left $ TransactionIsAbsurd "Transaction body must be of type 'map'"

{- | This `GYTxBody` doesn't represent @transaction_body@ as mentioned in [CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/babbage/test-suite/cddl-files/babbage.cddl) specification, it's API's internal type to represent transaction without signing key witnesses. However `GYTx` does represent `transaction` as defined in specification. We therefore obtain `GYTx` and work with it. Here we need an invariant, which is if we receive our simplified `GYTx` transaction, then obtaining `GYTxBody` via `getTxBody` and obtaining `GYTx` back via `unsignedTx` should have the same serialisation.
-}
simplifyGYTxBodyCbor :: GYTxBody -> Either CborSimplificationError GYTxBody
simplifyGYTxBodyCbor txBody =
  let tx = unsignedTx txBody
  in getTxBody <$> simplifyTxCbor tx

