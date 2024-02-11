module BetRef.Operations
  ( betRefValidator'
  , betRefAddress
  , placeBet
  , takeBets
  , addRefScript'
  , addRefInput'
  ) where

import           OnChain.Compiled
import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-- | Validator in question, obtained after giving required parameters.
betRefValidator' :: BetRefParams -> GYValidator 'PlutusV2
betRefValidator' brp = validatorFromPlutus $ betRefValidator brp

-- | Address of the validator, given params.
betRefAddress :: (HasCallStack, GYTxQueryMonad m) => BetRefParams -> m GYAddress
betRefAddress brp = scriptAddress $ betRefValidator' brp

-- | Operation to place bet.
placeBet :: (HasCallStack, GYTxMonad m)
              => GYTxOutRef         -- ^ Reference Script.
              -> BetRefParams       -- ^ Validator Params.
              -> OracleAnswerDatum  -- ^ Guess.
              -> GYValue            -- ^ Bet amount to place.
              -> GYAddress          -- ^ Own address.
              -> Maybe GYTxOutRef   -- ^ Reference to previous bets UTxO (if any).
              -> m (GYTxSkeleton 'PlutusV2)
placeBet refScript brp guess bet ownAddr mPreviousBetsUtxoRef = do
  gyLogDebug' "" $ printf "ownAddr: %s" (show ownAddr)
  gyLogDebug' "" $ printf "refOut: %s" (show mPreviousBetsUtxoRef)

  pkh <- addressToPubKeyHash' ownAddr
  betAddr <- betRefAddress brp
  case mPreviousBetsUtxoRef of
    -- This is the first bet.
    Nothing -> do
      return $ mustHaveOutput $ GYTxOut
        { gyTxOutAddress = betAddr
        , gyTxOutValue = bet
        , gyTxOutDatum = Just (datumFromPlutusData $ BetRefDatum [(pubKeyHashToPlutus pkh, guess)] (valueToPlutus bet), GYTxOutDontUseInlineDatum)
        , gyTxOutRefS    = Nothing
        }
    -- Need to append to previous.
    Just previousBetsUtxoRef -> do
      previousUtxo <- utxoAtTxOutRef' previousBetsUtxoRef
      gyLogDebug' "" $ printf "1. previousUtxo: %s" (show previousUtxo)
      (_addr, previousValue, dat@(BetRefDatum previousGuesses _previousBet)) <- utxoDatum' previousUtxo
      gyLogDebug' "" $ printf "2. previous guesses %s" (show previousGuesses)
      betUntilSlot <- enclosingSlotFromTime' (timeFromPlutus $ brpBetUntil brp)
      gyLogDebug' "" $ printf "3. bet until slot %s" (show betUntilSlot)
      return $
           input brp refScript previousBetsUtxoRef dat (Bet guess)
        <> mustHaveOutput GYTxOut
              { gyTxOutAddress = betAddr
              , gyTxOutValue = bet <> previousValue
              , gyTxOutDatum = Just (datumFromPlutusData $ BetRefDatum ((pubKeyHashToPlutus pkh, guess) : previousGuesses) (valueToPlutus bet), GYTxOutDontUseInlineDatum)
              , gyTxOutRefS    = Nothing
              }
        <> isInvalidAfter betUntilSlot
        <> mustBeSignedBy pkh

-- | Operation to take UTxO corresponding to previous bets.
takeBets :: (HasCallStack, GYTxMonad m)
              => GYTxOutRef    -- ^ Reference Script.
              -> BetRefParams  -- ^ Validator params.
              -> GYTxOutRef    -- ^ Script UTxO to consume.
              -> GYAddress     -- ^ Own address.
              -> GYTxOutRef    -- ^ Oracle reference input.
              -> m (GYTxSkeleton 'PlutusV2)
takeBets refScript brp previousBetsUtxoRef ownAddr oracleRefInput = do
  pkh <- addressToPubKeyHash' ownAddr
  previousUtxo <- utxoAtTxOutRef' previousBetsUtxoRef
  (_addr, _previousValue, dat) <- utxoDatum' previousUtxo
  betRevealSlot <- enclosingSlotFromTime' (timeFromPlutus $ brpBetReveal brp)
  return $
       input brp refScript previousBetsUtxoRef dat Take
    <> isInvalidBefore betRevealSlot
    <> mustHaveRefInput oracleRefInput
    <> mustBeSignedBy pkh

-- | Utility function to consume script UTxO.
input :: BetRefParams -> GYTxOutRef -> GYTxOutRef -> BetRefDatum -> BetRefAction -> GYTxSkeleton 'PlutusV2
input brp refScript inputRef dat red =
  mustHaveInput GYTxIn
    { gyTxInTxOutRef = inputRef
    -- , gyTxInWitness = GYTxInWitnessKey
    , gyTxInWitness  = GYTxInWitnessScript
        (GYInReference refScript $ validatorToScript $ betRefValidator' brp)
        (datumFromPlutusData dat)
        (redeemerFromPlutusData red)
    }

-- | Add UTxO to be used as reference input at a given address with given datum.
addRefInput' :: GYAddress -> OracleAnswerDatum -> GYTxSkeleton 'PlutusV2
addRefInput' addr dat =
  mustHaveOutput $ GYTxOut addr mempty (Just (datumFromPlutusData dat, GYTxOutUseInlineDatum)) Nothing
  -- Note that the value can be empty as tx building logic would add the needed minimum UTxO ada.

-- | Add Reference Script UTxO.
addRefScript' :: GYAddress -> GYValidator 'PlutusV2 -> GYTxSkeleton 'PlutusV2
addRefScript' addr script = mustHaveOutput $ GYTxOut addr mempty (Just (datumFromPlutusData (), GYTxOutDontUseInlineDatum)) (Just $ validatorToScript script)