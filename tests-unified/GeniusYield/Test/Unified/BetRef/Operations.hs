module GeniusYield.Test.Unified.BetRef.Operations (
  betRefValidator',
  betRefAddress,
  placeBet,
  takeBets,
) where

import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Test.Unified.OnChain.BetRef.Compiled

-- | Validator in question, obtained after giving required parameters.
betRefValidator' :: BetRefParams -> GYValidator 'PlutusV2
betRefValidator' brp = validatorFromPlutus $ betRefValidator brp

-- | Address of the validator, given params.
betRefAddress :: (HasCallStack, GYTxQueryMonad m) => BetRefParams -> m GYAddress
betRefAddress brp = scriptAddress $ betRefValidator' brp

-- | Operation to place bet.
placeBet ::
  (HasCallStack, GYTxQueryMonad m) =>
  -- | Reference Script.
  GYTxOutRef ->
  -- | Validator Params.
  BetRefParams ->
  -- | Guess.
  OracleAnswerDatum ->
  -- | Bet amount to place.
  GYValue ->
  -- | Own address.
  GYAddress ->
  -- | Reference to previous bets UTxO (if any).
  Maybe GYTxOutRef ->
  m (GYTxSkeleton 'PlutusV2)
placeBet refScript brp guess bet ownAddr mPreviousBetsUtxoRef = do
  gyLogDebug' "" $ printf "ownAddr: %s" (show ownAddr)
  gyLogDebug' "" $ printf "refOut: %s" (show mPreviousBetsUtxoRef)

  pkh <- addressToPubKeyHash' ownAddr
  betAddr <- betRefAddress brp
  case mPreviousBetsUtxoRef of
    -- This is the first bet.
    Nothing -> do
      return $
        mustHaveOutput $
          GYTxOut
            { gyTxOutAddress = betAddr
            , gyTxOutValue = bet
            , gyTxOutDatum = Just (datumFromPlutusData $ BetRefDatum [(pubKeyHashToPlutus pkh, guess)] (valueToPlutus bet), GYTxOutDontUseInlineDatum)
            , gyTxOutRefS = Nothing
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
          <> mustHaveOutput
            GYTxOut
              { gyTxOutAddress = betAddr
              , gyTxOutValue = bet <> previousValue
              , gyTxOutDatum =
                  Just
                    ( datumFromPlutusData $ BetRefDatum ((pubKeyHashToPlutus pkh, guess) : previousGuesses) (valueToPlutus bet)
                    , GYTxOutDontUseInlineDatum
                    )
              , gyTxOutRefS = Nothing
              }
          <> isInvalidAfter betUntilSlot
          <> mustBeSignedBy pkh

-- | Operation to take UTxO corresponding to previous bets.
takeBets ::
  (HasCallStack, GYTxUserQueryMonad m) =>
  -- | Reference Script.
  GYTxOutRef ->
  -- | Validator params.
  BetRefParams ->
  -- | Script UTxO to consume.
  GYTxOutRef ->
  -- | Own address.
  GYAddress ->
  -- | Oracle reference input.
  GYTxOutRef ->
  m (GYTxSkeleton 'PlutusV2)
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
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = inputRef
      , -- , gyTxInWitness = GYTxInWitnessKey
        gyTxInWitness =
          GYTxInWitnessScript
            (GYInReference refScript $ validatorToScript $ betRefValidator' brp)
            (datumFromPlutusData dat)
            (redeemerFromPlutusData red)
      }
