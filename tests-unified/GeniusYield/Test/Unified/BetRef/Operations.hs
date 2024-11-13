module GeniusYield.Test.Unified.BetRef.Operations (
  mkScript,
  placeBet,
  takeBets,
) where

import Cardano.Api qualified as Api
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Test.Unified.OnChain.BetRef.Compiled
import System.IO.Unsafe (unsafePerformIO)

-- import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC

import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import UntypedPlutusCore qualified as UPLC

import Cardano.Api.Shelley qualified as Api
import Control.Lens (over)
import PlutusLedgerApi.V1
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Operation to place bet.
placeBet ::
  ( HasCallStack
  , GYTxQueryMonad m
  , v `VersionIsGreaterOrEqual` 'PlutusV2
  ) =>
  -- | Reference Script output.
  GYTxOutRef ->
  -- | Script
  GYValidator v ->
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
  m (GYTxSkeleton v)
placeBet refScript script brp guess bet ownAddr mPreviousBetsUtxoRef = do
  gyLogDebug' "" $ printf "ownAddr: %s" (show ownAddr)
  gyLogDebug' "" $ printf "refOut: %s" (show mPreviousBetsUtxoRef)

  pkh <- addressToPubKeyHash' ownAddr
  betAddr <- scriptAddress $ coerce script
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
        input refScript (validatorToScript script) previousBetsUtxoRef dat (Bet guess)
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
  forall m v.
  ( HasCallStack
  , GYTxQueryMonad m
  , v `VersionIsGreaterOrEqual` 'PlutusV2
  ) =>
  -- | Reference Script output.
  GYTxOutRef ->
  -- | The script
  GYValidator v ->
  -- | Validator params.
  BetRefParams ->
  -- | Script UTxO to consume.
  GYTxOutRef ->
  -- | Own address.
  GYAddress ->
  -- | Oracle reference input.
  GYTxOutRef ->
  m (GYTxSkeleton v)
takeBets refScript script brp previousBetsUtxoRef ownAddr oracleRefInput = do
  pkh <- addressToPubKeyHash' ownAddr
  previousUtxo <- utxoAtTxOutRef' previousBetsUtxoRef
  (_addr, _previousValue, dat) <- utxoDatum' previousUtxo
  betRevealSlot <- enclosingSlotFromTime' (timeFromPlutus $ brpBetReveal brp)
  return $
    input refScript (validatorToScript script) previousBetsUtxoRef dat Take
      <> isInvalidBefore betRevealSlot
      <> mustHaveRefInput oracleRefInput
      <> mustBeSignedBy pkh

-- | Utility builder
input ::
  v `VersionIsGreaterOrEqual` 'PlutusV2 =>
  GYTxOutRef ->
  GYScript v ->
  GYTxOutRef ->
  BetRefDatum ->
  BetRefAction ->
  GYTxSkeleton v
input refScript script inputRef dat red =
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = inputRef
      , gyTxInWitness =
          GYTxInWitnessScript
            (GYInReference refScript script)
            (datumFromPlutusData dat)
            (redeemerFromPlutusData red)
      }

--------------------------------------------------------------------------------
-- Additional operations
--------------------------------------------------------------------------------

{- | Queries the cuurent slot, calculates parameters and builds
a script that is ready to be deployed.
-}
mkScript ::
  forall m (v :: PlutusVersion).
  ( GYTxQueryMonad m
  , SingPlutusVersionI v
  , Api.IsPlutusScriptLanguage (PlutusVersionToApi v)
  ) =>
  -- | How many slots betting should be open
  Integer ->
  -- | How many slots should pass before oracle reveals answer
  Integer ->
  -- | Oracle PKH
  GYPubKeyHash ->
  -- | Bet step value
  GYValue ->
  m (BetRefParams, GYValidator v)
mkScript betUntil betReveal oraclePkh betStep = do
  currSlot <- slotToInteger <$> slotOfCurrentBlock
  -- Calculate params for the script
  let betUntil' = slotFromApi $ fromInteger $ currSlot + betUntil
  let betReveal' = slotFromApi $ fromInteger $ currSlot + betReveal
  betUntilTime <- slotToBeginTime betUntil'
  betRevealTime <- slotToBeginTime betReveal'
  let params =
        BetRefParams
          (pubKeyHashToPlutus oraclePkh)
          (timeToPlutus betUntilTime)
          (timeToPlutus betRevealTime)
          (valueToPlutus betStep)
  gyLogDebug' "" $ printf "Parameters: %s" (show params)
  -- TODO: this might be improved once support for blueprints is merged.
  let s = unsafePerformIO $ do
        lookupEnv "AIKEN_BET_REF" >>= \case
          Nothing -> pure $ mkBetRefValidator params
          Just _ -> do
            putStrLn "Using Aiken-based on-chain script"
            mkBetRefValidatorExt params
  pure (params, s)

{- | Validator in question, obtained after giving required parameters.
This uses PlutusTx version of the validator
-}
mkBetRefValidator ::
  forall (v :: PlutusVersion).
  SingPlutusVersionI v =>
  BetRefParams ->
  GYValidator v
mkBetRefValidator brp = validatorFromPlutus $ betRefValidator brp

-- | Make a validator out of external UPLC envelope
mkBetRefValidatorExt ::
  forall (v :: PlutusVersion).
  SingPlutusVersionI v =>
  BetRefParams ->
  IO (GYValidator v)
mkBetRefValidatorExt BetRefParams {..} = do
  v <- readValidator @v "tests-unified/script/bet_ref_validator.plutus"
  let (Api.PlutusScriptSerialised sbs) = validatorToApi v
  let prog :: UPLCProgram = uncheckedDeserialiseUPLC sbs
  let params =
        Constr
          0
          [ toData brpOraclePkh
          , toData brpBetUntil
          , toData brpBetReveal
          , toData brpBetStep -- TODO: might be flaky
          ]
  let args = [params]
  let appliedProg = applyArguments prog args
  -- print $ Api.pretty appliedProg
  pure $ validatorFromSerialisedScript @v $ serialiseUPLC appliedProg

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

applyArguments :: UPLCProgram -> [Data] -> UPLCProgram
applyArguments p args =
  let termArgs = fmap ((,) () . PLC.mkConstant ()) args
      apply t = PLC.mkIterApp t termArgs
   in over UPLC.progTerm apply p
