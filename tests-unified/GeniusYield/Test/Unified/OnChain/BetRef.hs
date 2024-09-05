{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GeniusYield.Test.Unified.OnChain.BetRef (
  mkBetRefValidator,
  OracleAnswerDatum (..),
  BetRefParams (..),
  BetRefDatum (..),
  BetRefAction (..),
) where

import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (
  findDatum,
  findOwnInput,
  getContinuingOutputs,
 )
import PlutusTx qualified
import PlutusTx.Prelude as PlutusTx
import Prelude (Show)

-- | Goals made my the concerned team.
type TeamGoals = Integer

-- | Match result given by the oracle.
newtype OracleAnswerDatum = OracleAnswerDatum TeamGoals deriving newtype (Eq, Show)

PlutusTx.unstableMakeIsData ''OracleAnswerDatum

-- | Our contract is parameterized with this.
data BetRefParams = BetRefParams
  { brpOraclePkh :: PubKeyHash
  -- ^ Oracle's payment public key hash. This is needed to assert that UTxO being looked at indeed belongs to the Oracle.
  , brpBetUntil :: POSIXTime
  -- ^ Time until which bets can be placed.
  , brpBetReveal :: POSIXTime
  -- ^ Time at which Oracle will reveal the correct match result.
  , brpBetStep :: Value
  -- ^ Each newly placed bet must be more than previous bet by `brpBetStep` amount.
  }

-- PlutusTx.makeLift ''BetRefParams
PlutusTx.unstableMakeIsData ''BetRefParams

-- | List of guesses by users along with the maximum bet placed yet. A new guess gets /prepended/ to this list. Note that since we are always meant to increment previously placed bet with `brpBetStep`, the newly placed bet would necessarily be maximum (it would be foolish to initialize `brpBetStep` with some negative amounts).
data BetRefDatum = BetRefDatum
  { brdBets :: [(PubKeyHash, OracleAnswerDatum)]
  , brdPreviousBet :: Value
  }

PlutusTx.unstableMakeIsData ''BetRefDatum

-- | Redeemer representing choices available to the user.
data BetRefAction
  = -- | User makes a guess.
    Bet !OracleAnswerDatum
  | -- | User takes the pot.
    Take

PlutusTx.unstableMakeIsData ''BetRefAction

-- Note: The first argument is meant to be data encoded 'BetRefParams'.
-- Unable to use the actual type since makeLift doesn't work on it, for whatever reason....
{-# INLINEABLE mkBetRefValidator #-}

-- | Untyped wrapper around `mkBetRefValidator'`.
mkBetRefValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBetRefValidator params dat' red' ctx'
  | mkBetRefValidator' (unsafeFromBuiltinData params) (unsafeFromBuiltinData dat') (unsafeFromBuiltinData red') (unsafeFromBuiltinData ctx') = ()
  | otherwise = error ()

{-# INLINEABLE mkBetRefValidator' #-}

-- | Core smart contract logic. Read its description from Atlas guide.
mkBetRefValidator' :: BetRefParams -> BetRefDatum -> BetRefAction -> ScriptContext -> Bool
mkBetRefValidator' (BetRefParams oraclePkh betUntil betReveal betStep) (BetRefDatum previousGuesses previousBet) brAction ctx =
  case brAction of
    Bet guess ->
      let
        sOut = case getContinuingOutputs ctx of
          [sOut'] -> sOut'
          _anyOtherMatch -> traceError "Expected only one continuing output."
        outValue = txOutValue sOut
        -- Using the 'maybe' utility here makes validation fail... for some reason...
        -- Why is PlutusTx still allowed to exist?
        inValue = case findOwnInput ctx of
          Nothing -> traceError "Joever!"
          Just x -> txOutValue (txInInfoResolved x)
        -- inValue = txOutValue sIn
        (guessesOut, betOut) = case outputToDatum sOut of
          Nothing -> traceError "Could not resolve for script output datum"
          Just (BetRefDatum guessesOut' betOut') -> (guessesOut', betOut')
       in
        traceIfFalse
          "Must be before `BetUntil` time"
          (to betUntil `contains` validRange)
          && traceIfFalse
            "Guesses update is wrong"
            ((signerPkh, guess) : previousGuesses == guessesOut)
          && traceIfFalse
            "The current bet must be more than the previous bet by atleast `brpBetStep` amount"
            (outValue `geq` (inValue <> previousBet <> betStep))
          && traceIfFalse
            "Out bet is wrong"
            (inValue == outValue - betOut)
    Take ->
      let
        -- Note that `find` returns the first match. Since we were always prepending, this is valid.
        Just guess = find ((== signerPkh) . fst) previousGuesses
        oracleIn = case filter (isNothing . txOutReferenceScript) (txInInfoResolved <$> txInfoReferenceInputs info) of
          [oracleIn'] -> oracleIn'
          [] -> traceError "No reference input provided"
          _anyOtherMatch -> traceError "Expected only one reference input"
        oracleAnswer = case outputToDatum oracleIn of
          Nothing -> traceError "Could not resolve for datum"
          (Just (OracleAnswerDatum oracleAnswer')) -> oracleAnswer'
        guessDiff = getGuessDiff $ snd guess
        getGuessDiff (OracleAnswerDatum g) = abs (oracleAnswer - g)
        -- Unwrapping the 'Maybe' here to extract the 'Just' (and trace error for 'Nothing') kills PlutusTx compilation
        -- the issue is that GHC will fire the worker wrapper transformation combining this with the equality with 'oraclePkh'
        -- code down below. Which will cause issues with BuiltinByteString also being unwrapped into primitive pointers.
        -- See: https://github.com/IntersectMBO/plutus/issues/4193
        mOracleInPkh = toPubKeyHash (txOutAddress oracleIn)
       in
        traceIfFalse
          "Must be after `RevealTime`"
          (from betReveal `contains` validRange)
          && traceIfFalse
            "Must fully spend Script"
            (null (getContinuingOutputs ctx))
          && traceIfFalse
            "Reference input must be from Oracle address (wrt Payment part)"
            (mOracleInPkh == Just oraclePkh)
          && traceIfFalse
            "Guess is not closest"
            (all (\pg -> getGuessDiff (snd pg) >= guessDiff) previousGuesses)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validRange :: POSIXTimeRange
    validRange = txInfoValidRange info

    signerPkh :: PubKeyHash
    signerPkh = case txInfoSignatories info of
      [signerPkh'] -> signerPkh'
      [] -> traceError "No signatory"
      _anyOtherMatch -> traceError "Expected only one signatory"

    outputToDatum :: (FromData b) => TxOut -> Maybe b
    outputToDatum o = case txOutDatum o of
      NoOutputDatum -> Nothing
      OutputDatum d -> processDatum d
      OutputDatumHash dh -> processDatum =<< findDatum dh info
      where
        processDatum = fromBuiltinData . getDatum
