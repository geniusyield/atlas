{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GeniusYield.OnChain.BetRef (
  mkBetRefValidator,
  OracleAnswerDatum (..),
  BetRefParams (..),
  BetRefDatum (..),
  BetRefAction (..),
) where

import GeniusYield.OnChain.BetRef.Types
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (
  findDatum,
  findOwnInput,
  getContinuingOutputs,
 )
import PlutusTx.Prelude as PlutusTx

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

  outputToDatum :: FromData b => TxOut -> Maybe b
  outputToDatum o = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum d -> processDatum d
    OutputDatumHash dh -> processDatum =<< findDatum dh info
   where
    processDatum = fromBuiltinData . getDatum
