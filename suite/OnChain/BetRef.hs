{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module OnChain.BetRef
  ( mkBetRefValidator
  , OracleAnswerDatum (..)
  , BetRefParams (..)
  , BetRefDatum (..)
  , BetRefAction (..)
  ) where

import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, findOwnInput, findDatum)
import qualified PlutusTx
import           PlutusTx.Prelude          as PlutusTx
import           Prelude                   (Show)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V1.Address (toPubKeyHash)

-- | Goals made my the concerned team.
type TeamGoals = Integer

-- | Match result given by the oracle.
newtype OracleAnswerDatum = OracleAnswerDatum TeamGoals deriving newtype (Eq, Show)
PlutusTx.unstableMakeIsData ''OracleAnswerDatum

-- | Our contract is parameterized with this.
data BetRefParams = BetRefParams
  { brpOraclePkh :: PubKeyHash  -- ^ Oracle's payment public key hash. This is needed to assert that UTxO being looked at indeed belongs to the Oracle.
  , brpBetUntil  :: POSIXTime   -- ^ Time until which bets can be placed.
  , brpBetReveal :: POSIXTime   -- ^ Time at which Oracle will reveal the correct match result.
  , brpBetStep   :: Value       -- ^ Each newly placed bet must be more than previous bet by `brpBetStep` amount.
  }
PlutusTx.makeLift ''BetRefParams

-- | List of guesses by users along with the maximum bet placed yet. A new guess gets /prepended/ to this list. Note that since we are always meant to increment previously placed bet with `brpBetStep`, the newly placed bet would necessarily be maximum (it would be foolish to initialize `brpBetStep` with some negative amounts).
data BetRefDatum = BetRefDatum
  { brdBets        :: [(PubKeyHash, OracleAnswerDatum)]
  , brdPreviousBet :: Value
  }
PlutusTx.unstableMakeIsData ''BetRefDatum

-- | Redeemer representing choices available to the user.
data BetRefAction = Bet !OracleAnswerDatum  -- ^ User makes a guess.
                  | Take                    -- ^ User takes the pot.
PlutusTx.unstableMakeIsData ''BetRefAction

{-# INLINABLE mkBetRefValidator #-}
-- | Untyped wrapper around `mkBetRefValidator'`.
mkBetRefValidator :: BetRefParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBetRefValidator params dat' red' ctx'
  | mkBetRefValidator' params (unsafeFromBuiltinData dat') (unsafeFromBuiltinData red') (unsafeFromBuiltinData ctx') = ()
  | otherwise                     = error ()

{-# INLINABLE mkBetRefValidator' #-}
-- | Core smart contract logic. Read its description from Atlas guide.
mkBetRefValidator' :: BetRefParams -> BetRefDatum -> BetRefAction -> ScriptContext -> Bool
mkBetRefValidator' (BetRefParams oraclePkh betUntil betReveal betStep) (BetRefDatum previousGuesses previousBet) brAction ctx =
  case brAction of
    Bet guess  ->
      let
        sOut = case getContinuingOutputs ctx of
          [sOut']        -> sOut'
          _anyOtherMatch -> traceError "Expected only one continuing output."
        outValue = txOutValue sOut
        sIn = maybe (traceError "Could not find own input") txInInfoResolved (findOwnInput ctx)
        inValue = txOutValue sIn
        (guessesOut, betOut) = case outputToDatum sOut of
          Nothing                                -> traceError "Could not resolve for script output datum"
          Just (BetRefDatum guessesOut' betOut') -> (guessesOut', betOut')
      in
        traceIfFalse
          "Must be before `BetUntil` time"
            (to betUntil `contains` validRange) &&
        traceIfFalse
          "Guesses update is wrong"
            ((signerPkh, guess) : previousGuesses == guessesOut) &&
        traceIfFalse
          "The current bet must be more than the previous bet by atleast `brpBetStep` amount"
            (outValue `geq` (inValue <> previousBet <> betStep)) &&
        traceIfFalse
          "Out bet is wrong"
            (betOut == outValue - inValue)
    Take ->
      let
        -- Note that `find` returns the first match. Since we were always prepending, this is valid.
        Just guess = find ((== signerPkh) . fst) previousGuesses
        oracleIn = case filter (isNothing . txOutReferenceScript) (txInInfoResolved <$> txInfoReferenceInputs info) of
          [oracleIn']    -> oracleIn'
          []             -> traceError "No reference input provided"
          _anyOtherMatch -> traceError "Expected only one reference input"
        oracleAnswer = case outputToDatum oracleIn of
          Nothing                                  -> traceError "Could not resolve for datum"
          (Just (OracleAnswerDatum oracleAnswer')) -> oracleAnswer'
        guessDiff = getGuessDiff $ snd guess
        getGuessDiff (OracleAnswerDatum g) = abs (oracleAnswer - g)
        oracleInPkh = case toPubKeyHash (txOutAddress oracleIn) of
          Nothing  -> traceError "Not PKH for oracle address"
          Just pkh -> pkh
      in
        traceIfFalse
          "Must be after `RevealTime`"
            (from betReveal `contains` validRange) &&
        traceIfFalse
          "Must fully spend Script"
            (null (getContinuingOutputs ctx)) &&
        traceIfFalse
          "Reference input must be from Oracle address (wrt Payment part)"
            (oracleInPkh == oraclePkh) &&
        traceIfFalse
          "Guess is not closest"
            (all (\pg -> getGuessDiff (snd pg) >= guessDiff) previousGuesses)
  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    validRange :: POSIXTimeRange
    validRange = txInfoValidRange info

    signerPkh :: PubKeyHash
    signerPkh = case txInfoSignatories info of
      [signerPkh']   -> signerPkh'
      []             -> traceError "No signatory"
      _anyOtherMatch -> traceError "Expected only one signatory"

    outputToDatum :: FromData b => TxOut -> Maybe b
    outputToDatum o = case txOutDatum o of
      NoOutputDatum      -> Nothing
      OutputDatum d      -> processDatum d
      OutputDatumHash dh -> processDatum =<< findDatum dh info
      where processDatum = fromBuiltinData . getDatum