module GeniusYield.OnChain.BetRef.Types (
  OracleAnswerDatum (..),
  BetRefParams (..),
  BetRefDatum (..),
  BetRefAction (..),
) where

import GHC.Generics (Generic)
import PlutusLedgerApi.V2
import PlutusTx.Blueprint
import PlutusTx.Blueprint.TH qualified
import PlutusTx.Prelude as PlutusTx
import Prelude (Show)

-- | Goals made my the concerned team.
type TeamGoals = Integer

-- | Match result given by the oracle.
newtype OracleAnswerDatum = OracleAnswerDatum TeamGoals
  deriving newtype (Eq, Show)
  deriving stock Generic
  deriving anyclass HasBlueprintDefinition

$(PlutusTx.Blueprint.TH.unstableMakeIsDataSchema ''OracleAnswerDatum)

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
  deriving stock (Show, Generic)
  deriving anyclass HasBlueprintDefinition

$(PlutusTx.Blueprint.TH.unstableMakeIsDataSchema ''BetRefParams)

-- | List of guesses by users along with the maximum bet placed yet. A new guess gets /prepended/ to this list. Note that since we are always meant to increment previously placed bet with `brpBetStep`, the newly placed bet would necessarily be maximum (it would be foolish to initialize `brpBetStep` with some negative amounts).
data BetRefDatum = BetRefDatum
  { brdBets :: [(PubKeyHash, OracleAnswerDatum)]
  , brdPreviousBet :: Value
  }
  deriving stock Generic
  deriving anyclass HasBlueprintDefinition

$(PlutusTx.Blueprint.TH.unstableMakeIsDataSchema ''BetRefDatum)

-- | Redeemer representing choices available to the user.
data BetRefAction
  = -- | User makes a guess.
    Bet !OracleAnswerDatum
  | -- | User takes the pot.
    Take
  deriving stock Generic
  deriving anyclass HasBlueprintDefinition

$(PlutusTx.Blueprint.TH.unstableMakeIsDataSchema ''BetRefAction)
