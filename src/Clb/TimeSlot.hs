module Clb.TimeSlot (
  SlotConfig (..),
  slotConfigToEpochInfo,
  utcTimeToPOSIXTime,
  posixTimeToUTCTime,
  nominalDiffTimeToPOSIXTime,
  posixTimeToNominalDiffTime,
  Slot (..),
  slotToBeginPOSIXTime,
  currentSlot,
  emulatorEpochSize,
  beginningOfTime,
  toSlot,
  fromSlot,
) where

-- import PlutusTx.Prelude hiding (Eq, (<$>))
-- import Prelude (Eq, Show)
import Data.Text (Text)
import Data.Time.Clock qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Time (POSIXTime (POSIXTime, getPOSIXTime))
import Prettyprinter (Pretty (pretty), (<+>))

-- import Prelude qualified as Haskell
import Cardano.Ledger.Slot qualified as L
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Time (slotLengthFromMillisec)
import PlutusTx.Prelude (divide)

{- | Datatype to configure the length (ms) of one slot and the beginning of the
 first slot.

 `cardano-slotting` `EpochInfo` is not serializable.
 See: https://github.com/IntersectMBO/cardano-base/issues/469
-}
data SlotConfig = SlotConfig
  { scSlotLength :: Integer
  -- ^ Length (number of milliseconds) of one slot
  , scSlotZeroTime :: POSIXTime
  -- ^ Beginning of slot 0 (in milliseconds)
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SlotConfig where
  pretty SlotConfig {scSlotLength, scSlotZeroTime} =
    "Slot 0 starts at"
      <+> pretty scSlotZeroTime
      <+> "and one slot has length of"
      <+> pretty scSlotLength
      <+> "ms"

slotConfigToEpochInfo :: SlotConfig -> EpochInfo (Either Text)
slotConfigToEpochInfo sc = do
  fixedEpochInfo emulatorEpochSize (slotLengthFromMillisec $ scSlotLength sc)

-- Utils

utcTimeToPOSIXTime :: Time.UTCTime -> POSIXTime
utcTimeToPOSIXTime = nominalDiffTimeToPOSIXTime . Time.utcTimeToPOSIXSeconds

posixTimeToUTCTime :: POSIXTime -> Time.UTCTime
posixTimeToUTCTime = Time.posixSecondsToUTCTime . posixTimeToNominalDiffTime

nominalDiffTimeToPOSIXTime :: Time.NominalDiffTime -> POSIXTime
nominalDiffTimeToPOSIXTime =
  POSIXTime
    . truncate
    . (* 1000) -- Convert to ms
    . Time.nominalDiffTimeToSeconds

posixTimeToNominalDiffTime :: POSIXTime -> Time.NominalDiffTime
posixTimeToNominalDiffTime =
  Time.secondsToNominalDiffTime
    . (/ 1000)
    . fromInteger
    . getPOSIXTime

newtype Slot = Slot {unSlot :: Integer}
  deriving stock (Eq, Ord, Show, Generic)
  -- deriving newtype
  --   ( AdditiveSemigroup
  --   , AdditiveMonoid
  --   , AdditiveGroup
  --   )
  deriving newtype (Num, Enum, Real, Integral)

instance Pretty Slot where
  pretty (Slot i) = "Slot" <+> pretty i

toSlot :: L.SlotNo -> Slot
toSlot = Slot . fromIntegral . L.unSlotNo

fromSlot :: Slot -> L.SlotNo
fromSlot = fromIntegral . unSlot

-- | Get the starting 'POSIXTime' of a 'Slot' given a 'SlotConfig'.
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime SlotConfig {scSlotLength, scSlotZeroTime} (Slot n) =
  let msAfterBegin = n * scSlotLength
   in POSIXTime $ getPOSIXTime scSlotZeroTime + msAfterBegin

-- | Convert a 'POSIXTime' to 'Slot' given a 'SlotConfig'.
posixTimeToEnclosingSlot :: SlotConfig -> POSIXTime -> Slot
posixTimeToEnclosingSlot SlotConfig {scSlotLength, scSlotZeroTime} (POSIXTime t) =
  let timePassed = t - getPOSIXTime scSlotZeroTime
      slotsPassed = divide timePassed scSlotLength
   in Slot slotsPassed

{- | Get the current slot number. If scSlotZeroTime may produce negative results,
which is nonsense.
-}
currentSlot :: SlotConfig -> IO Slot
currentSlot sc = timeToSlot <$> Time.getPOSIXTime
 where
  timeToSlot =
    posixTimeToEnclosingSlot sc
      . nominalDiffTimeToPOSIXTime

-- | A sensible default 'EpochSize' value for the emulator
emulatorEpochSize :: L.EpochSize
emulatorEpochSize = L.EpochSize 432000

{- | 'beginningOfTime' corresponds to the Shelley launch date
(2020-07-29T21:44:51Z) which is 1596059091000 in POSIX time
(number of milliseconds since 1970-01-01T00:00:00Z).
-}
beginningOfTime :: Integer
beginningOfTime = 1596059091000
