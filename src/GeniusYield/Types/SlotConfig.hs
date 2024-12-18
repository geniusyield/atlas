{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : GeniusYield.Types.SlotConfig
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.SlotConfig (
  GYSlotConfig (gyscSystemStart, gyscEraSlotConfigs),
  GYEraSlotConfig,
  makeSlotConfig,
  simpleSlotConfig,
  slotToBeginTimePure,
  slotToEndTimePure,
  enclosingSlotFromTimePure,
  unsafeEnclosingSlotFromTimePure,
  slotToEpochPure,
  slotToEpochPure',
  epochToBeginSlotPure,
) where

import Control.Monad (unless, (<$!>))
import Control.Monad.Except (
  Except,
  MonadError (throwError),
  runExcept,
 )
import Data.Fixed (div')
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import Data.Word (Word64)

import Cardano.Api qualified as Api
import Cardano.Slotting.Slot qualified as CSlot
import Cardano.Slotting.Time qualified as CSlot
import Data.SOP.NonEmpty qualified as Ouroboros
import Ouroboros.Consensus.BlockchainTime qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros

import GeniusYield.CardanoApi.EraHistory
import GeniusYield.Types.Epoch (GYEpochNo (GYEpochNo), GYEpochSize (GYEpochSize), epochNoFromApi)
import GeniusYield.Types.Slot
import GeniusYield.Types.Time

--

{- $setup

>>> import qualified Data.Time.Clock.POSIX as Time
>>> import           GeniusYield.Types
-}

{- Note [Slot Config Design]

'GYSlotConfig' is effectively a stripped down version of 'Ouroboros.Interpreter' (combined with 'CSlot.SystemStart').
The slot <-> conversion operations also mimic (but consolidate) the behavior of 'Ouroboros.slotToWallClock' and
'Ouroboros.wallClockToSlot' query interpretations.

The rationale behind this is simply that 'Api.EraHistory' (which contains the interpreter) is much too overcomplicated
for this simple task. The design simplification here should allow easy construction of "simple" slot configs for testing
and similar.

== IMPORTANT ==

There is however, one notable deviation in this design: It assumes the final era is unbounded.

What does this imply in practice? To cut straight to the chase: 'EraHistory' interpretation assumes the
current era ends in the very next epoch. This means that any attempt to convert a slot/time beyond the current
epoch's end (see epoch size) would result in a 'PastHorizonException'. This is indeed a conservative decision,
as a hardfork _could_ happen at the next epoch!

Our design will happily give you a result even if you try to convert a slot/time beyond the current epoch. When can
this be a massive trap? Well, if the slot length changes in a future hardfork (highly unlikely?) - it may screw up
a smart contract.
-}

-- | Information about slot config for a particular ledger era.
data GYEraSlotConfig = GYEraSlotConfig
  { gyEraSlotStart :: !GYSlot
  -- ^ The slot with which the era started (inclusive).
  , gyEraSlotLength :: !CSlot.SlotLength
  -- ^ The slot length as set in the era.
  , gyEraSlotZeroTime :: !CSlot.RelativeTime
  -- ^ The time when the era started, relative to 'CSlot.SystemStart' in 'GYSlotConfig'.
  , gyEraStartEpoch :: !GYEpochNo
  -- ^ The epoch with which the era started (inclusive).
  , gyEraEpochSize :: !GYEpochSize
  }
  deriving stock (Eq, Show)

{- | Slot config for each era, alongside the absolute system start time.

== Invariants ==

- List must be ordered on era, with earliest era first, and current era last.
- Each era element must be continuous.

  i.e for [x, y], the slot start of y must be the end slot of x (see 'Api.EraHistory').
- The final era element must be the current era, and it is _assumed_ that its end is unbounded (realistic).
-}
data GYSlotConfig = GYSlotConfig
  { gyscSystemStart :: !CSlot.SystemStart
  , gyscEraSlotConfigs :: !(NonEmpty GYEraSlotConfig)
  }
  deriving stock (Eq, Show)

{- | Create a 'GYSlotConfig' from the system start and the cardano era history.

This is the recommended, robust, way to create slot config.
-}
makeSlotConfig :: CSlot.SystemStart -> Api.EraHistory -> Either String GYSlotConfig
makeSlotConfig sysStart eraHist = GYSlotConfig sysStart <$!> simplifiedEraSumms
 where
  simplifiedEraSumms :: Either String (NonEmpty GYEraSlotConfig)
  !simplifiedEraSumms = case extractEraSummaries eraHist of
    -- This pattern match ensures the summaries start with the very first era (Bound should be all 0).
    summ@(Ouroboros.Summary eraSumms@(Ouroboros.NonEmptyCons Ouroboros.EraSummary {eraStart = FirstEraBound} _)) ->
      -- Verify the rest of the invariants.
      runExcept (invariantSummary summ)
        -- Convert the summaries into a collection of 'GYEraSlotConfig'.
        $> (toEraSlotConf <$!> toNonEmpty eraSumms)
    _ ->
      Left $!
        "Initial era element within given EraHistory must be the very first ledger era"
          ++ " (Era Start bound should be 0)"
  toEraSlotConf :: Ouroboros.EraSummary -> GYEraSlotConfig
  toEraSlotConf
    Ouroboros.EraSummary
      { eraStart = Ouroboros.Bound {boundTime, boundSlot, boundEpoch}
      , eraParams = Ouroboros.EraParams {eraSlotLength, eraEpochSize = CSlot.EpochSize epochSize}
      } = GYEraSlotConfig {gyEraSlotStart = slotFromApi boundSlot, gyEraSlotLength = eraSlotLength, gyEraSlotZeroTime = boundTime, gyEraStartEpoch = epochNoFromApi boundEpoch, gyEraEpochSize = GYEpochSize epochSize}
  toNonEmpty :: Ouroboros.NonEmpty xs a -> NonEmpty a
  toNonEmpty (Ouroboros.NonEmptyOne x) = x :| []
  toNonEmpty (Ouroboros.NonEmptyCons x xs) = x :| toList xs

-- The era start bound for the very first era.
pattern FirstEraBound :: Ouroboros.Bound
pattern FirstEraBound <- Ouroboros.Bound (CSlot.RelativeTime 0) 0 (CSlot.EpochNo 0)

{- | Create a single era slot config (useful for emulator traces).

DO NOT USE for testnets/mainnet. Please use 'makeSlotConfig' instead.
-}
simpleSlotConfig :: Time.UTCTime -> Time.NominalDiffTime -> GYSlotConfig
simpleSlotConfig zero len =
  GYSlotConfig (CSlot.SystemStart zero) $
    GYEraSlotConfig
      { gyEraSlotStart = slotFromApi 0
      , gyEraSlotZeroTime = CSlot.RelativeTime 0
      , gyEraSlotLength = CSlot.mkSlotLength len
      , gyEraStartEpoch = GYEpochNo 0
      , gyEraEpochSize = GYEpochSize 432000
      }
      :| []

{- Finds the slot config for the given slot. Essentially, the chosen slot config must have its starting slot
less than, or equal to, the given slot. Furthermore, the chosen slot config's end slot, i.e next slot config's
starting slot (or unbounded if final era), should be greater than the given slot.
-}
findSlotConfViaSlot :: GYSlot -> NonEmpty GYEraSlotConfig -> GYEraSlotConfig
findSlotConfViaSlot _slot (x :| []) = x
findSlotConfViaSlot
  slot
  (thisSlotConf@GYEraSlotConfig {gyEraSlotStart = startSlot} :| nextSlotConf@GYEraSlotConfig {gyEraSlotStart = endSlot} : rest) =
    if slot >= startSlot && slot < endSlot then thisSlotConf else findSlotConfViaSlot slot $ nextSlotConf :| rest

{- Finds the slot config for the given relative time. Essentially, the chosen slot config must have its starting time
greater than, or equal to, the given relative time. Furthermore, the chosen slot config's end time, i.e next slot config's
starting time (or unbounded if final era), should be greater than the given relative time.
-}
findSlotConfViaRelTime :: Ouroboros.RelativeTime -> NonEmpty GYEraSlotConfig -> GYEraSlotConfig
findSlotConfViaRelTime _relTime (x :| []) = x
findSlotConfViaRelTime
  relTime
  ( thisSlotConf@GYEraSlotConfig {gyEraSlotZeroTime = startTime}
      :| nextSlotConf@GYEraSlotConfig {gyEraSlotZeroTime = endTime}
      : rest
    ) =
    if relTime >= startTime && relTime < endTime then thisSlotConf else findSlotConfViaRelTime relTime $ nextSlotConf :| rest

{- Finds the slot config for the given epoch. Essentially, the chosen slot config must have its starting epoch no
less than, or equal to, the given epoch no. Furthermore, the chosen slot config's end epoch, i.e next slot config's
starting epoch no (or unbounded if final era), should be greater than the given slot.
-}
findSlotConfViaEpoch :: GYEpochNo -> NonEmpty GYEraSlotConfig -> GYEraSlotConfig
findSlotConfViaEpoch _epochNo (x :| []) = x
findSlotConfViaEpoch
  epochNo
  (thisSlotConf@GYEraSlotConfig {gyEraStartEpoch = startEpoch} :| nextSlotConf@GYEraSlotConfig {gyEraStartEpoch = endEpoch} : rest) =
    if epochNo >= startEpoch && epochNo < endEpoch then thisSlotConf else findSlotConfViaEpoch epochNo $ nextSlotConf :| rest

{- | Get the starting 'GYTime' of a 'GYSlot' given a 'GYSlotConfig'.

>>> slotToBeginTimePure (simpleSlotConfig (Time.posixSecondsToUTCTime 10) 2) (unsafeSlotFromInteger 1)
GYTime 12s
-}
slotToBeginTimePure :: GYSlotConfig -> GYSlot -> GYTime
slotToBeginTimePure sc slot = timeFromPOSIX $ slotToBeginPOSIXTime' sc slot

slotToBeginPOSIXTime' :: GYSlotConfig -> GYSlot -> Time.POSIXTime
slotToBeginPOSIXTime' (GYSlotConfig sysStart slotConfs) slot =
  Time.utcTimeToPOSIXSeconds
  -- SystemStart + relativeResult
  $
    CSlot.fromRelativeTime sysStart relativeResult
 where
  -- slotZeroTime + (slot - startSlotNo) *  slotLength
  relativeResult =
    CSlot.getSlotLength gyEraSlotLength
      `CSlot.multNominalDiffTime` (slotToInteger slot - slotToInteger gyEraSlotStart)
      `CSlot.addRelativeTime` gyEraSlotZeroTime
  GYEraSlotConfig {gyEraSlotZeroTime, gyEraSlotStart, gyEraSlotLength} = findSlotConfViaSlot slot slotConfs

{- | Get the ending 'GYTime' of a 'GYSlot' (inclusive) given a 'GYSlotConfig'.

>>> slotToEndTimePure (simpleSlotConfig (Time.posixSecondsToUTCTime 10) 2) (unsafeSlotFromInteger 1)
GYTime 13.999s
-}
slotToEndTimePure :: GYSlotConfig -> GYSlot -> GYTime
slotToEndTimePure sc@(GYSlotConfig _ _) slot =
  timeFromPOSIX $ slotToBeginPOSIXTime' sc (unsafeAdvanceSlot slot 1) - oneMs
 where
  oneMs :: Time.NominalDiffTime
  oneMs = 0.001

{- | Get the 'GYSlot' of a 'GYTime' given a 'GYSlotConfig'.

Returns 'Nothing' if given time is before known system start.

>>> enclosingSlotFromTimePure (simpleSlotConfig (Time.posixSecondsToUTCTime 10) 2) (timeFromPOSIX 12)
Just (GYSlot 1)

>>> enclosingSlotFromTimePure (simpleSlotConfig (Time.posixSecondsToUTCTime 10) 2) (timeFromPOSIX 14)
Just (GYSlot 2)
-}
enclosingSlotFromTimePure :: GYSlotConfig -> GYTime -> Maybe GYSlot
enclosingSlotFromTimePure (GYSlotConfig sysStart slotConfs) (timeToPOSIX -> absTime)
  | absTimeUtc < CSlot.getSystemStart sysStart = Nothing
  | otherwise =
      -- startSlotNo + relativeResult
      Just . slotFromApi . Ouroboros.addSlots relativeResult $ slotToApi gyEraSlotStart
 where
  absTimeUtc = Time.posixSecondsToUTCTime absTime
  -- absTime - SystemStart
  relTime = CSlot.toRelativeTime sysStart absTimeUtc
  -- (relTime - slotZeroTime) / slotLength
  relativeResult = (relTime `CSlot.diffRelativeTime` gyEraSlotZeroTime) `div'` CSlot.getSlotLength gyEraSlotLength
  GYEraSlotConfig {gyEraSlotZeroTime, gyEraSlotStart, gyEraSlotLength} = findSlotConfViaRelTime relTime slotConfs

-- | Partial version of 'enclosingSlotFromTimePure'.
unsafeEnclosingSlotFromTimePure :: GYSlotConfig -> GYTime -> GYSlot
unsafeEnclosingSlotFromTimePure sc =
  fromMaybe (error "Given time is before system start")
    . enclosingSlotFromTimePure sc

-- | Get epoch number in which the given slot belongs to.
slotToEpochPure :: GYSlotConfig -> GYSlot -> GYEpochNo
slotToEpochPure sc slot = fst $ slotToEpochPure' sc slot

-- | Get epoch number and epoch size in which the given slot belongs to.
slotToEpochPure' :: GYSlotConfig -> GYSlot -> (GYEpochNo, GYEpochSize)
slotToEpochPure' (GYSlotConfig _ slotConfs) slot =
  ( GYEpochNo $
      startEpoch + ((slotToWord64 slot - slotToWord64 gyEraSlotStart) `div` epochSize)
  , GYEpochSize epochSize
  )
 where
  GYEraSlotConfig {gyEraSlotStart, gyEraEpochSize = GYEpochSize epochSize, gyEraStartEpoch = GYEpochNo startEpoch} = findSlotConfViaSlot slot slotConfs

-- | Get the first slot in the given epoch.
epochToBeginSlotPure :: GYSlotConfig -> GYEpochNo -> GYSlot
epochToBeginSlotPure (GYSlotConfig _ slotConfs) epochNo = gyEraSlotStart
 where
  GYEraSlotConfig {gyEraSlotStart} = findSlotConfViaEpoch epochNo slotConfs

----------------------------------
-- Handrolled Invariant Summary --
----------------------------------

{- |

This is literally 'Ouroboros.invariantSummary' with a singular character change, see below.

If possible, remove this horrid thing once this is resolved:
https://github.com/input-output-hk/ouroboros-network/issues/4100
-}
invariantSummary :: Ouroboros.Summary xs -> Except String ()
invariantSummary = \(Ouroboros.Summary summary) ->
  -- Pretend the start of the first era is the "end of the previous" one
  go (Ouroboros.eraStart (Ouroboros.nonEmptyHead summary)) (toList summary)
 where
  go ::
    Ouroboros.Bound ->
    -- \^ End of the previous era
    [Ouroboros.EraSummary] ->
    Except String ()
  go _ [] = return ()
  go prevEnd (curSummary : next) = do
    unless (curStart == prevEnd) $
      throwError $
        mconcat
          [ "Bounds don't line up: end of previous era "
          , show prevEnd
          , " /= start of current era "
          , show curStart
          ]

    case mCurEnd of
      Ouroboros.EraUnbounded ->
        unless (null next) $
          throwError "Unbounded non-final era"
      Ouroboros.EraEnd curEnd -> do
        -- Check the invariants mentioned at 'EraSummary'
        --
        -- o @epochsInEra@ corresponds to @e' - e@
        -- o @slotsInEra@ corresponds to @(e' - e) * epochSize)@
        -- o @timeInEra@ corresponds to @((e' - e) * epochSize * slotLen@
        --   which, if INV-1b holds, equals @(s' - s) * slotLen@
        let epochsInEra, slotsInEra :: Word64
            epochsInEra = Ouroboros.countEpochs (Ouroboros.boundEpoch curEnd) (Ouroboros.boundEpoch curStart)
            slotsInEra = epochsInEra * CSlot.unEpochSize (Ouroboros.eraEpochSize curParams)

            timeInEra :: NominalDiffTime
            timeInEra =
              fromIntegral slotsInEra
                * CSlot.getSlotLength (Ouroboros.eraSlotLength curParams)

        -- NOTE: The only change is here, using >= rather than >
        unless (Ouroboros.boundEpoch curEnd >= Ouroboros.boundEpoch curStart) $
          throwError "Empty era"

        unless (Ouroboros.boundSlot curEnd == Ouroboros.addSlots slotsInEra (Ouroboros.boundSlot curStart)) $
          throwError $
            mconcat
              [ "Invalid final boundSlot in "
              , show curSummary
              , " (INV-1b)"
              ]

        unless (Ouroboros.boundTime curEnd == Ouroboros.addRelTime timeInEra (Ouroboros.boundTime curStart)) $
          throwError $
            mconcat
              [ "Invalid final boundTime in "
              , show curSummary
              , " (INV-2b)"
              ]

        go curEnd next
   where
    curStart :: Ouroboros.Bound
    mCurEnd :: Ouroboros.EraEnd
    curParams :: Ouroboros.EraParams
    Ouroboros.EraSummary curStart mCurEnd curParams = curSummary
