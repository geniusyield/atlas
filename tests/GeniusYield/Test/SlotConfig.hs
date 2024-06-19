module GeniusYield.Test.SlotConfig
    ( slotConversionTests
    ) where

import           Data.Time.Clock                      (UTCTime)
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time       ()
import           Test.Tasty                           (TestTree, testGroup)
import           Test.Tasty.QuickCheck

import qualified Cardano.Api                          as Api
import qualified Ouroboros.Consensus.BlockchainTime   as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros

import           GeniusYield.CardanoApi.EraHistory    (extractEraSummaries)
import           GeniusYield.Imports
import           GeniusYield.Providers.Common         (preprodEraHist, previewEraHist)
import           GeniusYield.Types

slotToTime :: forall xs. Ouroboros.SystemStart -> Ouroboros.Interpreter xs -> Api.SlotNo -> Either String UTCTime
slotToTime systemStart eraHistory x = bimap show (Ouroboros.fromRelativeTime systemStart) res
  where
    res = Ouroboros.interpretQuery eraHistory $ fst <$> Ouroboros.slotToWallclock x

timeToSlot :: forall xs. Ouroboros.SystemStart -> Ouroboros.Interpreter xs -> UTCTime -> Either String Api.SlotNo
timeToSlot systemStart eraHistory utc = first show res
  where
    res = Ouroboros.interpretQuery eraHistory $ (\(slot,_,_) -> slot)
          <$> Ouroboros.wallclockToSlot (Ouroboros.toRelativeTime systemStart utc)

checkTimeToSlot :: Api.EraHistory -> Property
checkTimeToSlot eraHistory
  = forAll (Ouroboros.SystemStart <$> arbitrary) $ \systemStart ->
    forAll (arbitraryTimeInRange systemStart eraEnd) $ \utc -> either error id $ do
      let Api.EraHistory interpreter = eraHistory
      expected <- timeToSlot systemStart interpreter utc

      slotCfg <- makeSlotConfig systemStart eraHistory

      let actualRes = enclosingSlotFromTimePure slotCfg $ timeFromPOSIX (utcTimeToPOSIXSeconds utc)

      pure $ Just expected == (slotToApi <$> actualRes)
  where
    summaries = extractEraSummaries eraHistory
    (_, eraEnd) = Ouroboros.summaryBounds summaries

checkSlotToTime :: Api.EraHistory -> Property
checkSlotToTime eraHistory
  = forAll (Ouroboros.SystemStart <$> arbitrary) $ \systemStart ->
    -- Also see: [Slot Config Design]
    forAll (arbitrarySlotBefore eraEnd) $ \slot -> either error id $ do
        let gslot                        = slotFromApi slot
            Api.EraHistory interpreter = eraHistory
        expectedRes <- slotToTime systemStart interpreter slot

        slotCfg <- makeSlotConfig systemStart eraHistory
        let actualRes = posixSecondsToUTCTime (timeToPOSIX $ slotToBeginTimePure slotCfg gslot)
        pure $ expectedRes === actualRes
  where
    summaries = extractEraSummaries eraHistory
    (_, eraEnd) = Ouroboros.summaryBounds summaries

slotConversionTests :: TestTree
slotConversionTests = testGroup "SlotToFromTime"
  [ testGroup "preprod"
      [ testProperty "SlotToTime"
          $ checkSlotToTime (Api.EraHistory preprodEraHist)
      , testProperty "TimeToSlot"
          $ checkTimeToSlot (Api.EraHistory preprodEraHist)
      ]
  , testGroup "preview"
      [ testProperty "SlotToTime"
          $ checkSlotToTime (Api.EraHistory previewEraHist)
      , testProperty "TimeToSlot"
          $ checkTimeToSlot (Api.EraHistory previewEraHist)
      ]
  ]

-- | Greater than or equal to system start, less than or equal to final era bound.
arbitraryTimeInRange :: Ouroboros.SystemStart -> Ouroboros.EraEnd -> Gen UTCTime
arbitraryTimeInRange sysStart eraEnd = arbitrary `suchThat` (\x -> x >= absStart && ltEnd x)
  where
    absStart = Ouroboros.getSystemStart sysStart
    ltEnd x = case eraEnd of
      Ouroboros.EraEnd bo    -> x < Ouroboros.fromRelativeTime sysStart (Ouroboros.boundTime bo)
      Ouroboros.EraUnbounded -> True

-- | Generate an arbitrary slot before given era end.
arbitrarySlotBefore :: Ouroboros.EraEnd -> Gen Api.SlotNo
arbitrarySlotBefore eraEnd = (Api.SlotNo <$> arbitrary) `suchThat` \slotNo -> case eraEnd of
    Ouroboros.EraEnd bo    -> slotNo < Ouroboros.boundSlot bo
    Ouroboros.EraUnbounded -> True
