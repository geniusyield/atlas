{- |
Module      : GeniusYield.CardanoApi.EraHistory
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.CardanoApi.EraHistory (
  extractEraSummaries,
  showEraSummaries,
  getEraEndSlot,
) where

import Unsafe.Coerce qualified as UNSAFE

import Cardano.Api qualified as Api
import Data.SOP.NonEmpty (nonEmptyLast)
import Ouroboros.Consensus.Cardano.Block qualified as Ouroboros
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros

{- | Extract the 'Ouroboros.Summary' from Cardano 'Api.EraHistory'.

This is safe as long as Ouroboros.Interpeter is defined as a newtype to Ouroboros.Summary.

TODO: Can we ensure at runtime, that Interpreter is still a newtype to Summary? #20 (https://github.com/geniusyield/atlas/issues/20)
-}
extractEraSummaries ::
  Api.EraHistory ->
  Ouroboros.Summary (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
extractEraSummaries (Api.EraHistory interpreter) = UNSAFE.unsafeCoerce interpreter

{- | Extract and show the era summaries with all its details, useful for manually constructing 'Api.EraHistory'.

See: "GeniusYield.Providers.Common.preprodEraHist"
-}
showEraSummaries :: Api.EraHistory -> String
showEraSummaries eraHist = show $ extractEraSummaries eraHist

-- | Get the slot after which the current era ends.
getEraEndSlot :: Api.EraHistory -> Maybe Api.SlotNo
getEraEndSlot (extractEraSummaries -> Ouroboros.Summary summaries) =
  case Ouroboros.eraEnd (nonEmptyLast summaries) of
    Ouroboros.EraUnbounded -> Nothing
    Ouroboros.EraEnd bound -> Just $! Ouroboros.boundSlot bound
