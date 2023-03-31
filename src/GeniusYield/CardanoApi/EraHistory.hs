{-|
Module      : GeniusYield.CardanoApi.EraHistory
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.CardanoApi.EraHistory (extractEraSummaries, showEraSummaries, getEraEndSlot) where

import qualified Unsafe.Coerce as UNSAFE

import qualified Cardano.Api                          as Api
import qualified Ouroboros.Consensus.Cardano.Block    as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History as Ouroboros
import qualified Ouroboros.Consensus.Util.Counting as Ouroboros

{- | Extract the 'Ouroboros.Summary' from Cardano 'Api.EraHistory'.

This is safe as long as Ouroboros.Interpeter is defined as a newtype to Ouroboros.Summary.

TODO: Can we ensure at runtime, that Interpreter is still a newtype to Summary? #20 (https://github.com/geniusyield/atlas/issues/20)
-}
extractEraSummaries :: Api.EraHistory Api.CardanoMode -> Ouroboros.Summary (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
extractEraSummaries (Api.EraHistory _ interpreter) = UNSAFE.unsafeCoerce interpreter

{- | Extract and show the era summaries with all its details, useful for manually constructing 'Api.EraHistory'.

See: "GeniusYield.Providers.Common.preprodEraHist"
-}
showEraSummaries :: Api.EraHistory Api.CardanoMode -> String
showEraSummaries eraHist = show $ extractEraSummaries eraHist

-- | Get the slot after which the current era ends.
getEraEndSlot :: Api.EraHistory Api.CardanoMode -> Maybe Api.SlotNo
getEraEndSlot (extractEraSummaries -> Ouroboros.Summary summaries) =
    case Ouroboros.eraEnd (Ouroboros.nonEmptyLast summaries) of
        Ouroboros.EraUnbounded -> Nothing
        Ouroboros.EraEnd bound -> Just $! Ouroboros.boundSlot bound
