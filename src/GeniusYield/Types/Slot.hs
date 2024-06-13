{-|
Module      : GeniusYield.Types.Slot
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Slot (
    GYSlot,
    slotToApi,
    slotFromApi,
    advanceSlot,
    unsafeAdvanceSlot,
    slotToInteger,
    slotFromInteger,
    slotFromWord64,
    unsafeSlotFromInteger
) where

import           Data.Word           (Word64)
import           GeniusYield.Imports

import qualified Cardano.Api         as Api
import qualified Data.Swagger        as Swagger
import qualified Text.Printf         as Printf
import           Web.HttpApiData     (FromHttpApiData, ToHttpApiData)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson as Aeson

-- >>> Aeson.fromJSON @GYSlot $ Aeson.Number 420000000000000000000000000000
-- Error "parsing Word64 failed, value is either floating or will cause over or underflow 4.2e29"
newtype GYSlot = GYSlot Word64
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Swagger.ToParamSchema, Swagger.ToSchema, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

instance Printf.PrintfArg GYSlot where
    formatArg (GYSlot n) = Printf.formatArg (show n)

slotToApi :: GYSlot -> Api.SlotNo
slotToApi = coerce

slotFromApi :: Api.SlotNo -> GYSlot
slotFromApi = coerce

slotToInteger :: GYSlot -> Integer
slotToInteger = coerce (toInteger @Word64)

slotFromInteger :: Integer -> Maybe GYSlot
slotFromInteger s
    | s > toInteger (maxBound :: Word64) = Nothing
    | s < toInteger (minBound :: Word64) = Nothing
    | otherwise                          = Just . GYSlot $ fromInteger s

slotFromWord64 :: Word64 -> GYSlot
slotFromWord64 = GYSlot

-- | Advance 'GYSlot' forward. If slot value overflows, return 'Nothing'.
advanceSlot :: GYSlot -> Natural -> Maybe GYSlot
advanceSlot (GYSlot s) t
    | st > fromIntegral (maxBound :: Word64) = Nothing
    | otherwise                              = Just (GYSlot (fromIntegral st))
  where
    st :: Natural
    st = fromIntegral s + t

-- | Unsafe advance 'GYSlot'. Doesn't check for the overflow.
unsafeAdvanceSlot :: GYSlot -> Natural -> GYSlot
unsafeAdvanceSlot (GYSlot s) t = GYSlot (s + fromIntegral t)

-- | Convert from regular integer, which might under or overflow.
unsafeSlotFromInteger :: Integer -> GYSlot
unsafeSlotFromInteger s
    | s > toInteger (maxBound :: Word64) = error "slot overflow"
    | s < toInteger (minBound :: Word64) = error "slot underflow"
    | otherwise                          = GYSlot (fromInteger s)
