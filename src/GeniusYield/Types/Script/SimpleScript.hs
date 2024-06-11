{-|
Module      : GeniusYield.Types.Script.SimpleScript
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
-- TODO: Add the module description.
module GeniusYield.Types.Script.SimpleScript (
  GYSimpleScript (..),
  simpleScriptToApi,
  simpleScriptFromApi,
  hashSimpleScript,
  hashSimpleScript',
) where

import qualified Cardano.Api                         as Api
import           GeniusYield.Imports
import           GeniusYield.Types.PaymentKeyHash    (GYPaymentKeyHash,
                                                      paymentKeyHashFromApi,
                                                      paymentKeyHashToApi)
import           GeniusYield.Types.Script.ScriptHash (GYScriptHash,
                                                      scriptHashFromApi)
import           GeniusYield.Types.Slot              (GYSlot, slotFromApi,
                                                      slotToApi)

-- TODO: Haddock.
data GYSimpleScript
  = RequireSignature !GYPaymentKeyHash
  | RequireTimeBefore !GYSlot
  | RequireTimeAfter !GYSlot
  | RequireAllOf ![GYSimpleScript]
  | RequireAnyOf ![GYSimpleScript]
  | RequireMOf !Int ![GYSimpleScript]
  deriving (Eq, Show)

simpleScriptToApi :: GYSimpleScript -> Api.SimpleScript
simpleScriptToApi s = case s of
  RequireSignature pkh   -> Api.RequireSignature $ paymentKeyHashToApi pkh
  RequireTimeBefore slot -> Api.RequireTimeBefore $ slotToApi slot
  RequireTimeAfter slot  -> Api.RequireTimeAfter $ slotToApi slot
  RequireAllOf ss        -> Api.RequireAllOf $ map simpleScriptToApi ss
  RequireAnyOf ss        -> Api.RequireAnyOf $ map simpleScriptToApi ss
  RequireMOf m ss        -> Api.RequireMOf m $ map simpleScriptToApi ss

simpleScriptFromApi :: Api.SimpleScript -> GYSimpleScript
simpleScriptFromApi s = case s of
  Api.RequireSignature pkh   -> RequireSignature $ paymentKeyHashFromApi pkh
  Api.RequireTimeBefore slot -> RequireTimeBefore $ slotFromApi slot
  Api.RequireTimeAfter slot  -> RequireTimeAfter $ slotFromApi slot
  Api.RequireAllOf ss        -> RequireAllOf $ map simpleScriptFromApi ss
  Api.RequireAnyOf ss        -> RequireAnyOf $ map simpleScriptFromApi ss
  Api.RequireMOf m ss        -> RequireMOf m $ map simpleScriptFromApi ss

instance ToJSON GYSimpleScript where
  toJSON = toJSON . simpleScriptToApi

instance FromJSON GYSimpleScript where
  parseJSON = fmap simpleScriptFromApi . parseJSON

-- TODO: Functions to parse from JSON?

hashSimpleScript :: GYSimpleScript -> GYScriptHash
hashSimpleScript = scriptHashFromApi . hashSimpleScript'

hashSimpleScript' :: GYSimpleScript -> Api.ScriptHash
hashSimpleScript' = Api.hashScript . Api.SimpleScript . simpleScriptToApi
