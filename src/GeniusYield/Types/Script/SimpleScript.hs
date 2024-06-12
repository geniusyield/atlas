{-|
Module      : GeniusYield.Types.Script.SimpleScript
Description : Simple scripts API
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Script.SimpleScript (
  -- * Docspec setup
  -- $setup

  -- * Simple script
  GYSimpleScript (..),
  simpleScriptToApi,
  simpleScriptFromApi,
  readSimpleScript,
  getTotalKeysInSimpleScript,
  hashSimpleScript,
  hashSimpleScript',
) where

import qualified Cardano.Api                         as Api
import           Data.Foldable                       (foldMap')
import qualified Data.Set                            as Set
import           GeniusYield.Imports
import           GeniusYield.ReadJSON                (readJSON)
import           GeniusYield.Types.PaymentKeyHash    (GYPaymentKeyHash,
                                                      paymentKeyHashFromApi,
                                                      paymentKeyHashToApi)
import           GeniusYield.Types.Script.ScriptHash (GYScriptHash,
                                                      scriptHashFromApi)
import           GeniusYield.Types.Slot              (GYSlot, slotFromApi,
                                                      slotToApi)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>>
-- >>> import GeniusYield.Types
-- >>> import Data.Set qualified as Set

-- | A simple (aka native / timelock) script that can be used in a transaction.
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

-- | Read a 'GYSimpleScript' represented as JSON from a file.
readSimpleScript :: FilePath -> IO GYSimpleScript
readSimpleScript = readJSON

-- | Count the total number of unique `GYPaymentKeyHash` mentioned in a 'GYSimpleScript'.
--
-- This is useful for estimating the number of signatures required for a transaction.
--
-- >>> reqSigA = RequireSignature "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38a"
-- >>> reqSigB = RequireSignature "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38b"
-- >>> reqSigC = RequireSignature "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38c"
-- >>> reqSigD = RequireSignature "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
-- >>> reqSigE = RequireSignature "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38e"
-- >>> Set.size $ getTotalKeysInSimpleScript $ RequireMOf 2 [RequireAllOf [reqSigA, reqSigB, reqSigC], RequireAnyOf [reqSigA, reqSigD], reqSigE]
-- 5
getTotalKeysInSimpleScript :: GYSimpleScript -> Set GYPaymentKeyHash
getTotalKeysInSimpleScript = \case
  RequireSignature pkh -> Set.singleton pkh
  RequireTimeBefore _ -> Set.empty
  RequireTimeAfter _ -> Set.empty
  RequireAllOf ss -> f ss
  RequireAnyOf ss -> f ss
  RequireMOf _ ss -> f ss
 where
  f = foldMap' getTotalKeysInSimpleScript

hashSimpleScript :: GYSimpleScript -> GYScriptHash
hashSimpleScript = scriptHashFromApi . hashSimpleScript'

hashSimpleScript' :: GYSimpleScript -> Api.ScriptHash
hashSimpleScript' = Api.hashScript . Api.SimpleScript . simpleScriptToApi
