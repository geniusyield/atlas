{-|
Module      : GeniusYield.Types.Redeemer
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Redeemer (
    GYRedeemer,
    redeemerToApi,
    redeemerToPlutus,
    redeemerFromPlutus',
    redeemerFromPlutusData,
    unitRedeemer,
    nothingRedeemer,
) where

import qualified Cardano.Api          as Api
import qualified PlutusLedgerApi.V1 as Plutus

import           GeniusYield.Types.Datum      (dataToScriptData)

newtype GYRedeemer = GYRedeemer Plutus.BuiltinData
  deriving (Eq)

instance Show GYRedeemer where
    showsPrec d (GYRedeemer x) = showParen (d > 10)
        -- Show BuiltinData doesn't respect precedence.
        $ showString "redeemerFromPlutus' (BuiltinData ("
        . shows x
        . showString "))"

redeemerToPlutus :: GYRedeemer -> Plutus.Redeemer
redeemerToPlutus (GYRedeemer x) = Plutus.Redeemer x

redeemerFromPlutus' :: Plutus.BuiltinData -> GYRedeemer
redeemerFromPlutus' = GYRedeemer

redeemerFromPlutusData :: Plutus.ToData a => a -> GYRedeemer
redeemerFromPlutusData = GYRedeemer . Plutus.toBuiltinData

redeemerToApi :: GYRedeemer -> Api.HashableScriptData
redeemerToApi (GYRedeemer (Plutus.BuiltinData x)) = dataToScriptData x

-- | Unit redeemer
--
-- @'redeemerFromPlutusData' ()@.
--
-- Often used as an arbitrary redeemer.
--
unitRedeemer :: GYRedeemer
unitRedeemer = redeemerFromPlutusData ()

-- | A @'redeemerFromPlutusData' (Nothing \@a)@ for any @a@.
--
-- >>> nothingRedeemer
-- redeemerFromPlutus' (BuiltinData (Constr 1 []))
--
-- >>> redeemerFromPlutusData (Nothing @Integer)
-- redeemerFromPlutus' (BuiltinData (Constr 1 []))
--
nothingRedeemer :: GYRedeemer
nothingRedeemer = redeemerFromPlutusData (Nothing @())
