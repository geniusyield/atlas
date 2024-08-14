{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : GeniusYield.Types.Era
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Era (
    GYEra (..),
    ConwayEra,
) where

import qualified Cardano.Api.Shelley as Api.S
import qualified Data.Aeson          as Aeson
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

type ConwayEra = Api.S.ShelleyLedgerEra Api.S.ConwayEra

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson

-- | Eras at which cardano-node provider may operate.
--
-- We will drop the older eras when the transition to them is complete.
--
-- >>> Aeson.encode GYConway
-- "\"Conway\""
--
-- >>> Aeson.decode @GYEra "\"Babbage\""
-- Just GYBabbage
--
data GYEra = GYBabbage | GYConway
  deriving (Show, Read, Eq, Ord, Generic)

instance Aeson.ToJSON GYEra where
    toJSON GYConway  = Aeson.toJSON ("Conway" :: Text)
    toJSON GYBabbage = Aeson.toJSON ("Babbage" :: Text)

    toEncoding GYConway  = Aeson.toEncoding ("Conway" :: Text)
    toEncoding GYBabbage = Aeson.toEncoding ("Babbage" :: Text)

instance Aeson.FromJSON GYEra where
    parseJSON "Conway"  = pure GYConway
    parseJSON "Babbage" = pure GYBabbage
    parseJSON _         = fail "Expected 'Conway' or 'Babbage'"
