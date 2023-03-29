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
) where

import qualified Data.Aeson   as Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- $setup
--
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Aeson                 as Aeson

-- | Eras at which cardano-node provider may operate.
--
-- We will drop the older eras when the transition to them is complete.
-- (atm, August 2022, we still need Alonzo a bit)
--
-- >>> Aeson.encode GYAlonzo
-- "\"Alonzo\""
--
-- >>> Aeson.decode @GYEra "\"Babbage\""
-- Just GYBabbage
--
data GYEra = GYAlonzo | GYBabbage
  deriving (Show, Read, Eq, Ord, Generic)

instance Aeson.ToJSON GYEra where
    toJSON GYAlonzo  = Aeson.toJSON ("Alonzo" :: Text)
    toJSON GYBabbage = Aeson.toJSON ("Babbage" :: Text)

    toEncoding GYAlonzo  = Aeson.toEncoding ("Alonzo" :: Text)
    toEncoding GYBabbage = Aeson.toEncoding ("Babbage" :: Text)

instance Aeson.FromJSON GYEra where
    parseJSON "Alonzo"  = pure GYAlonzo
    parseJSON "Babbage" = pure GYBabbage
    parseJSON _         = fail "Expected 'Alonzo' or 'Babbage'"
