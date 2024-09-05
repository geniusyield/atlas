{- |
Module      : GeniusYield.ReadJSON
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.ReadJSON (
  readJSON,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import GeniusYield.Imports

readJSON :: FromJSON a => FilePath -> IO a
readJSON fp = do
  bs <- LBS.readFile fp
  case Aeson.eitherDecode' bs of
    Left err -> throwIO $ userError err
    Right cfg -> pure cfg
