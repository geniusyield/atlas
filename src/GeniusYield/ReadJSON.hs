module GeniusYield.ReadJSON (
  readJSON
) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import           GeniusYield.Imports

readJSON :: FromJSON a => FilePath -> IO a
readJSON fp = do
  bs <- LBS.readFile fp
  case Aeson.eitherDecode' bs of
    Left err  -> throwIO $ userError err
    Right cfg -> pure cfg
