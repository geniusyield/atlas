{-|
Module      : GeniusYield.Test.Privnet.Paths
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Paths (
    Paths (..),
    UserPaths (..),
    initPaths,
) where

import           System.FilePath  ((</>))
import           System.Exit      (exitFailure)

import qualified System.Directory as Dir

import GeniusYield.Imports

data Paths = Paths
    { pathNodeSocket  :: FilePath
    , pathGeniusYield :: FilePath
    , pathUser1       :: UserPaths
    , pathUser2       :: UserPaths
    , pathUser3       :: UserPaths
    }
  deriving Show

data UserPaths = UserPaths
    { pathUserAddr :: !FilePath
    , pathUserSKey :: !FilePath
    , pathUserColl :: !FilePath
    }
  deriving Show

initPaths :: FilePath -> IO Paths
initPaths fp = do
    checkPaths paths
    return paths
  where
    paths = mkPaths fp

mkPaths :: FilePath -> Paths
mkPaths privnetPath = Paths
    { pathNodeSocket  = privnetPath </> "node-spo1" </> "node.sock"
    , pathGeniusYield = pathGY
    , pathUser1       = UserPaths
        { pathUserAddr = privnetPath </> "addresses" </> "user1.addr"
        , pathUserSKey = privnetPath </> "addresses" </> "user1.skey"
        , pathUserColl = pathGY </> "user1.collateral"
        }
    , pathUser2       = UserPaths
        { pathUserAddr = pathGY </> "user2.addr"
        , pathUserSKey = pathGY </> "user2.skey"
        , pathUserColl = pathGY </> "user2.collateral"
        }
    , pathUser3       = UserPaths
        { pathUserAddr = pathGY </> "user3.addr"
        , pathUserSKey = pathGY </> "user3.skey"
        , pathUserColl = pathGY </> "user3.collateral"
        }
    }
  where
    pathGY = privnetPath </> "geniusyield"

checkPaths :: Paths -> IO ()
checkPaths Paths {..} = do
    checkFile pathNodeSocket
    checkFile $ pathUserAddr pathUser1
    checkFile $ pathUserSKey pathUser1
    Dir.createDirectoryIfMissing False pathGeniusYield
    checkDir pathGeniusYield
  where
    checkFile p = do
        -- printf "INFO: checkFile %s\n" p
        exists <- Dir.doesFileExist p
        unless exists $ do
            printf "ERROR: checkFile: doesn't exist: %s\n" p
            exitFailure

    checkDir p = do
        -- printf "INFO: checkDir %s\n" p
        exists <- Dir.doesDirectoryExist p
        unless exists $ do
            printf "ERROR: checkDir: doesn't exist: %s\n" p
            exitFailure
