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

import           System.Exit         (exitFailure)
import           System.FilePath     ((</>))

import qualified System.Directory    as Dir

import           GeniusYield.Imports

data Paths = Paths
    { pathNodeSocket  :: !FilePath
    , pathGeniusYield :: !FilePath
    , pathUserF       :: !UserPaths
    , pathUser2       :: !UserPaths
    , pathUser3       :: !UserPaths
    , pathUser4       :: !UserPaths
    , pathUser5       :: !UserPaths
    , pathUser6       :: !UserPaths
    , pathUser7       :: !UserPaths
    , pathUser8       :: !UserPaths
    , pathUser9       :: !UserPaths
    }
  deriving Show

data UserPaths = UserPaths
    { pathUserAddr :: !FilePath
    , pathUserSKey :: !FilePath
    , pathUserColl :: !FilePath
    }
  deriving Show

initPaths :: FilePath -> IO Paths
initPaths privnetPath = do
    checkPaths privnetPath paths
    return paths
  where
    paths = mkPaths privnetPath

mkPaths :: FilePath -> Paths
mkPaths privnetPath = Paths
    { pathNodeSocket  = privnetPath </> "node-spo1" </> "node.sock"
    , pathGeniusYield = pathGY
    -- Funder
    , pathUserF  = UserPaths
        { pathUserAddr = pathGY </> "userF.addr"
        , pathUserSKey = pathGY </> "userF.skey"
        , pathUserColl = pathGY </> "userF.collateral"
        }
    -- TODO: Seems unclean to manually put entries for all 9 users, could use vector/map etc.
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
    , pathUser4       = UserPaths
        { pathUserAddr = pathGY </> "user4.addr"
        , pathUserSKey = pathGY </> "user4.skey"
        , pathUserColl = pathGY </> "user4.collateral"
        }
    , pathUser5       = UserPaths
        { pathUserAddr = pathGY </> "user5.addr"
        , pathUserSKey = pathGY </> "user5.skey"
        , pathUserColl = pathGY </> "user5.collateral"
        }
    , pathUser6       = UserPaths
        { pathUserAddr = pathGY </> "user6.addr"
        , pathUserSKey = pathGY </> "user6.skey"
        , pathUserColl = pathGY </> "user6.collateral"
        }
    , pathUser7       = UserPaths
        { pathUserAddr = pathGY </> "user7.addr"
        , pathUserSKey = pathGY </> "user7.skey"
        , pathUserColl = pathGY </> "user7.collateral"
        }
    , pathUser8       = UserPaths
        { pathUserAddr = pathGY </> "user8.addr"
        , pathUserSKey = pathGY </> "user8.skey"
        , pathUserColl = pathGY </> "user8.collateral"
        }
    , pathUser9       = UserPaths
        { pathUserAddr = pathGY </> "user9.addr"
        , pathUserSKey = pathGY </> "user9.skey"
        , pathUserColl = pathGY </> "user9.collateral"
        }
    }
  where
    pathGY = privnetPath </> "geniusyield"

checkPaths :: FilePath -> Paths -> IO ()
checkPaths privnetPath Paths {..} = do
    checkFile pathNodeSocket
    let originalUserAddrFP = privnetPath </> "addresses" </> "user1.addr"
        originalUserSKeyFP = privnetPath </> "addresses" </> "user1.skey"
    checkFile originalUserAddrFP
    checkFile originalUserSKeyFP
    Dir.createDirectoryIfMissing False pathGeniusYield
    checkDir pathGeniusYield
    -- if already present due to previous ran of privnet test, replacing is fine.
    Dir.copyFile originalUserAddrFP (pathUserAddr pathUserF)
    Dir.copyFile originalUserSKeyFP (pathUserSKey pathUserF)
    checkFile $ pathUserAddr pathUserF
    checkFile $ pathUserSKey pathUserF
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
