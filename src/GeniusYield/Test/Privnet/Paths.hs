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

import           System.Exit             (exitFailure)
import           System.FilePath         ((</>))

import qualified System.Directory        as Dir

import qualified Data.Vector.Fixed       as V
import qualified Data.Vector.Fixed.Boxed as VB

import           GeniusYield.Imports

data Paths = Paths
    { pathNodeSocket  :: !FilePath
    , pathGeniusYield :: !FilePath
    , pathUserF       :: !UserPaths
    , pathUsers       :: !(VB.Vec 8 UserPaths)
    }
  deriving Show

data UserPaths = UserPaths
    { pathUserAddr :: !FilePath
    , pathUserSKey :: !FilePath
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
        }
    , pathUsers =
        fmap
          (\i -> UserPaths {
            pathUserAddr = pathGY </> printf "user%s.addr" (show i)
          , pathUserSKey = pathGY </> printf "user%s.skey" (show i)
          })
          (V.fromList [2 :: Integer .. 9])
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
