{-|
Module      : GeniusYield.Test.Privnet.Examples
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Examples (tests) where

import           Test.Tasty                               (TestTree, testGroup)

import qualified GeniusYield.Test.Privnet.Examples.Gift
import qualified GeniusYield.Test.Privnet.Examples.Oracle
import qualified GeniusYield.Test.Privnet.Examples.Treat
import           GeniusYield.Test.Privnet.Setup

tests :: IO Setup -> TestTree
tests setup = testGroup "examples"
    [ GeniusYield.Test.Privnet.Examples.Gift.tests setup
    , GeniusYield.Test.Privnet.Examples.Treat.tests setup
    , GeniusYield.Test.Privnet.Examples.Oracle.tests setup
    ]
