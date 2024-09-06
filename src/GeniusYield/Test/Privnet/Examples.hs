{- |
Module      : GeniusYield.Test.Privnet.Examples
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Privnet.Examples (tests) where

import Test.Tasty (TestTree, testGroup)

import GeniusYield.Test.Privnet.Examples.Gift qualified
import GeniusYield.Test.Privnet.Examples.Misc qualified
import GeniusYield.Test.Privnet.Examples.Oracle qualified
import GeniusYield.Test.Privnet.Examples.Treat qualified
import GeniusYield.Test.Privnet.Setup

tests :: Setup -> TestTree
tests setup =
  testGroup
    "examples"
    [ GeniusYield.Test.Privnet.Examples.Gift.tests setup
    , GeniusYield.Test.Privnet.Examples.Treat.tests setup
    , GeniusYield.Test.Privnet.Examples.Oracle.tests setup
    , GeniusYield.Test.Privnet.Examples.Misc.tests setup
    ]
