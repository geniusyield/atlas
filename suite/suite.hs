module Main
    ( main
    ) where

import           Test.Tasty              (defaultMain, testGroup)

import           BetRef.Tests.PlaceBet   (placeBetTests)
import           BetRef.Tests.TakePot (takeBetPotTests)

main :: IO ()
main = defaultMain $ testGroup "BetRef" [placeBetTests, takeBetPotTests]