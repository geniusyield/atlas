module Main
    ( main
    ) where

import           Test.Tasty                               (defaultMain, testGroup)

import           GeniusYield.Test.Unified.BetRef.PlaceBet
import           GeniusYield.Test.Unified.BetRef.TakePot

main :: IO ()
main = defaultMain $ testGroup "BetRef" [placeBetTests, takeBetPotTests]