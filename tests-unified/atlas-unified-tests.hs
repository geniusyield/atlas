module Main (
  main,
) where

import Test.Tasty (
  defaultMain,
  testGroup,
 )

import GeniusYield.Test.Privnet.Setup

import GeniusYield.Test.Unified.BetRef.PlaceBet
import GeniusYield.Test.Unified.BetRef.TakePot


main :: IO ()
main = do
  defaultMain $ testGroup "Emulator"
    [ placeBetTestsClb
    , takeBetPotTestsClb
    ]
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $ testGroup "Privnet"
      [ placeBetTests setup
      , takeBetPotTests setup
      ]