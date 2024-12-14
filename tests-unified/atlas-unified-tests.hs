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
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $
      testGroup
        "BetRef"
        [ testGroup
            "Emulator - CLB"
            [ placeBetTestsClb
            , takeBetPotTestsClb
            ]
        , testGroup
            "Privnet"
            [ placeBetTests setup
            , takeBetPotTests setup
            ]
        ]
