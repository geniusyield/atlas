module Main (
  main,
) where

import Data.Maybe (isJust)
import System.Directory (findExecutable)
import Test.Tasty (
  defaultMain,
  localOption,
  testGroup,
 )
import Test.Tasty.Runners (NumThreads (..))

import GeniusYield.Test.Privnet.Setup

import GeniusYield.Test.Unified.BetRef.PlaceBet
import GeniusYield.Test.Unified.BetRef.TakePot

main :: IO ()
main = do
  let clbGroup =
        testGroup
          "Emulator - CLB"
          [ placeBetTestsClb
          , takeBetPotTestsClb
          ]
  hasNode <- ((&&) . isJust <$> findExecutable "cardano-node") <*> (isJust <$> findExecutable "cardano-cli")
  if hasNode
    then withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
      defaultMain $
        localOption (NumThreads 1) $
          testGroup
            "BetRef"
            [ clbGroup
            , testGroup
                "Privnet"
                [ placeBetTests setup
                , takeBetPotTests setup
                ]
            ]
    else
      defaultMain $
        testGroup
          "BetRef"
          [ clbGroup
          ]
