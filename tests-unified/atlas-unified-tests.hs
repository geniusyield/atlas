module Main (
  main,
) where

import Test.Tasty (
  defaultMain,
  testGroup, withResource,
 )

import GeniusYield.Test.Privnet.Setup

import GeniusYield.Test.Unified.BetRef.PlaceBet
import GeniusYield.Test.Unified.BetRef.TakePot
import Control.Concurrent (killThread)


-- main :: IO ()
-- main = do
--   withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
--     defaultMain $
--       testGroup "Atlas Unified Tests"
--         [
--           -- testGroup "Emulator"
--           --   [ placeBetTestsClb
--           --   , takeBetPotTestsClb
--           --   ]
--           -- ,
--         testGroup "Privnet"
--             [ placeBetTests $ pure (setup, undefined)
--               -- , takeBetPotTests setup
--             ]
--         ]


main :: IO ()
main = do
    defaultMain $
      testGroup "Atlas Unified Tests"
        [ testGroup "Emulator"
            [ placeBetTestsClb
            , takeBetPotTestsClb
            ]
        , withResource
            (withPrivnet' cardanoDefaultTestnetOptionsConway)
            (\(_, threadId) -> putStrLn ("killing thread: " <> show threadId)
                                   *> killThread threadId)
              $ \getSetup -> testGroup "Privnet"
                  [ placeBetTests getSetup
                  -- , takeBetPotTests setup
                  ]
        ]

