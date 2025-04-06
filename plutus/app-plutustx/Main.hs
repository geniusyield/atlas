module Main (main) where

import GeniusYield.OnChain.AStakeValidator.Compiled (writeAStakeValidator)
import GeniusYield.OnChain.TestToken.Compiled (writeTestTokenPolicy)
import System.FilePath ((</>))

main :: IO ()
main =
  let getPath fn = "data" </> "compiled-scripts" </> fn <> ".bp"
   in do
        writeTestTokenPolicy $ getPath "test-token-policy"
        writeAStakeValidator $ getPath "a-stake-validator"