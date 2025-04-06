module Main (main) where

import GeniusYield.OnChain.TestToken.Compiled (writeTestTokenPolicy)
import System.FilePath ((</>))

main :: IO ()
main =
  let getPath fn = "data" </> "compiled-scripts" </> fn <> ".bp"
   in writeTestTokenPolicy $ getPath "test-token-policy"