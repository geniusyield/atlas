module Main (main) where

import GeniusYield.OnChain.AStakeValidator.Compiled (writeAStakeValidator)
import GeniusYield.OnChain.Examples.ReadOracle.Compiled (writeReadOracleValidator)
import GeniusYield.OnChain.FakeCoin.Compiled (writeFakeCoin)
import GeniusYield.OnChain.GuessRefInputDatum.Compiled (writeGuessRefInputDatumValidator)
import GeniusYield.OnChain.TestToken.Compiled (writeTestTokenPolicy)
import System.FilePath ((</>))

main :: IO ()
main =
  let getPath fn = "data" </> "compiled-scripts" </> fn <> ".bp"
   in do
        writeTestTokenPolicy $ getPath "test-token-policy"
        writeAStakeValidator $ getPath "a-stake-validator"
        writeFakeCoin $ getPath "fake-coin"
        writeReadOracleValidator $ getPath "read-oracle-validator"
        writeGuessRefInputDatumValidator $ getPath "guess-ref-input-datum-validator"