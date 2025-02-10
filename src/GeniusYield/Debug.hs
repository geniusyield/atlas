module GeniusYield.Debug (
  coreConfigIO,
  startDebugCps,
  testCps,
  stopDebugCps,
  eval',
  GYNetworkId (..),
) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Monad (void)
import GeniusYield.GYConfig (GYCoreConfig, coreConfigIO, withCfgProviders)
import GeniusYield.TxBuilder (GYTxGameMonadIO, runGYTxGameMonadIO)
import GeniusYield.Types (GYNetworkId (..), GYProviders)

{-

Utils to use with ghci to run Atlas' operations from repl.
Based on https://stackoverflow.com/questions/42425939/how-can-i-use-repl-with-cps-function

ghci> :m + GeniusYield.Debug GeniusYield.Types.UTxO GeniusYield.Types.Address GeniusYield.TxBuilder
ghci> cfg <- coreConfigIO "maestro-config.json"
ghci> (providers, ctrl) <- startDebugCps @_ @() $ testCps cfg
ghci> eval = eval' GYTestnetPreview providers
ghci> take 1 . utxosToList <$> eval (utxosAtAddresses [unsafeAddressFromText "addr_test1wqtcz4vq80zxr3dskdcuw7wtfq0vwssd7rrpnnvcvrjhp5sx7leew"] )
ghci> stopDebugCps ctrl ()

-}

startDebugCps :: forall a b. ((a -> IO b) -> IO b) -> IO (a, MVar b)
startDebugCps cps = do
  cpsVal <- newEmptyMVar @a
  stopAndRet <- newEmptyMVar @b
  void . forkIO $
    void . cps $ \c -> do
      putMVar cpsVal c
      b <- readMVar stopAndRet
      putStrLn "Done"
      pure b
  s <- takeMVar cpsVal
  return (s, stopAndRet)

stopDebugCps :: MVar b -> b -> IO ()
stopDebugCps = putMVar

testCps :: GYCoreConfig -> (GYProviders -> IO b) -> IO b
testCps c = withCfgProviders c "test"

eval' :: GYNetworkId -> GYProviders -> GYTxGameMonadIO a -> IO a
eval' = runGYTxGameMonadIO
