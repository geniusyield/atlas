{- |
Module      : Main
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module Main (main) where

import GeniusYield.Imports

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import GeniusYield.CardanoApi.EraHistory
import GeniusYield.Types

import GeniusYield.Test.Privnet.Blueprint qualified
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.DRep qualified
import GeniusYield.Test.Privnet.Examples qualified
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Privnet.SimpleScripts qualified
import GeniusYield.Test.Privnet.Stake qualified
import GeniusYield.Test.Privnet.StakePool qualified
import GeniusYield.TxBuilder

main :: IO ()
main = do
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup ->
    defaultMain $
      testGroup
        "atlas-privnet-tests"
        [ testCaseSteps "Balances" $ \info -> withSetup info setup $ \ctx -> do
            forM_
              (zip [(1 :: Integer) ..] (ctxUserF ctx : ctxUsers ctx))
              ( \(i, ctxUser) -> do
                  userIutxos <- gyQueryUtxosAtAddress (ctxProviders ctx) (userAddr ctxUser) Nothing
                  info $
                    unlines $
                      printf "User%s:" (if i == 1 then "F" else show i)
                        : [ printf "%s: %s" (utxoRef utxo) (utxoValue utxo)
                          | utxo <- utxosToList userIutxos
                          ]
              )
        , testCaseSteps "SlotConfig" $ \info -> withSetup info setup $ \ctx -> do
            slot <- ctxSlotOfCurrentBlock ctx
            info $ printf "Slot %s" slot

            sc <- ctxSlotConfig ctx
            info $ show sc
        , testCaseSteps "GetParameters" $ \info -> withSetup info setup $ \ctx -> do
            ss <- ctxRunQuery ctx systemStart
            info $ printf "System start: %s" (show ss)

            sp <- ctxRunQuery ctx stakePools
            info $ printf "Stake pools: %s" (show sp)

            eh <- ctxRunQuery ctx eraHistory
            info $ showEraSummaries eh

            pp <- ctxRunQuery ctx protocolParams
            info $ printf "Protocol parameters: %s" (show pp)
        , GeniusYield.Test.Privnet.Blueprint.blueprintTests setup
        , GeniusYield.Test.Privnet.Examples.tests setup
        , GeniusYield.Test.Privnet.Stake.stakeKeyTests setup
        , GeniusYield.Test.Privnet.Stake.stakeValidatorTests setup
        , GeniusYield.Test.Privnet.SimpleScripts.simpleScriptsTests setup
        , GeniusYield.Test.Privnet.DRep.drepTests setup
        , GeniusYield.Test.Privnet.StakePool.stakePoolTests setup
        ]
