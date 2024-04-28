{-|
Module      : Main
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module Main (main) where

import           GeniusYield.Imports

import           Test.Tasty                        (defaultMain, testGroup,
                                                    withResource)
import           Test.Tasty.HUnit                  (testCaseSteps)

import           GeniusYield.CardanoApi.EraHistory
import           GeniusYield.Types

import           GeniusYield.Test.Privnet.Ctx
import qualified GeniusYield.Test.Privnet.Examples
import           GeniusYield.Test.Privnet.Setup
import qualified GeniusYield.Test.Privnet.Stake

main :: IO ()
main = do
    defaultMain $
      withResource makeSetup (const mempty) $ \setup ->
      testGroup "atlas-privnet-tests"
          [ testCaseSteps "Balances" $ \info -> withSetup setup info $ \ctx -> do
              forM_ (zip [(1 :: Integer) ..] (ctxUserF ctx : ctxUsers ctx))
                (\(i, ctxUser) -> do
                  userIutxos <- gyQueryUtxosAtAddress (ctxProviders ctx) (userAddr ctxUser) Nothing
                  info $ unlines $
                      printf "User%s:" (if i == 1 then "F" else show i) :
                      [ printf "%s: %s" (utxoRef utxo) (utxoValue utxo)
                      | utxo <- utxosToList userIutxos
                      ]

                )
          , testCaseSteps "SlotConfig" $ \info -> withSetup setup info $ \ctx -> do
              slot <- ctxSlotOfCurrentBlock ctx
              info $ printf "Slot %s" slot

              sc <- ctxSlotConfig ctx
              info $ show sc

          , testCaseSteps "GetParameters" $ \info -> withSetup setup info $ \ctx -> do
              let providers = ctxProviders ctx

              systemStart <- gyGetSystemStart providers
              info $ printf "System start: %s" (show systemStart)

              stakePools <- gyGetStakePools providers
              info $ printf "Stake pools: %s" (show stakePools)

              eraHistory <- gyGetEraHistory providers
              info $ showEraSummaries eraHistory

              pp <- gyGetProtocolParameters providers
              info $ printf "Protocol parameters: %s" (show pp)

          , GeniusYield.Test.Privnet.Examples.tests setup

          , GeniusYield.Test.Privnet.Stake.stakeKeyTests setup
          , GeniusYield.Test.Privnet.Stake.stakeValidatorTests setup

          ]

