module Main (main) where

import           GeniusYield.Imports

import           Test.Tasty                           (defaultIngredients,
                                                       defaultMainWithIngredients,
                                                       includingOptions,
                                                       testGroup, withResource)
import           Test.Tasty.HUnit                     (testCaseSteps)
import           Test.Tasty.Ingredients               (Ingredient)

import           GeniusYield.CardanoApi.EraHistory
import           GeniusYield.Providers.CardanoDbSync
import           GeniusYield.Providers.LiteChainIndex
import           GeniusYield.Types

import           GeniusYield.Test.Privnet.Ctx
import qualified GeniusYield.Test.Privnet.Examples
import           GeniusYield.Test.Privnet.Options
import           GeniusYield.Test.Privnet.Setup

ingredients :: [Ingredient]
ingredients =
    includingOptions optionDescriptions :
    defaultIngredients

main :: IO ()
main = do
    defaultMainWithIngredients ingredients $
      askDbSyncOpts $ \dbSyncOpts ->
      withResource (makeSetup dbSyncOpts) (const mempty) $ \setup ->
      testGroup "atlas-privnet-tests"
          [ testCaseSteps "Balances" $ \info -> withSetup setup info $ \ctx -> do
              forM_ (zip [(1 :: Integer) ..] (ctxUserF ctx : ctxUsers ctx))
                (\(i, ctxUser) -> do
                  userIutxos <- gyQueryUtxosAtAddress (ctxProviders ctx) (userAddr ctxUser)
                  info $ unlines $
                      printf "User%s:" (if i == 1 then "F" else show i) :
                      [ printf "%s: %s" (utxoRef utxo) (utxoValue utxo)
                      | utxo <- utxosToList userIutxos
                      ]

                )
          , testCaseSteps "Chain index stats" $ \info -> withSetup setup info $ \ctx -> do
              ctxWaitNextSlot ctx
              (slot''', ndatums) <- lciStats $ ctxLCI ctx
              info $ printf "chain index @ slot %s with %d datum hashes known\n" slot''' ndatums

          , testCaseSteps "DB Sync slot" $ \info -> withSetup setup info $ \ctx -> do
              case ctxDbSync ctx of
                  Just dbSync -> do
                      slot <- dbSyncSlotNumber dbSync
                      info $ printf "db sync slot %s" slot

                  Nothing ->
                      info "No db sync connection"

          , testCaseSteps "SlotConfig" $ \info -> withSetup setup info $ \ctx -> do
              slot <- ctxCurrentSlot ctx
              info $ printf "Slot %s" slot

              sc <- ctxSlotConfig ctx
              info $ show sc

          , testCaseSteps "GetParameters" $ \info -> withSetup setup info $ \ctx -> do
              let providers = ctxProviders ctx

              systemStart <- gyGetSystemStart providers
              info $ printf "System start: %s" (show systemStart)

              stakePools <- gyGetStakePools providers
              info $ printf "Stake pools: %s" (show stakePools)

              -- TODO: how to select?
              eraHistory <- gyGetEraHistory providers
              info $ showEraSummaries eraHistory

              pp <- gyGetProtocolParameters providers
              info $ printf "Protocol parameters: %s" (show pp)

          , GeniusYield.Test.Privnet.Examples.tests setup

          ]

