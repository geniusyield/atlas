{- |
Module      : GeniusYield.Test.Privnet.Examples.Oracle
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Privnet.Examples.Oracle (tests) where

import Control.Lens ((.~))
import Data.Map.Strict qualified as Map
import Test.Tasty (
  TestTree,
  testGroup,
 )
import Test.Tasty.HUnit (testCaseSteps)

import GeniusYield.Examples.Gift
import GeniusYield.Imports
import GeniusYield.OnChain.Examples.ReadOracle.Compiled
import GeniusYield.Test.Privnet.Asserts
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types

readOracleValidatorV2 :: GYScript 'PlutusV2
readOracleValidatorV2 = validatorFromPlutus readOracleValidator

tests :: Setup -> TestTree
tests setup =
  testGroup
    "oracle"
    [ testCaseSteps "without-ref" $ \info -> withSetup info setup $ \ctx -> do
        let goldAC = ctxGold ctx

        -- create output with read oracle script
        ctxRun ctx (ctxUserF ctx) $ do
          addr <- scriptAddress readOracleValidatorV2
          txBodyPlaceOracle <-
            buildTxBody $
              mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
          signAndSubmitConfirmed_ txBodyPlaceOracle

        -- fails: no reference input with datum
        assertThrown isTxBodyErrorAutoBalance $ ctxRun ctx (ctxUserF ctx) $ do
          addr <- scriptAddress readOracleValidatorV2
          utxo <- utxosAtAddress addr Nothing
          datums <- utxosDatums utxo
          buildTxBody $
            mconcat
              [ mustHaveInput
                  GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness =
                        GYTxInWitnessScript
                          (GYInScript @PlutusV2 readOracleValidatorV2)
                          (datumFromPlutusData (d :: ()))
                          unitRedeemer
                    }
              | (ref, (_, _, d)) <- Map.toList datums
              ]
    , testCaseSteps "with-ref" $ \info -> withSetup info setup $ \ctx -> do
        let goldAC = ctxGold ctx

        -- address
        giftValidatorV2Addr <-
          ctxRunQuery ctx $
            scriptAddress giftValidatorV2

        -- create output with input
        txBodyPlaceDatum <- ctxRun ctx (ctxUserF ctx) $ do
          txBodyPlaceDatum <-
            buildTxBody $
              mconcat
                [ mustHaveOutput $
                    mkGYTxOut giftValidatorV2Addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                      & gyTxOutDatumL
                      .~ GYTxOutUseInlineDatum @'PlutusV2
                ]
          signAndSubmitConfirmed_ txBodyPlaceDatum
          pure txBodyPlaceDatum

        -- get datum ref.
        datumRef <- findOutput giftValidatorV2Addr txBodyPlaceDatum

        -- create output with read oracle script
        ctxRun ctx (ctxUserF ctx) $ do
          addr <- scriptAddress readOracleValidatorV2
          txBodyPlaceOracle <-
            buildTxBody $
              mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
          signAndSubmitConfirmed_ txBodyPlaceOracle

        ctxRun ctx (ctxUserF ctx) $ do
          addr <- scriptAddress readOracleValidatorV2
          utxo <- utxosAtAddress addr Nothing
          datums <- utxosDatums utxo
          txBodyConsume <-
            buildTxBody . mconcat $
              [ mustHaveInput
                  GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness =
                        GYTxInWitnessScript
                          (GYInScript @PlutusV2 readOracleValidatorV2)
                          (datumFromPlutusData (d :: ()))
                          unitRedeemer
                    }
              | (ref, (_, _, d)) <- Map.toList datums
              ]
                ++ [ mustHaveRefInput datumRef
                   ]

          signAndSubmitConfirmed_ txBodyConsume
    ]
