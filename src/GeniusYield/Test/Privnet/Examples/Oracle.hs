{-|
Module      : GeniusYield.Test.Privnet.Examples.Oracle
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Examples.Oracle (tests) where

import           Control.Lens                                     ((&), (.~))
import qualified Data.Map.Strict                                  as Map
import           Test.Tasty                                       (TestTree, testGroup)
import           Test.Tasty.HUnit                                 (testCaseSteps)

import           GeniusYield.Examples.Gift
import           GeniusYield.Imports
import           GeniusYield.OnChain.Examples.ReadOracle.Compiled
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types

readOracleValidatorV2 :: GYValidator PlutusV2
readOracleValidatorV2 = validatorFromPlutus readOracleValidator

tests :: IO Setup -> TestTree
tests setup = testGroup "oracle"
    [ testCaseSteps "without-ref" $ \info -> withSetup setup info $ \ctx -> do
        let goldAC = ctxGold ctx

        -- create output with read oracle script
        txBodyPlaceOracle <- ctxRunI ctx User1 $ do
            addr <- scriptAddress readOracleValidatorV2
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
        void $ submitTx ctx User1 txBodyPlaceOracle

        -- fails: no reference input with datum
        assertThrown isTxBodyErrorAutoBalance $ ctxRunI ctx User1 $ do
            addr <- scriptAddress readOracleValidatorV2
            utxo <- utxosAtAddress addr
            datums <- utxosDatums utxo
            return (mconcat
                [ mustHaveInput GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness  = GYTxInWitnessScript
                        (GYInScript readOracleValidatorV2)
                        (datumFromPlutusData (d :: ()))
                        unitRedeemer
                    }
                | (ref, (_, _, d)) <- Map.toList datums
                ] :: GYTxSkeleton PlutusV2)

    , testCaseSteps "with-ref" $ \info -> withSetup setup info $ \ctx -> do
        let goldAC = ctxGold ctx

        -- address
        giftValidatorV2Addr <- ctxRunC ctx User1 $
            scriptAddress giftValidatorV2

        -- create output with input
        txBodyPlaceDatum <- ctxRunI ctx User1 $ do
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut giftValidatorV2Addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                    & gyTxOutDatumL .~ GYTxOutUseInlineDatum
                ]
        void $ submitTx ctx User1 txBodyPlaceDatum

        -- get datum ref.
        datumRef <- findOutput giftValidatorV2Addr txBodyPlaceDatum

        -- create output with read oracle script
        txBodyPlaceOracle <- ctxRunI ctx User1 $ do
            addr <- scriptAddress readOracleValidatorV2
            return $ mconcat
                [ mustHaveOutput $ mkGYTxOut addr (valueSingleton goldAC 10) (datumFromPlutusData ())
                ]
        void $ submitTx ctx User1 txBodyPlaceOracle

        txBodyConsume <- ctxRunI ctx User1 $ do
            addr <- scriptAddress readOracleValidatorV2
            utxo <- utxosAtAddress addr
            datums <- utxosDatums utxo
            return (mconcat $
                [ mustHaveInput GYTxIn
                    { gyTxInTxOutRef = ref
                    , gyTxInWitness  = GYTxInWitnessScript
                        (GYInScript readOracleValidatorV2)
                        (datumFromPlutusData (d :: ()))
                        unitRedeemer
                    }
                | (ref, (_, _, d)) <- Map.toList datums
                ] ++
                [ mustHaveRefInput datumRef
                ] :: GYTxSkeleton PlutusV2)

        void $ submitTx ctx User1 txBodyConsume
    ]
