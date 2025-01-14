{- |
Module      : GeniusYield.Test.FeeTracking
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.FeeTracking (feeTrackingTests) where

import Test.Tasty

import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types

import GeniusYield.Test.OnChain.AlwaysSucceeds.Compiled

data LovelaceConfig = WithLovelace | WithoutLovelace

mkAmt :: LovelaceConfig -> GYValue -> GYValue
mkAmt WithLovelace = (valueFromLovelace 10_000_000 <>)
mkAmt WithoutLovelace = (mempty <>)

type TestFunction = forall m. GYTxGameMonad m => Wallets -> GYValue -> m ()

feeTrackingTests :: TestTree
feeTrackingTests =
  testGroup
    "feetracker"
    [ mkTestFor "send to another" $ prepTest WithLovelace sendToOther
    , mkTestFor "send (no lovelace) to another" $ prepTest WithoutLovelace sendToOther
    , mkTestFor "send to address as w1, consume from address as w2" $ prepTest WithLovelace sendAndConsume
    , mkTestFor "send (no lovelace) to address as w1, consume from address as w2" $
        prepTest WithoutLovelace sendAndConsume
    , mkTestFor "send to address as w1, consume from address as w2 to create continuing output" $
        prepTest WithLovelace sendAndContinue
    , mkTestFor "send (without lovelace) to address as w1, consume from address as w2 to create continuing output" $
        prepTest WithoutLovelace sendAndContinue
    , mkTestFor "send to address as w1, consume from address as w1" $ prepTest WithLovelace selfConsume
    , mkTestFor "send (without lovelace) to address as w1, consume from address as w1" $ prepTest WithoutLovelace selfConsume
    , mkTestFor "send to address as w1, consume from address as w1 to create continuing output" $
        prepTest WithLovelace selfContinue
    , mkTestFor "send (without lovelace) to address as w1, consume from address as w1 to create continuing output" $
        prepTest WithoutLovelace selfContinue
    , mkTestFor "send to address as w1, partially consume from address as w1 to create partial continuing output" $
        selfPartialConsume WithLovelace
    , mkTestFor "send (without lovelace) to address as w1, partially consume from address as w1 to create partial continuing output" $
        selfPartialConsume WithoutLovelace
    ]

sendToOther :: TestFunction
sendToOther Wallets {w1, w2} amt = withWalletBalancesCheckSimple [w1 := valueNegate amt, w2 := amt] . asUser w1 $ do
  txBody <- buildTxBody . mustHaveOutput $ mkGYTxOut (userChangeAddress w2) amt unitDatum
  signAndSubmitConfirmed_ txBody

sendAndConsume :: TestFunction
sendAndConsume Wallets {w1, w2} amt = withWalletBalancesCheckSimple [w1 := valueNegate amt, w2 := amt] $ do
  target <- scriptAddress gyAlwaysSucceedsValidator
  txId <- asUser w1 $ do
    txBody <- buildTxBody . mustHaveOutput $ mkGYTxOut target amt unitDatum
    signAndSubmitConfirmed txBody
  asUser w2 $ do
    txBody <-
      buildTxBody @PlutusV1 . mustHaveInput $
        GYTxIn
          { gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined gyAlwaysSucceedsValidator) unitDatum unitRedeemer
          , gyTxInTxOutRef = txOutRefFromTuple (txId, 0)
          }
    signAndSubmitConfirmed_ txBody

sendAndContinue :: TestFunction
sendAndContinue Wallets {w1, w2} amt = withWalletBalancesCheckSimple [w1 := valueNegate amt, w2 := mempty] $ do
  target <- scriptAddress gyAlwaysSucceedsValidator
  txId <- asUser w1 $ do
    txBody <- buildTxBody . mustHaveOutput $ mkGYTxOut target amt unitDatum
    signAndSubmitConfirmed txBody
  asUser w2 $ do
    txBody <-
      buildTxBody @PlutusV1 $
        mconcat
          [ mustHaveInput $
              GYTxIn
                { gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined gyAlwaysSucceedsValidator) unitDatum unitRedeemer
                , gyTxInTxOutRef = txOutRefFromTuple (txId, 0)
                }
          , mustHaveOutput $ mkGYTxOut target amt unitDatum
          ]
    signAndSubmitConfirmed_ txBody

selfConsume :: TestFunction
selfConsume Wallets {w1} amt = withWalletBalancesCheckSimple [w1 := mempty] $ do
  target <- scriptAddress gyAlwaysSucceedsValidator
  asUser w1 $ do
    sendBody <- buildTxBody . mustHaveOutput $ mkGYTxOut target amt unitDatum
    txId <- signAndSubmitConfirmed sendBody
    consumeBody <-
      buildTxBody @PlutusV1 . mustHaveInput $
        GYTxIn
          { gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined gyAlwaysSucceedsValidator) unitDatum unitRedeemer
          , gyTxInTxOutRef = txOutRefFromTuple (txId, 0)
          }
    signAndSubmitConfirmed_ consumeBody

selfContinue :: TestFunction
selfContinue Wallets {w1} amt = withWalletBalancesCheckSimple [w1 := valueNegate amt] $ do
  target <- scriptAddress gyAlwaysSucceedsValidator
  asUser w1 $ do
    sendBody <- buildTxBody . mustHaveOutput $ mkGYTxOut target amt unitDatum
    txId <- signAndSubmitConfirmed sendBody
    continueBody <-
      buildTxBody @PlutusV1 $
        mconcat
          [ mustHaveInput $
              GYTxIn
                { gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined gyAlwaysSucceedsValidator) unitDatum unitRedeemer
                , gyTxInTxOutRef = txOutRefFromTuple (txId, 0)
                }
          , mustHaveOutput $ mkGYTxOut target amt unitDatum
          ]
    signAndSubmitConfirmed_ continueBody

selfPartialConsume :: GYTxGameMonad m => LovelaceConfig -> TestInfo -> m ()
selfPartialConsume lovelaceConf TestInfo {testWallets = Wallets {w1}, testGoldAsset} = do
  let amt = mkAmt lovelaceConf $ valueSingleton testGoldAsset 10
      partialAmt = mkAmt lovelaceConf $ valueSingleton testGoldAsset 5
  withWalletBalancesCheckSimple [w1 := valueNegate partialAmt] $ do
    target <- scriptAddress gyAlwaysSucceedsValidator
    asUser w1 $ do
      sendBody <- buildTxBody . mustHaveOutput $ mkGYTxOut target amt unitDatum
      txId <- signAndSubmitConfirmed sendBody
      continueBody <-
        buildTxBody @PlutusV1 $
          mconcat
            [ mustHaveInput $
                GYTxIn
                  { gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptInlined gyAlwaysSucceedsValidator) unitDatum unitRedeemer
                  , gyTxInTxOutRef = txOutRefFromTuple (txId, 0)
                  }
            , mustHaveOutput $ mkGYTxOut target partialAmt unitDatum
            ]
      signAndSubmitConfirmed_ continueBody

prepTest :: GYTxGameMonad m => LovelaceConfig -> TestFunction -> TestInfo -> m ()
prepTest lovelaceConf f TestInfo {testWallets, testGoldAsset} =
  f testWallets . mkAmt lovelaceConf $ valueSingleton testGoldAsset 10

gyAlwaysSucceedsValidator :: GYScript 'PlutusV2
gyAlwaysSucceedsValidator = validatorFromPlutus alwaysSucceedsValidator
