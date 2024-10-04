{-# LANGUAGE LambdaCase #-}

-- Test to signify correct functionality of reference inputs implementation.
-- TODO: Atlas currently doesn't support referring to the uninlined datum of reference input. But if that support is added, tests can be written utilising it here.

{- |
Module      : GeniusYield.Test.RefInput
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop
-}
module GeniusYield.Test.RefInput (
  refInputTests,
) where

import Test.Tasty (
  TestTree,
  testGroup,
 )

import GeniusYield.HTTP.Errors
import GeniusYield.Imports
import GeniusYield.Test.Clb
import GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
import GeniusYield.Test.Utils
import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types

gyGuessRefInputDatumValidator :: GYScript 'PlutusV2
gyGuessRefInputDatumValidator = validatorFromPlutus guessRefInputDatumValidator

refInputTests :: TestTree
refInputTests =
  testGroup
    "Reference Input"
    [ mkTestFor "Inlined datum" $ refInputTrace True 5 5 . testWallets
    , mkTestFor "Inlined datum - Wrong guess" $ mustFail . refInputTrace True 5 4 . testWallets
    , mkTestFor "Reference input must not be consumed" $
        mustFailWith (\case GYBuildTxException (GYBuildTxBalancingError (GYBalancingErrorInsufficientFunds _)) -> True; _ -> False)
          . tryRefInputConsume
          . testWallets
    ]

guessRefInputRun :: GYTxMonad m => GYTxOutRef -> GYTxOutRef -> Integer -> m ()
guessRefInputRun refInputORef consumeRef guess = do
  let redeemer = Guess guess
      skeleton :: GYTxSkeleton 'PlutusV2 =
        mustHaveInput
          GYTxIn
            { gyTxInTxOutRef = consumeRef
            , gyTxInWitness =
                GYTxInWitnessScript
                  (GYInScript gyGuessRefInputDatumValidator)
                  (datumFromPlutusData ())
                  (redeemerFromPlutusData redeemer)
            }
          <> mustHaveRefInput refInputORef
  buildTxBody skeleton >>= signAndSubmitConfirmed_

refInputTrace :: GYTxGameMonad m => Bool -> Integer -> Integer -> Wallets -> m ()
refInputTrace toInline actual guess Wallets {..} = do
  let myGuess :: Integer = guess
      outValue :: GYValue = valueFromLovelace 20_000_000
  refInputORef <- asUser w1 $ addRefInput toInline (userAddr w9) (datumFromPlutusData (RefInputDatum actual))
  withWalletBalancesCheckSimple [w1 := valueFromLovelace 0] . asUser w1 $ do
    gyLogInfo' "" $ printf "Reference input ORef %s" refInputORef
    addr <- scriptAddress gyGuessRefInputDatumValidator
    txBody <- buildTxBody . mustHaveOutput $ mkGYTxOut addr outValue (datumFromPlutusData ())
    tx <- signTxBody txBody
    txId <- submitTxConfirmed tx
    let mOrefIndices = findLockedUtxosInBody addr tx
    orefIndices <- maybe (throwAppError . someBackendError $ "Unable to get GYAddress from some Plutus.Address in txBody") return mOrefIndices
    oref <- case fmap (txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx) orefIndices of
      [oref'] -> return oref'
      _non_singleton -> throwAppError . someBackendError $ "expected exactly one reference"
    gyLogInfo' "" $ printf "Locked ORef %s" oref
    guessRefInputRun refInputORef oref myGuess

tryRefInputConsume :: GYTxGameMonad m => Wallets -> m ()
tryRefInputConsume Wallets {..} = do
  -- Approach: Create a new output with 60% of total ada. Mark this UTxO as reference input and try sending this same 60%, or any amount greater than 40% of this original balance. Since coin balancer can't consume this UTxO, it won't be able to build for it.
  asUser w1 $ do
    walletBalance <- queryBalance $ userAddr w1
    let walletLovelaceBalance = fst $ valueSplitAda walletBalance
        lovelaceToSend = (walletLovelaceBalance `div` 10) * 6 -- send 60% of total ada
        lovelaceToSendValue = valueFromLovelace lovelaceToSend
    txBody <- buildTxBody . mustHaveOutput $ mkGYTxOutNoDatum (userAddr w1) lovelaceToSendValue
    signAndSubmitConfirmed_ txBody
    let bodyUtxos = utxosToList $ txBodyUTxOs txBody
    desiredOutputRef <- case utxoRef <$> find (\GYUTxO {utxoValue} -> utxoValue == lovelaceToSendValue) bodyUtxos of
      Nothing -> throwAppError . someBackendError $ "Shouldn't happen: Couldn't find the desired UTxO"
      Just ref -> pure ref
    buildTxBody (mustHaveRefInput @'PlutusV2 desiredOutputRef <> mustHaveOutput (mkGYTxOutNoDatum (userAddr w1) lovelaceToSendValue))
      >>= signAndSubmitConfirmed_
