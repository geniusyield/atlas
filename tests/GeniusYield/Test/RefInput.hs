-- Test to signify correct functionality of reference inputs implementation.
-- TODO: Atlas currently doesn't support referring to the uninlined datum of reference input. But if that support is added, tests can be written utilising it here.
module GeniusYield.Test.RefInput
    ( refInputTests
    ) where

import           Control.Monad.Reader
import           Plutus.Model
import           Test.Tasty                                           (TestTree,
                                                                       testGroup)

import           GeniusYield.Imports
import           GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types

gyGuessRefInputDatumValidator :: GYValidator 'PlutusV2
gyGuessRefInputDatumValidator = validatorFromPlutus guessRefInputDatumValidator

refInputTests :: TestTree
refInputTests = testGroup "Reference Input"
    [
      testRun "Inlined datum" $ refInputTrace True 5 5 332976
    , testRun "Inlined datum - Wrong guess" $ mustFail . refInputTrace True 5 4 332976
    ]

guessRefInputRun :: GYTxOutRef -> GYTxOutRef -> Integer -> GYTxMonadRun ()
guessRefInputRun refInputORef consumeRef guess = do
  let redeemer = Guess guess
      skeleton :: GYTxSkeleton 'PlutusV2 =
        mustHaveInput GYTxIn
          { gyTxInTxOutRef = consumeRef
          , gyTxInWitness  = GYTxInWitnessScript
              (GYInScript gyGuessRefInputDatumValidator)
              (datumFromPlutusData ())
              (redeemerFromPlutusData redeemer)
          } <>
        mustHaveRefInput refInputORef
  void $ sendSkeleton skeleton

refInputTrace :: Bool -> Integer -> Integer -> Integer -> Wallets -> Run ()
refInputTrace toInline actual guess fees Wallets{..} = do
  let myGuess :: Integer = guess
      outValue :: GYValue = valueFromLovelace 20_000_000
  mMOref <- runWallet w1 $ addRefInput toInline (walletAddress w9) (datumFromPlutusData (RefInputDatum actual))
  case mMOref of
    Nothing -> fail "Unable to create utxo to reference"
    Just Nothing -> fail "Couldn't find index for reference utxo in outputs"
    Just (Just refInputORef) ->
      void $ runWallet w1 $ do
        liftRun $ logInfo $ printf "Reference input ORef %s" refInputORef
        addr <- scriptAddress gyGuessRefInputDatumValidator
        (Tx _ plutusTxBody, txId) <- sendSkeleton' (mustHaveOutput $ mkGYTxOut addr outValue (datumFromPlutusData ()))
        let mOrefIndices = findLockedUtxosInBody (walletNetworkId w1) addr plutusTxBody
        orefIndices <- maybe (fail "Unable to get GYAddress from some Plutus.Address in txBody") return mOrefIndices
        oref        <- case fmap (txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx) orefIndices of
          [oref']        -> return oref'
          _non_singleton -> fail "expected exactly one reference"
        liftRun $ logInfo $ printf "Locked ORef %s" oref
        withWalletBalancesCheck [w1 := outValue <> valueNegate (valueFromLovelace fees)] $ do
          guessRefInputRun refInputORef oref myGuess

