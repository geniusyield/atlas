module GeniusYield.Test.CoinSelection where

import           Control.Monad.Random                  (evalRand, mkStdGen)
import           Control.Monad.Trans.Except            (runExceptT)
import           Data.List                             (tails, (\\))

import           Test.QuickCheck
import           Test.QuickCheck.Monadic               as M
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.Transaction.CoinSelection
import           GeniusYield.Types

data CoinSelectionTestParams = CoinSelectionTestParams
    { cstpTxExtInps :: [GYValue]
    -- ^ Existing inputs in the transaction (not coming from own wallet).
    , cstpTxOwnInps :: [GYValue]
    -- ^ Existing own wallet inputs in the transaction.
    , cstpTxOuts    :: [GYValue]
    -- ^ Desired tx outputs.
    , cstpTxMint    :: GYValue
    -- ^ Value being minted in the transaction.
    , cstpOwnUtxos  :: [GYValue]
    -- ^ This shouldn't contain the collateral.
    }
    deriving Show

prettyTestParams :: CoinSelectionTestParams -> String
prettyTestParams CoinSelectionTestParams{..} = unlines
        [ "* Params:"
        , "\tExtInputs = [\n" ++ (unlines (map (("\t\t" ++) . show) cstpTxExtInps) ++ "\t]")
        , "\tOwnInputs = [\n" ++ (unlines (map (("\t\t" ++) . show) cstpTxOwnInps) ++ "\t]")
        , "\tTxOuts = [\n" ++ (unlines (map (("\t\t" ++) . show) cstpTxOuts) ++ "\t]")
        , "\tTxMint = " ++ show cstpTxMint
        , "\tOwnUtxos = [\n" ++ (unlines (map (("\t\t" ++) . show) cstpOwnUtxos)  ++ "\t]")
        ]

-- Constant address used across all coin selection tests, since the address does not affect the logic being tested.
mockChangeAddress :: GYAddress
mockChangeAddress = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"

-- Constant address where inputs come from.
mockInpAddress :: GYAddress
mockInpAddress = unsafeAddressFromText "addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4"

-- Constant address representing some script.
mockScriptAddress :: GYAddress
mockScriptAddress = unsafeAddressFromText "addr_test1wr9tm5vmtr2zn877qk6nctqt47tch7dduu6tfy8upnyt05qp7uqhd"

-- Constant address representing some recipient of a transaction.
mockRecipientAddress :: GYAddress
mockRecipientAddress = unsafeAddressFromText "addr_test1qp0m05v9tyvszktmhh4f8r66pkm79t98wc3xgfmvtjxglc4npshc5gt5a6ynzh5tgzf4f6slflglp9vp72004gkl7vqqc37ewh"

{- We use a very simple mock minimum tx out ada adjuster.

In general, the implications of a _real_ 'minimumUtxo' is simply that one might have to use a higher extraLovelace.
Currently, no clever tricks are used to handle this predictively/gracefully - we simply retry with double the lovelace overshoot.
As such, for testing the coin selection itself, the practicality of this mock function shouldn't be too relevant.

Note that this function cannot realistically fail even in practical scenarios. The only time it can throw a 'BalancingError' is
if the min ada field is missing from the given protocol params, which shouldn't happen with the officially supported providers.
-}
mockMinimumUtxo :: GYValue -> Natural
mockMinimumUtxo val = fromInteger . max 2_000_000 $ valueAssetClass val GYLovelace

-- We assume 2 ada overshoot is generally enough for all the tests. If it isn't, retry once more with twice the amount.
mockExtraLovelace :: Natural
mockExtraLovelace = 2_000_000

-- Used for existing inputs.
mockTxId1 :: GYTxId
mockTxId1 = "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"

-- Used for own utxos.
mockTxId2 :: GYTxId
mockTxId2 = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189"

mockAsset :: GYTokenName -> GYAssetClass
mockAsset = GYToken "005eaf690cba88f441494e42f5edce9bd7f595c56f99687e2fa0aad4"

gyToken :: GYAssetClass
gyToken = GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GY"

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

coinSelectionTests :: TestTree
coinSelectionTests = testGroup "CoinSelection" [ testGroup "LargestFirst" largestFirstTests
                                               , testGroup "RandomImprove" randomImproveTests
                                               ]

quickCheckTests :: GYCoinSelectionStrategy -> TestTree
quickCheckTests strat = testGroup "QuickCheck"
        [ testProperty "Additional inputs included in own utxos" $
            testCaseQuickCheckBody strat propInputsAreSubset
        , testProperty "Inputs can pay outputs" $
            testCaseQuickCheckBody strat propInputsAreEnough
        , testProperty "Change is enough" $
            testCaseQuickCheckBody strat propChangeIsEnough
        ]

largestFirstTests :: [TestTree]
largestFirstTests = [quickCheckTests GYLargestFirstMultiAsset]

randomImproveTests :: [TestTree]
randomImproveTests =
    [ quickCheckTests GYRandomImproveMultiAsset
    , testGroup "Basic"
        [ testCase "no extra input needed" $ do
            let expectedAdditionalInps = []
                expectedChangeOuts     = [valueFromLovelace 5_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps   = []
                    , cstpTxOwnInps   = [valueFromLovelace 10_000_000]
                    , cstpTxOuts      = [valueFromLovelace  3_000_000]
                    , cstpTxMint      = mempty
                    , cstpOwnUtxos    = []
                    }
        , testCase "no output" $ do
            let expectedAdditionalInps = []
                expectedChangeOuts     = [valueFromLovelace 4_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps   = []
                    , cstpTxOwnInps   = [valueFromLovelace 6_000_000]
                    , cstpTxOuts      = []
                    , cstpTxMint      = mempty
                    , cstpOwnUtxos    = []
                    }
        , testCase "no output and no change" $ do
            let expectedAdditionalInps = []
                expectedChangeOuts     = []
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps   = []
                    , cstpTxOwnInps   = [valueFromLovelace 2_000_000]
                    , cstpTxOuts      = []
                    , cstpTxMint      = mempty
                    , cstpOwnUtxos    = []
                    }
        , testCase "no inputs and no mint" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000]
                expectedChangeOuts     = [valueFromLovelace 3_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = []
                    , cstpTxOuts    = [valueFromLovelace 5_000_000]
                    , cstpTxMint    = mempty
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000]
                    }
        , testCase "burning all from inputs" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000]
                expectedChangeOuts     = [valueFromLovelace 3_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = [valueFromLovelace 5_000_000 <> valueSingleton (mockAsset "A") 5]
                    , cstpTxOuts    = [valueFromLovelace 10_000_000]
                    , cstpTxMint    = valueSingleton (mockAsset "A") (-5)
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000]
                    }
        , testCase "burning all from utxos" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000 <> valueSingleton (mockAsset "A") 5]
                expectedChangeOuts     = [valueFromLovelace 10_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = [valueFromLovelace 12_000_000]
                    , cstpTxOuts    = [valueFromLovelace 10_000_000]
                    , cstpTxMint    = valueSingleton (mockAsset "A") (-5)
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000 <> valueSingleton (mockAsset "A") 5]
                    }
        , testCase "burning some" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000]
                expectedChangeOuts     = [valueFromLovelace 10_000_000 <> valueSingleton (mockAsset "A") 5]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = [valueFromLovelace 12_000_000 <> valueSingleton (mockAsset "A") 10]
                    , cstpTxOuts    = [valueFromLovelace 10_000_000]
                    , cstpTxMint    = valueSingleton (mockAsset "A") (-5)
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000]
                    }
        , testCase "minting to output" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000]
                expectedChangeOuts     = [valueFromLovelace 3_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = []
                    , cstpTxOuts    = [valueFromLovelace 5_000_000 <> valueSingleton (mockAsset "A") 1]
                    , cstpTxMint    = valueSingleton (mockAsset "A") 1
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000]
                    }
        , testCase "minting to change" $ do
            let expectedAdditionalInps = [valueFromLovelace 10_000_000]
                expectedChangeOuts     = [valueFromLovelace 3_000_000 <> valueSingleton (mockAsset "A") 1]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = []
                    , cstpTxOuts    = [valueFromLovelace 5_000_000]
                    , cstpTxMint    = valueSingleton (mockAsset "A") 1
                    , cstpOwnUtxos  = [valueFromLovelace 10_000_000]
                    }
        , testCase "multiple outputs" $ do
            let expectedAdditionalInps = [valueFromLovelace 25_000_000]
                expectedChangeOuts     = [valueFromLovelace 2_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps = []
                    , cstpTxOwnInps = []
                    , cstpTxOuts    = [ valueFromLovelace 5_000_000
                                      , valueFromLovelace 10_000_000
                                      , valueFromLovelace 6_000_000
                                      ]
                    , cstpTxMint    = mempty
                    , cstpOwnUtxos  = [valueFromLovelace 25_000_000]
                    }
        , testCase "multi-asset basic" $ do
            let expectedAdditionalInps = [ valueFromLovelace 10_000_000
                                         , valueFromLovelace 2_000_000 <> valueSingleton (mockAsset "A") 1
                                         ]
                expectedChangeOuts     = [valueFromLovelace 7_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts $ CoinSelectionTestParams
                    { cstpTxExtInps   = []
                    , cstpTxOwnInps   = []
                    , cstpTxOuts      = [valueFromLovelace  3_000_000 <> valueSingleton (mockAsset "A") 1]
                    , cstpTxMint      = mempty
                    , cstpOwnUtxos    = [ valueFromLovelace 10_000_000
                                        , valueFromLovelace 2_000_000 <> valueSingleton (mockAsset "A") 1
                                        ]
                    }
        ]
    , testGroup "TokenSalePlaceOrder"
        [ testCase "wallet.zebra" $ do
            let expectedAdditionalInps = [valueFromLovelace 5000_000_000]
                expectedChangeOuts     = [valueFromLovelace 4696_000_000]
            testCaseBody expectedAdditionalInps expectedChangeOuts (tokenSalePlaceTestParams 300_000_000 zebra)
        , testCase "wallet.whale" $ do
            let expectedAdditionalInps =
                    [ valueFromLovelace 720_834_944
                    , valueFromLovelace 29_850_895
                    ]
                expectedChangeOuts =
                    [ valueFromLovelace 714_685_839]
            testCaseBody expectedAdditionalInps expectedChangeOuts (tokenSalePlaceTestParams 32_000_000 whale)
        , testCase "wallet.rideTheWave" $ do
            let expectedAdditionalInps =
                    [ valueFromLovelace 9750_000_000
                    , valueFromLovelace 3690_000_000
                    , valueFromLovelace 2997_540_000
                    , valueFromLovelace 1498_860_000
                    , valueFromLovelace 1498_860_000
                    , valueFromLovelace 1000_000_000
                    , valueFromLovelace 1_480_000 <> valueSingleton (mockAsset "A") 1
                    , valueFromLovelace 1_480_000 <> valueSingleton (mockAsset "B") 1
                    , valueFromLovelace 1_440_000 <> valueSingleton (mockAsset "C") 1
                    , valueFromLovelace 1_440_000 <> valueSingleton (mockAsset "D") 1
                    ]
                expectedChangeOuts     =
                    [ valueFromLovelace 535_100_000
                        <> valueSingleton (mockAsset "A") 1
                        <> valueSingleton (mockAsset "B") 1
                        <> valueSingleton (mockAsset "C") 1
                        <> valueSingleton (mockAsset "D") 1
                    ]
            testCaseBody expectedAdditionalInps expectedChangeOuts (tokenSalePlaceTestParams 19_902_000_000 rideTheWave)
        ]
    ]
  where
    testCaseBody expectedAdditionalInps expectedChangeOuts params = do
        case runCoinSelectionTest GYRandomImproveMultiAsset params of
            Left err -> assertFailure $ "Selection failed: " ++ show err
            Right x  -> (expectedAdditionalInps, expectedChangeOuts) @=? x

-------------------------------------------------------------------------------
-- Transaction representing place token sale order
-------------------------------------------------------------------------------

{- | Represent a token sale place order transaction given the payment amount being made
(for some amount of tokens at some price), and the wallet distribution.
-}
tokenSalePlaceTestParams :: Natural -> [GYValue] -> CoinSelectionTestParams
tokenSalePlaceTestParams payment wallet = CoinSelectionTestParams
    { cstpTxExtInps = []
    , cstpTxOwnInps = []
    , cstpTxOuts    = [valueInsert GYLovelace lovelaceAmount gyTokenVal <> valueFromLovelace (fromIntegral payment)]
    , cstpTxMint    = gyTokenVal
    , cstpOwnUtxos  = wallet
    }
  where
    lovelaceAmount = max minLovelace $ valueAssetClass gyTokenVal GYLovelace
    minLovelace    = toInteger $ mockMinimumUtxo gyTokenVal
    gyTokenVal     = valueSingleton gyToken 1

-------------------------------------------------------------------------------
-- Different mock wallet distributions
-------------------------------------------------------------------------------

{- UTxO list

- 5k Ada
- 3 Ada, 1 A, 1 B, 1 C, 1 D, 1 E, 1 F
- 2 Ada

This represents a basic/typical preprod wallet.
-}
zebra :: [GYValue]
zebra =
    [ valueFromLovelace 5000_000_000
    , valueFromLovelace 3_000_000
        <> valueSingleton (mockAsset "A") 1
        <> valueSingleton (mockAsset "B") 1
        <> valueSingleton (mockAsset "C") 1
        <> valueSingleton (mockAsset "D") 1
        <> valueSingleton (mockAsset "E") 1
        <> valueSingleton (mockAsset "F") 1
    , valueFromLovelace 2_000_000
    ]

-- Skimmed representation of a randomly picked mainnet wallet.
whale :: [GYValue]
whale =
    [ valueFromLovelace 1_202_490 <> valueSingleton (mockAsset "X") 1
    , valueFromLovelace 720_834_944
    , valueFromLovelace 8_238_547
        <> valueSingleton (mockAsset "A") 1
        <> valueSingleton (mockAsset "B") 1
        <> valueSingleton (mockAsset "C") 1
        <> valueSingleton (mockAsset "D") 1
        <> valueSingleton (mockAsset "E") 1
        <> valueSingleton (mockAsset "F") 150_951_806
        <> valueSingleton (mockAsset "G") 58
        <> valueSingleton (mockAsset "H") 1
        <> valueSingleton (mockAsset "I") 1
    , valueFromLovelace 4_620_320
        <> valueSingleton (mockAsset "Ax") 1_166
        <> valueSingleton (mockAsset "Bx") 1
        <> valueSingleton (mockAsset "Cx") 990_000_000
        <> valueSingleton (mockAsset "Dx") 1_464_223
        <> valueSingleton (mockAsset "Ex") 1
        <> valueSingleton (mockAsset "Fx") 1
        <> valueSingleton (mockAsset "Gx") 1
        <> valueSingleton (mockAsset "Hx") 122_000_000
        <> valueSingleton (mockAsset "Ix") 24_403_870
        <> valueSingleton (mockAsset "Jx") 1
        <> valueSingleton (mockAsset "Kx") 1
    , valueFromLovelace 5_000_000
    , valueFromLovelace 29_850_895
    ]

-- | https://cexplorer.io/tx/c2fb472634f2cbb9f1d38db44e8beca1d10acdf91b4c8b943c858e0302b5a240
rideTheWave :: [GYValue]
rideTheWave =
    [ valueFromLovelace 9750_000_000
    , valueFromLovelace 3690_000_000
    , valueFromLovelace 2997_540_000
    , valueFromLovelace 1498_860_000
    , valueFromLovelace 1498_860_000
    , valueFromLovelace 1000_000_000
    , valueFromLovelace 1_480_000 <> valueSingleton (mockAsset "A") 1
    , valueFromLovelace 1_480_000 <> valueSingleton (mockAsset "B") 1
    , valueFromLovelace 1_440_000 <> valueSingleton (mockAsset "C") 1
    , valueFromLovelace 1_440_000 <> valueSingleton (mockAsset "D") 1
    ]

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

{- | Runs 'selectInputs' with the given strategy and test params.

Returns a pair where the first element is a list of additional inputs chosen, and the second element
is a list of change outputs generated.
-}
runCoinSelectionTest :: GYCoinSelectionStrategy -> CoinSelectionTestParams -> Either BalancingError ([GYValue], [GYValue])
runCoinSelectionTest cstrat cstParams = do
    (additionalInps, changeOuts) <- (`evalRand` pureStdGen)
        . runExceptT $ selectInputs (coinSelectionTestParamsToEnv cstParams) cstrat
    let inpVals = gyTxInDetValue <$> additionalInps
        changeVals = gyTxOutValue <$> changeOuts
    pure (inpVals, changeVals)
  where
    -- We use a pure StdGen for reproducible tests.
    pureStdGen = mkStdGen 936 -- 42 wasn't random enough.

coinSelectionTestParamsToEnv :: CoinSelectionTestParams -> GYCoinSelectionEnv v
coinSelectionTestParamsToEnv CoinSelectionTestParams {cstpTxExtInps, cstpTxOwnInps, cstpTxOuts, cstpTxMint, cstpOwnUtxos} =
    buildEnvWith
        ownUtxos
        inps
        -- TODO: Should we use different recipient addresses for each output? #36
        --       (https://github.com/geniusyield/atlas/issues/36)
        ((mockRecipientAddress, ) <$> cstpTxOuts)
        cstpTxMint
  where
    ownUtxos = buildOwnUtxos cstpOwnUtxos
    inps     = buildInps cstpTxExtInps cstpTxOwnInps

buildEnvWith :: GYUTxOs -> [GYTxInDetailed v] -> [(GYAddress, GYValue)] -> GYValue -> GYCoinSelectionEnv v
buildEnvWith ownUtxos existingInps targetOuts mintVal = GYCoinSelectionEnv
    { existingInputs  = existingInps
    , requiredOutputs = targetOuts
    , mintValue       = mintVal
    , changeAddr      = mockChangeAddress
    , ownUtxos        = ownUtxos
    , extraLovelace   = mockExtraLovelace
    , minimumUTxOF    = mockMinimumUtxo . gyTxOutValue
    , maxValueSize    = 5000 -- Current max value size (obtained from protocol params)
    }

buildInps :: [GYValue] -> [GYValue] -> [GYTxInDetailed v]
buildInps ext own = go (ext ++ own)
  where
    go = zipWith
        (\i v -> GYTxInDetailed
            (GYTxIn (txOutRefFromTuple (mockTxId1, i)) GYTxInWitnessKey)
            mockInpAddress
            v
            Nothing
            False
        )
        [0..]

buildOwnUtxos :: [GYValue] -> GYUTxOs
buildOwnUtxos = utxosFromList . zipWith
    (\i v -> GYUTxO
        (txOutRefFromTuple (mockTxId2, i))
        mockChangeAddress
        v
        GYOutDatumNone
        Nothing
    )
    [0..]

-------------------------------------------------------------------------------
-- QuickCheck Props
-------------------------------------------------------------------------------
{- QuickCheck tests for certain properties of the CoinSelection algorithms. All
    properties must hold, regardless of the strategy used. The properties being
    tested are:

    * checkInputsAreSubset: Checks that the additionalInputs chosen are valid
        utxos from the list of ownUtxos.

    * checkInputsAreEnough: Checks that the sum of existingInputs and
        additionalInputs are greater than the requiredOutputs

    * checkChangeIsEnough: Ensures that the value has all the assets
        from using the inputs to pay the outputs. Checks the following equation

            changeOuts >= (existingInputs + additionalInputs) - (requiredOuts + extraLovelace)

    The third property doesn't check ADA change.The coin-selection algorithm
        doesn't guarantee that all ADA left from using the inputs are contained
        in the changeOuts. The next step in the balancing flow will take care of
        that.

    If a given test case doesn't have enough funds to cover the minADA for each
        change output, it will throw a BalancingErrorChangeShortFall and it will
        be discarded. We can't check for this case in the generation of the
        params as we can't predict the amount of change outputs.
-}

testCaseQuickCheckBody
    :: GYCoinSelectionStrategy
    -> (GYCoinSelectionEnv v -> [GYTxInDetailed v] -> [GYTxOut v] -> Bool)
    -> Property
testCaseQuickCheckBody strat prop = forAllShrinkShow genParamsLovelace shrinkParamsLovelace prettyParamsLovelace $
    \(cstParams, extraLov) -> monadicIO $ do
        let cstEnv = (coinSelectionTestParamsToEnv cstParams) { extraLovelace = extraLov }
        pre $ paramsInputsAreValid extraLov cstParams
        pre $ outputsHaveLovelace cstEnv
        seed <- run $ generate arbitrary
        case (`evalRand` mkStdGen seed) . runExceptT $ selectInputs cstEnv strat of
            Left (BalancingErrorChangeShortFall _) -> discard
            Left err -> fail $ show err
            Right (addInputs, changeOuts) -> monitor (counterexample (getReason addInputs changeOuts)) >>
                                            M.assert (prop cstEnv addInputs changeOuts)
  where
    getReason addInputs changeOuts = unlines [ "* AdditionalInputs: " ++ show addInputs
                                             , "* ChangeOuts: " ++ show changeOuts]
    outputsHaveLovelace env = all (\(_,v) -> valueAssetClass v GYLovelace > 0) (requiredOutputs env)

propInputsAreSubset :: GYCoinSelectionEnv v -> [GYTxInDetailed v] -> [GYTxOut v] -> Bool
propInputsAreSubset env addIns _ = all ((`elem` utxosRefs (ownUtxos env)) . gyTxInTxOutRef . gyTxInDet) addIns

propInputsAreEnough :: GYCoinSelectionEnv v -> [GYTxInDetailed v] -> [GYTxOut v] -> Bool
propInputsAreEnough env addIns _ =
        allInputsValue `valueGreaterOrEqual` allOutputsValue
  where
    allInputsValue = sumEntries (existingInputs env ++ addIns) <> mintValue env
    allOutputsValue = mconcat $ map snd (requiredOutputs env)

propChangeIsEnough :: GYCoinSelectionEnv v -> [GYTxInDetailed v] -> [GYTxOut v] -> Bool
propChangeIsEnough env addIns changeOuts = changeAssets == txAssets
  where
    changeValue = mconcat (map gyTxOutValue changeOuts)
    changeAssets = snd $ valueSplitAda changeValue
    allInputsValue = sumEntries (existingInputs env ++ addIns) <> mintValue env
    allOutputsValue = mconcat (map snd (requiredOutputs env)) <> naturalToValue (extraLovelace env)
    txAssets = snd $ valueSplitAda $ allInputsValue `valueMinus` allOutputsValue

-------------------------------------------------------------------------------
-- QuickCheck Generators
-------------------------------------------------------------------------------

{- Generates a random CoinSelectionTestParams.

    It ensures that the sum of the existing inputs and the ownUtxos are able to
    pay for the outputs and the given extraLovelace.

    It generates values with only ADAs, a single asset or a collection of assets
-}
genCoinSelectionParams :: Natural -> Gen CoinSelectionTestParams
genCoinSelectionParams extraLovelace = do
    outs <- listOf genGYValue
    (extIns, ownIns, ownUtxos, minted) <- genValidInputs outs
    return CoinSelectionTestParams
        { cstpTxExtInps = extIns
        , cstpTxOwnInps = ownIns
        , cstpTxOuts    = outs
        , cstpTxMint    = minted
        , cstpOwnUtxos  = ownUtxos
        }
  where
    genGYAssetClass :: Gen GYAssetClass
    genGYAssetClass = elements $ map mockAsset ["A","B","C","D","E","F","G","H","I"]

    genGYValue :: Gen GYValue
    genGYValue = oneof [genLovelaceValue, genSingleAssetValue, genAssetValue]

    genLovelaceValue :: Gen GYValue
    genLovelaceValue = valueFromLovelace <$> chooseInteger (2_000_000, 200_000_000)

    genSingleAssetValue :: Gen GYValue
    genSingleAssetValue = do
        lovelaceVal <- genLovelaceValue
        assetClass  <- genGYAssetClass
        amount      <- chooseInteger (1, 10_000)
        return (lovelaceVal <> valueSingleton assetClass amount)

    genAssetValue :: Gen GYValue
    genAssetValue = do
        lovelaceVal  <- genLovelaceValue
        assetClasses <- listOf1 genGYAssetClass
        amounts      <- vectorOf (length assetClasses) $ chooseInteger (1, 10_000)
        return $ lovelaceVal <> valueFromList (zip assetClasses amounts)

    genInputs :: Gen ([GYValue], [GYValue], [GYValue], GYValue)
    genInputs = do
        extIns   <- listOf genGYValue
        ownIns   <- listOf genGYValue
        ownUtxos <- listOf genGYValue
        minted   <- frequency [(3, genAssetValue), (1, elements [mempty])]
        let assetsMinted = snd $ valueSplitAda minted
        return (extIns, ownIns, ownUtxos, assetsMinted)

    genValidInputs :: [GYValue] -> Gen ([GYValue], [GYValue], [GYValue], GYValue)
    genValidInputs outs = genInputs `suchThat` inputsAreValid outs extraLovelace

genParamsLovelace :: Gen (CoinSelectionTestParams, Natural)
genParamsLovelace = do
    el     <- elements extraLovelaceValues
    params <- genCoinSelectionParams el
    return (params, el)

shrinkParamsLovelace :: (CoinSelectionTestParams, Natural) -> [(CoinSelectionTestParams, Natural)]
shrinkParamsLovelace (params, el) = [(nParams, el) | nParams <- shrinkParams params]
                                    ++ [(params, nEl) | nEl <- shrinkExtraLovelace el]

-------------------------------------------------------------------------------
-- QuickCheck Shrinks
-------------------------------------------------------------------------------

{- To efficiently shrink the CoinSelectionTestParams we first try to shrink
    each list of GYValues. `shrinkList` will try to remove as many values as
    possible. And if none of them can be removed, it will shrink each individual
    value using `shrinkValue`.

    We then move into the `collapseValues` stage where we try to reduce the
    amount of values in the list by adding a pair of them together.

    The order outs -> mint -> extInputs -> ownInputs -> ownUtxos was the most
    efficient order tested.
-}
shrinkParams :: CoinSelectionTestParams -> [CoinSelectionTestParams]
shrinkParams params@CoinSelectionTestParams{..} =
        [ params { cstpTxOuts = nOuts } | nOuts <- shrinkValues cstpTxOuts]
        ++
        [ params { cstpTxMint = nMint } | nMint <- shrinkValue cstpTxMint]
        ++
        [ params { cstpTxExtInps = nExtInps } | nExtInps <- shrinkValues cstpTxExtInps]
        ++
        [ params { cstpTxOwnInps = nOwnInps } | nOwnInps <- shrinkValues cstpTxOwnInps]
        ++
        [ params { cstpOwnUtxos = nOwnUtxos } | nOwnUtxos <- shrinkValues cstpOwnUtxos]
        ++
        [ params { cstpTxOuts = nOuts } | nOuts <- collapseValues cstpTxOuts]
        ++
        [ params { cstpTxExtInps = nExtInps } | nExtInps <- collapseValues cstpTxExtInps]
        ++
        [ params { cstpTxOwnInps = nOwnInps } | nOwnInps <- collapseValues cstpTxOwnInps]
        ++
        [ params { cstpOwnUtxos = nOwnUtxos } | nOwnUtxos <- collapseValues cstpOwnUtxos]

-- To collapseValues we take every possible pair and try to add them together.
collapseValues :: [GYValue] -> [[GYValue]]
collapseValues []  = []
collapseValues [_] = [[]]
collapseValues vs  = [] : [mconcat vs] : [(v <> v') : (vs \\ [v,v']) | (v,v') <- pairs vs]

shrinkExtraLovelace :: Natural -> [Natural]
shrinkExtraLovelace el = [nEl | nEl <- extraLovelaceValues, nEl < el]

{- To shrink a value we only try to remove as many assetClasses from it as
   possible. Trying to shrink the actual amount of each value using
   `shrinkIntegral` took a long of time.
-}
shrinkValue :: GYValue -> [GYValue]
shrinkValue = map valueFromList . shrinkList shrinkNothing . valueToList

-- We filter lists that only have one empty value in them
shrinkValues :: [GYValue] -> [[GYValue]]
shrinkValues = map removeEmpties . shrinkList shrinkValue

removeEmpties :: [GYValue] -> [GYValue]
removeEmpties [] = []
removeEmpties (x:xs) | x == mempty = removeEmpties xs
                     | otherwise = x : removeEmpties xs

-------------------------------------------------------------------------------
-- QuickCheck Utils
-------------------------------------------------------------------------------

prettyParamsLovelace :: (CoinSelectionTestParams, Natural) -> String
prettyParamsLovelace (params, el) = prettyTestParams params ++ "\n* ExtraLovelace: " ++ show el

{- Inputs are valid as long as:
    * The sum of existings inputs, the minted values and the ownUtxos is greater
        than the requiredOuts + the extraLovelace
-}
inputsAreValid :: [GYValue] -> Natural -> ([GYValue], [GYValue], [GYValue], GYValue) -> Bool
inputsAreValid outs extraLovelace (extIns, ownIns, ownUtxos, minted) =
        mconcat (minted : extIns ++ ownIns ++ ownUtxos) `valueGreaterOrEqual` (mconcat outs <> naturalToValue extraLovelace)

-- Easier way to call `inputsAreValid` for a given CoinSelectionTestParams
paramsInputsAreValid :: Natural -> CoinSelectionTestParams -> Bool
paramsInputsAreValid eLovelace CoinSelectionTestParams{..} =
    inputsAreValid cstpTxOuts eLovelace (cstpTxExtInps, cstpTxOwnInps, cstpOwnUtxos, cstpTxMint)

naturalToValue :: Natural -> GYValue
naturalToValue = valueFromLovelace . toInteger

-- Get all possible pairs of values from a list, without duplicates
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

extraLovelaceValues :: [Natural]
extraLovelaceValues = [2_000_000, 4_000_000 .. 10_000_000]

sumEntries :: [GYTxInDetailed v] -> GYValue
sumEntries = foldMap gyTxInDetValue
