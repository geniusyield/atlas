module GeniusYield.Test.GYTxBody
    ( gyTxBodyTests
    , mockTxId
    ) where

import qualified Cardano.Api                          as Api
import qualified Cardano.Api.Shelley                  as Api.S
import qualified Data.Set                             as Set (empty)
import           Data.Time.Clock.POSIX                (posixSecondsToUTCTime)
import           Numeric.Natural                      (Natural)
import           Test.Tasty                           (TestTree, testGroup)
import           Test.Tasty.HUnit                     (Assertion, testCase,
                                                       (@?=))

import           Plutus.Model.Fork.Ledger.TimeSlot    (scSlotLength,
                                                       scSlotZeroTime)
import           Plutus.Model.Mock.MockConfig         (defaultSlotConfig)
import           Plutus.Model.Mock.ProtocolParameters (PParams (BabbageParams),
                                                       defaultBabbageParams)

import           GeniusYield.Types.Address            (GYAddress,
                                                       unsafeAddressFromText)
import           GeniusYield.Types.SlotConfig         (gyscSystemStart,
                                                       simpleSlotConfig)
import           GeniusYield.Types.Time               (timeFromPlutus,
                                                       timeToPOSIX)
import           GeniusYield.Types.Tx                 (GYTxId)
import           GeniusYield.Types.TxOut              (GYTxOut,
                                                       mkGYTxOutNoDatum)
import           GeniusYield.Types.TxOutRef           (GYTxOutRef,
                                                       txOutRefFromTuple)
import           GeniusYield.Types.UTxO               (GYOutDatum (GYOutDatumNone),
                                                       GYUTxO (..), GYUTxOs,
                                                       utxosFromList)
import           GeniusYield.Types.Value              (GYAssetClass (..),
                                                       GYTokenName, GYValue,
                                                       valueFromList,
                                                       valueFromLovelace,
                                                       valueSingleton)

import           GeniusYield.Providers.Common         (mainnetEraHist)
import           GeniusYield.Transaction              (GYBuildTxEnv (..),
                                                       GYCoinSelectionStrategy (..),
                                                       balanceTxStep)
import           GeniusYield.Transaction.Common       (BalancingError (..),
                                                       adjustTxOut, minimumUTxO)
-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

gyTxBodyTests :: TestTree
gyTxBodyTests = testGroup "GYTxBody" [ testGroup "AdjustTx" adjustTxTests
                                     , testGroup "BalanceTxStep" balanceTxStepTests
                                     ]

adjustTxTests :: [TestTree]
adjustTxTests =
    [ testCase "No adjust needed" $
        10_000_000 `lovelacesAdjustedShouldEqual` 10_000_000
    , testCase "Few ADA" $
        2_000_000 `lovelacesAdjustedShouldEqual` 2_000_000
    , testCase "Very Few ADA" $
        100_000 `lovelacesAdjustedShouldEqual` 978_370
    , testCase "ADA and Assets" $ do
        let val = valueFromList [ (GYLovelace, 10_000_000)
                                  , (mockAsset "A", 100)
                                  , (mockAsset "B", 200)
                                  ]
        val `adjustedShouldEqual` val
    , testCase "Few ADA and Assets" $ do
        let val = valueFromList [ (GYLovelace, 2_000_000)
                                , (mockAsset "A", 100)
                                , (mockAsset "B", 200)
                                ]
        val `adjustedShouldEqual` val
    , testCase "Very Few ADA and Assets" $ do
        let val = valueFromList [ (GYLovelace, 100_000)
                                , (mockAsset "A", 100)
                                , (mockAsset "B", 200)
                                ]
        val `adjustedShouldEqual` (val <> valueFromLovelace 1_055_080)
    , testCase "Very Few ADA and a lot of Assets" $ do
        let val = valueFromList [ (GYLovelace, 100_000)
                                , (mockAsset "A", 1000)
                                , (mockAsset "B", 2000)
                                , (mockAsset "C", 3000)
                                , (mockAsset "D", 4000)
                                , (mockAsset "E", 5000)
                                , (mockAsset "F", 6000)
                                , (mockAsset "G", 7000)
                                , (mockAsset "H", 8000)
                                ]
        val `adjustedShouldEqual` (val <> valueFromLovelace 1_193_000)
    ]
  where
    mockAdjust :: GYTxOut v -> GYTxOut v
    mockAdjust = adjustTxOut (mockMinimumUTxO True)

    mockMinimumUTxO :: Bool -> GYTxOut v -> Natural
    mockMinimumUTxO b = minimumUTxO b mockProtocolParams

    lovelacesAdjustedShouldEqual :: Integer -> Integer -> Assertion
    lovelacesAdjustedShouldEqual n m =
        mockAdjust (mockTxOutFromLovelace n) @?= mockTxOutFromLovelace m

    adjustedShouldEqual :: GYValue -> GYValue -> Assertion
    adjustedShouldEqual v1 v2 = mockAdjust (mockTxOut v1) @?= mockTxOut v2

balanceTxStepTests :: [TestTree]
balanceTxStepTests =
    [ testCase "Empty OwnUtxos" $ do
        res <- balanceTxStep
                (mockBuildTxEnv mempty)
                True
                Nothing
                []
                []
                GYRandomImproveMultiAsset
                2_000_000
        res @?= Left BalancingErrorEmptyOwnUTxOs
    , testCase "No collateral needed" $ do
        Right (_, collaterals, _) <- balanceTxStep
                                        (mockBuildTxEnv [valueFromLovelace 10_000_000])
                                        True
                                        Nothing
                                        []
                                        []
                                        GYRandomImproveMultiAsset
                                        2_000_000
        collaterals @?= utxosFromList []

    , testCase "Collateral Needed" $ do
        Right (_, collaterals, _) <- balanceTxStep
                                        (mockBuildTxEnv [valueFromLovelace 10_000_000])
                                        True
                                        (Just (valueSingleton (mockAsset "A") 100, []))
                                        []
                                        []
                                        GYRandomImproveMultiAsset
                                        2_000_000
        collaterals @?= utxosFromList [collateralUtxo]
    ]

-------------------------------------------------------------------------------
-- Mock Values
-------------------------------------------------------------------------------

mockOutAddress :: GYAddress
mockOutAddress = unsafeAddressFromText "addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4"

mockChangeAddress :: GYAddress
mockChangeAddress = unsafeAddressFromText "addr_test1qrsuhwqdhz0zjgnf46unas27h93amfghddnff8lpc2n28rgmjv8f77ka0zshfgssqr5cnl64zdnde5f8q2xt923e7ctqu49mg5"

mockTxOutFromLovelace :: Integer -> GYTxOut v
mockTxOutFromLovelace = mockTxOut . valueFromLovelace

mockTxOut :: GYValue -> GYTxOut v
mockTxOut = mkGYTxOutNoDatum mockOutAddress

mockTxId :: GYTxId
mockTxId = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189"

mockTxOutRef :: GYTxOutRef
mockTxOutRef = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"

mockAsset :: GYTokenName -> GYAssetClass
mockAsset = GYToken "005eaf690cba88f441494e42f5edce9bd7f595c56f99687e2fa0aad4"

mockProtocolParams :: Api.S.ProtocolParameters
mockProtocolParams = Api.S.fromLedgerPParams Api.ShelleyBasedEraBabbage params
 where
    params = case defaultBabbageParams of
        BabbageParams p -> p
        _               -> error "Impossible"

collateralUtxo :: GYUTxO
collateralUtxo = GYUTxO
    { utxoRef       = mockTxOutRef
    , utxoAddress   = mockChangeAddress
    , utxoValue     = valueFromLovelace 1_234_567
    , utxoOutDatum  = GYOutDatumNone
    , utxoRefScript = Nothing
    }

mockBuildTxEnv :: [GYValue] -> GYBuildTxEnv
mockBuildTxEnv wallet = GYBuildTxEnv
    { gyBTxEnvSystemStart    = mockSystemStart
    , gyBTxEnvEraHistory     = Api.EraHistory Api.CardanoMode mainnetEraHist
    , gyBTxEnvProtocolParams = mockProtocolParams
    , gyBTxEnvPools          = Set.empty
    , gyBTxEnvOwnUtxos       = buildOwnUtxos wallet
    , gyBTxEnvChangeAddr     = mockChangeAddress
    , gyBTxEnvCollateral     = collateralUtxo
    }
  where
    slotLen = fromInteger (scSlotLength defaultSlotConfig) / 1000
    slotZero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $
        scSlotZeroTime defaultSlotConfig
    mockSystemStart = gyscSystemStart $ simpleSlotConfig slotZero slotLen

buildOwnUtxos :: [GYValue] -> GYUTxOs
buildOwnUtxos = utxosFromList . zipWith
    (\i v -> GYUTxO
        (txOutRefFromTuple (mockTxId, i))
        mockChangeAddress
        v
        GYOutDatumNone
        Nothing
    )
    [0..]
