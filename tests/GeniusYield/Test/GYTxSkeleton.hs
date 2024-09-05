module GeniusYield.Test.GYTxSkeleton (
  gyTxSkeletonTests,
) where

import Data.Either (fromRight)
import Data.Map as Map (
  Map,
  empty,
  fromList,
  singleton,
 )
import Data.Maybe (fromJust)
import Data.Set as Set (fromList, singleton)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import GeniusYield.Types.Address (
  GYAddress,
  unsafeAddressFromText,
 )
import GeniusYield.Types.PlutusVersion (PlutusVersion (PlutusV2))
import GeniusYield.Types.PubKeyHash (
  GYPubKeyHash,
  pubKeyHashFromPlutus,
 )
import GeniusYield.Types.Redeemer (GYRedeemer, unitRedeemer)
import GeniusYield.Types.Script (
  GYMintScript (..),
  mintingPolicyFromApi,
  scriptFromCBOR,
  scriptToApi,
 )
import GeniusYield.Types.Slot (GYSlot, slotFromInteger)
import GeniusYield.Types.TxIn (
  GYTxIn (GYTxIn, gyTxInTxOutRef, gyTxInWitness),
  GYTxInWitness (GYTxInWitnessKey),
 )
import GeniusYield.Types.TxOut (GYTxOut, mkGYTxOutNoDatum)
import GeniusYield.Types.TxOutRef (GYTxOutRef)
import GeniusYield.Types.Value (
  GYTokenName,
  GYValue,
  unsafeTokenNameFromHex,
  valueFromLovelace,
 )

import GeniusYield.TxBuilder.Class (
  GYTxSkeleton (..),
  GYTxSkeletonRefIns (..),
  isInvalidAfter,
  isInvalidBefore,
  mustBeSignedBy,
  mustHaveInput,
  mustHaveOptionalOutput,
  mustHaveOutput,
  mustHaveRefInput,
  mustMint,
 )

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

gyTxSkeletonTests :: TestTree
gyTxSkeletonTests = testGroup "GYTxSkeleton" basicTests

basicTests :: [TestTree]
basicTests =
  [ testGroup
      "Constructors"
      [ testCase "mustHaveInput" $
          gytxIns (mustHaveInput mockTxIn) @?= [mockTxIn]
      , testCase "mustHaveReferenceInput" $
          gytxRefIns (mustHaveRefInput @'PlutusV2 mockTxOutRef) @?= GYTxSkeletonRefIns (Set.singleton mockTxOutRef)
      , testCase "mustHaveOutput" $
          gytxOuts (mustHaveOutput mockTxOut1) @?= [mockTxOut1]
      , testCase "mustHaveOptionalOutput (Just x)" $
          gytxOuts (mustHaveOptionalOutput (Just mockTxOut1)) @?= [mockTxOut1]
      , testCase "mustHaveOptionalOutput (Nothing)" $
          gytxOuts (mustHaveOptionalOutput Nothing) @?= []
      , testCase "mustMint" $
          gytxMint (mustMint mockMintingPolicy unitRedeemer mockTokenName 10) @?= mockMint
      , testCase "mustMint 0 is empty" $
          gytxMint (mustMint mockMintingPolicy unitRedeemer mockTokenName 0) @?= Map.empty
      , testCase "mustMint when burning" $
          gytxMint (mustMint mockMintingPolicy unitRedeemer mockTokenName (-10)) @?= mockBurn
      , testCase "mustBeSignedBy" $
          gytxSigs (mustBeSignedBy mockPkh1) @?= Set.singleton mockPkh1
      , testCase "isInvalidBefore" $
          gytxInvalidBefore (isInvalidBefore mockSlot) @?= Just mockSlot
      , testCase "isInvalidAfter" $
          gytxInvalidAfter (isInvalidAfter mockSlot) @?= Just mockSlot
      ]
  , testGroup
      "SemiGroup"
      [ testGroup
          "Input"
          [ testCase "Adding two inputs" $
              let skeleton1 = mustHaveInput mockTxIn
                  skeleton2 = mustHaveInput mockTxIn1
                  newSkeleton = skeleton1 <> skeleton2
               in gytxIns newSkeleton @?= [mockTxIn, mockTxIn1]
          , testCase "Adding the same inputs" $
              let skeleton1 = mustHaveInput mockTxIn
                  skeleton2 = mustHaveInput mockTxIn
                  newSkeleton = skeleton1 <> skeleton2
               in gytxIns newSkeleton @?= [mockTxIn]
          ]
      , testGroup
          "Ref Input"
          [ testCase "Adding two reference inputs - Just/Just" $
              let skeleton1 = mustHaveRefInput mockTxOutRef
                  skeleton2 = mustHaveRefInput mockTxOutRef1
                  newSkeleton = skeleton1 <> skeleton2 :: GYTxSkeleton 'PlutusV2
               in gytxRefIns newSkeleton @?= GYTxSkeletonRefIns (Set.fromList [mockTxOutRef, mockTxOutRef1])
          , testCase "Adding two reference inputs - Just/Nothing" $
              let skeleton1 = mustHaveRefInput mockTxOutRef
                  skeleton2 = mustHaveOptionalOutput Nothing -- This won't have any refInputs
                  newSkeleton = skeleton1 <> skeleton2 :: GYTxSkeleton 'PlutusV2
               in gytxRefIns newSkeleton @?= GYTxSkeletonRefIns (Set.singleton mockTxOutRef)
          , testCase "Adding two reference inputs - Nothing/Just" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have any refInputs
                  skeleton2 = mustHaveRefInput mockTxOutRef
                  newSkeleton = skeleton1 <> skeleton2 :: GYTxSkeleton 'PlutusV2
               in gytxRefIns newSkeleton @?= GYTxSkeletonRefIns (Set.singleton mockTxOutRef)
          , testCase "Adding two reference inputs - Nothing/Nothing" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have any refInputs
                  newSkeleton = skeleton1 <> skeleton1 :: GYTxSkeleton 'PlutusV2
               in gytxRefIns newSkeleton @?= GYTxSkeletonNoRefIns
          ]
      , testGroup
          "Output"
          [ testCase "Adding two outputs" $
              let skeleton1 = mustHaveOutput mockTxOut1
                  skeleton2 = mustHaveOutput mockTxOut2
                  newSkeleton = skeleton1 <> skeleton2
               in gytxOuts newSkeleton @?= [mockTxOut1, mockTxOut2]
          , testCase "Adding the same outputs" $
              let skeleton1 = mustHaveOutput mockTxOut1
                  skeleton2 = mustHaveOutput mockTxOut1
                  newSkeleton = skeleton1 <> skeleton2
               in gytxOuts newSkeleton @?= [mockTxOut1, mockTxOut1]
          ]
      , testGroup
          "Mint"
          [ testCase "Adding two mints - same token" $
              let skeleton1 = mustMint mockMintingPolicy unitRedeemer mockTokenName 10
                  skeleton2 = mustMint mockMintingPolicy unitRedeemer mockTokenName 20
                  newSkeleton = skeleton1 <> skeleton2
               in gytxMint newSkeleton @?= mockMint' 30
          , testCase "Adding one mint and one burn" $
              let skeleton1 = mustMint mockMintingPolicy unitRedeemer mockTokenName 10
                  skeleton2 = mustMint mockMintingPolicy unitRedeemer mockTokenName (-20)
                  newSkeleton = skeleton1 <> skeleton2
               in gytxMint newSkeleton @?= mockMint' (-10)
          , testCase "Adding two burns" $
              let skeleton1 = mustMint mockMintingPolicy unitRedeemer mockTokenName (-10)
                  skeleton2 = mustMint mockMintingPolicy unitRedeemer mockTokenName (-20)
                  newSkeleton = skeleton1 <> skeleton2
               in gytxMint newSkeleton @?= mockMint' (-30)
          , testCase "Adding two mints - different tokens" $
              let skeleton1 = mustMint mockMintingPolicy unitRedeemer mockTokenName 10
                  skeleton2 = mustMint mockMintingPolicy unitRedeemer mockTokenName1 20
                  newSkeleton = skeleton1 <> skeleton2
               in gytxMint newSkeleton @?= mockMintSum
          ]
      , testGroup
          "Required Signers"
          [ testCase "Adding two required signers" $
              let skeleton1 = mustBeSignedBy mockPkh1
                  skeleton2 = mustBeSignedBy mockPkh2
                  newSkeleton = skeleton1 <> skeleton2
               in gytxSigs newSkeleton @?= Set.fromList [mockPkh1, mockPkh2]
          , testCase "Adding the same required signers" $
              let skeleton1 = mustBeSignedBy mockPkh1
                  skeleton2 = mustBeSignedBy mockPkh1
                  newSkeleton = skeleton1 <> skeleton2
               in gytxSigs newSkeleton @?= Set.singleton mockPkh1
          ]
      , testGroup
          "InvalidBefore"
          [ testCase "Adding two invalidBefore - Just/Just" $
              let skeleton1 = isInvalidBefore $ mockSlot' 1000
                  skeleton2 = isInvalidBefore $ mockSlot' 2000
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidBefore newSkeleton @?= Just (mockSlot' 2000)
          , testCase "Adding two invalidBefore - Just/Nothing" $
              let skeleton1 = isInvalidBefore $ mockSlot' 1000
                  skeleton2 = mustHaveOptionalOutput Nothing -- This won't have isInvalidBefore set
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidBefore newSkeleton @?= Just (mockSlot' 1000)
          , testCase "Adding two invalidBefore - Nothing/Just" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have isInvalidBefore set
                  skeleton2 = isInvalidBefore $ mockSlot' 1000
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidBefore newSkeleton @?= Just (mockSlot' 1000)
          , testCase "Adding two invalidBefore - Nothing/Nothing" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have isInvalidBefore set
                  newSkeleton = skeleton1 <> skeleton1
               in gytxInvalidBefore newSkeleton @?= Nothing
          ]
      , testGroup
          "InvalidAfter"
          [ testCase "Adding two invalidAfter - Just/Just" $
              let skeleton1 = isInvalidAfter $ mockSlot' 1000
                  skeleton2 = isInvalidAfter $ mockSlot' 2000
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidAfter newSkeleton @?= Just (mockSlot' 1000)
          , testCase "Adding two invalidAfter - Just/Nothing" $
              let skeleton1 = isInvalidAfter $ mockSlot' 2000
                  skeleton2 = mustHaveOptionalOutput Nothing -- This won't have isInvalidAfter set
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidAfter newSkeleton @?= Just (mockSlot' 2000)
          , testCase "Adding two invalidAfter - Nothing/Just" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have isInvalidAfter set
                  skeleton2 = isInvalidAfter $ mockSlot' 2000
                  newSkeleton = skeleton1 <> skeleton2
               in gytxInvalidAfter newSkeleton @?= Just (mockSlot' 2000)
          , testCase "Adding two invalidAfter - Nothing/Nothing" $
              let skeleton1 = mustHaveOptionalOutput Nothing -- This won't have isInvalidAfter set
                  newSkeleton = skeleton1 <> skeleton1
               in gytxInvalidAfter newSkeleton @?= Nothing
          ]
      ]
  ]

-------------------------------------------------------------------------------
-- Mock Values
-------------------------------------------------------------------------------

-- Address where outputs go to.
mockOutAddress :: GYAddress
mockOutAddress = unsafeAddressFromText "addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4"

mockOutValue :: GYValue
mockOutValue = valueFromLovelace 10_000_000

mockTxOut1 :: GYTxOut v
mockTxOut1 = mkGYTxOutNoDatum mockOutAddress mockOutValue

mockTxOut2 :: GYTxOut v
mockTxOut2 = mkGYTxOutNoDatum mockOutAddress (mockOutValue <> mockOutValue)

mockPkh1 :: GYPubKeyHash
mockPkh1 = fromRight err $ pubKeyHashFromPlutus "e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d"
  where
    err = error "absurd"

mockPkh2 :: GYPubKeyHash
mockPkh2 = fromRight err $ pubKeyHashFromPlutus "69aeb93ec15eb963dda5176b66949fe1c2a6a38de1cbb80db89e2922"
  where
    err = error "absurd"

mockSlot :: GYSlot
mockSlot = mockSlot' 1000

mockSlot' :: Integer -> GYSlot
mockSlot' = fromJust . slotFromInteger

mockMint :: (Map (GYMintScript 'PlutusV2) (Map GYTokenName Integer, GYRedeemer))
mockMint = mockMint' 10

mockBurn :: (Map (GYMintScript 'PlutusV2) (Map GYTokenName Integer, GYRedeemer))
mockBurn = mockMint' (-10)

mockMint' :: Integer -> Map (GYMintScript 'PlutusV2) (Map GYTokenName Integer, GYRedeemer)
mockMint' n = Map.singleton mockMintingPolicy (Map.singleton mockTokenName n, unitRedeemer)

mockMintSum :: (Map (GYMintScript 'PlutusV2) (Map GYTokenName Integer, GYRedeemer))
mockMintSum = Map.singleton mockMintingPolicy (Map.fromList [(mockTokenName, 10), (mockTokenName1, 20)], unitRedeemer)

mockMintingPolicy :: GYMintScript 'PlutusV2
mockMintingPolicy = GYMintScript $ mintingPolicyFromApi @'PlutusV2 $ scriptToApi $ fromJust $ scriptFromCBOR @'PlutusV2 "5902a70100003232323232323232323232323232323232223232323232533301253330123371000290000a51153330123017001153330123375e980129d8799fd87a9f581c0312bfe52db5be9f48d9ee30270ba6459b4277c4b6a0a363b9c5f6e4ffd87a80ff003014301337546601c44a6660240022c2a6660286032666601c00a90001199980780ca4000eb4dd58009bab3016301730153754602c0022602c00226004602e0026eb0c050c054c04cdd500109919299980a299980a19805180b001180b000899805180b180b800980b180b8010a5014a22c60286ea8c054c058c058c058c040c050dd500198099baa30183300b300a482038a82860584cc02cc0292080beedb581614bd700b0b0a4c2c6666601844460046eacc05400cdd48011bab3013300e3012375400246666601a44460046eb4c05800cdd480b00090008a40002c602460226ea80114ccc03ccdc3a4000601c00426eb8c04400458c03c004dd51807980818071baa0012232323232323232325333014533301433710004002294454ccc050cdc380100089919299980b180d802099b88375a60300046eb4c060004528180b002180a8020a5014a22a66602866ebc0180144ccc050c02cc058c05c020c02cc058c05c01d288a503012002301100237540046ea8008c044008c040008c038dd500118069baa0022300f300937540024601e6600466e95200233002375000297ae0330023374a6660129452002480012f5c097ae057404644446600e44a666016002200a2a66601a66ebcc030c03c0040184c010c038c03c0044c008c040004004dd48009111980211299980400089128008a99980519baf3009300c00100413005300c00113002300d00100123230022330020020012300223300200200123007300730070015573eaae755cd2ab9e5742ae8922102475900370e90011ba5480001"

mockTokenName :: GYTokenName
mockTokenName = unsafeTokenNameFromHex "abc123"

mockTokenName1 :: GYTokenName
mockTokenName1 = unsafeTokenNameFromHex "def456"

mockTxOutRef :: GYTxOutRef
mockTxOutRef = "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8#1"

mockTxOutRef1 :: GYTxOutRef
mockTxOutRef1 = "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#0"

mockTxIn :: GYTxIn v
mockTxIn =
  GYTxIn
    { gyTxInTxOutRef = mockTxOutRef
    , gyTxInWitness = GYTxInWitnessKey
    }

mockTxIn1 :: GYTxIn v
mockTxIn1 =
  GYTxIn
    { gyTxInTxOutRef = mockTxOutRef1
    , gyTxInWitness = GYTxInWitnessKey
    }
