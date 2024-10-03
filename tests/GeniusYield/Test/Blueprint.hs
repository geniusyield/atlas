{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

-- Note: See [this](https://stackoverflow.com/a/69678961/20330802) answer on where one can find dumped splice file. As an example, @dist-newstyle/build/aarch64-osx/ghc-9.6.5/atlas-cardano-0.6.0/t/atlas-tests/build/atlas-tests/atlas-tests-tmp/tests/GeniusYield/Test@.

module GeniusYield.Test.Blueprint (
  blueprintTests,
) where

import System.FilePath

import Codec.Serialise qualified as CBOR
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types (writeScript)
import GeniusYield.Types.Blueprint
import GeniusYield.Types.PlutusVersion (PlutusVersion (..))
import GeniusYield.Types.Script (hashScript)
import PlutusLedgerApi.Common qualified as Plutus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?), (@?=))

simpleBlueprint :: ContractBlueprint
simpleBlueprint =
  MkContractBlueprint
    { contractId = Nothing
    , contractPreamble =
        MkPreamble
          { preambleTitle = "foo/bar"
          , preambleDescription = Just "Aiken contracts for project 'foo/bar'"
          , preambleVersion = "0.0.0"
          , preamblePlutusVersion = PlutusV3
          , preambleLicense = Just "Apache-2.0"
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "baz.baz.spend"
              , validatorDescription = Nothing
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "redeemer"
                    , argumentDescription = Nothing
                    , argumentPurpose = mempty
                    , argumentSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/baz~1MyRedeemer"
                    }
              , validatorDatum =
                  Just $
                    MkArgumentBlueprint
                      { argumentTitle = Just "datumOpt"
                      , argumentDescription = Nothing
                      , argumentPurpose = mempty
                      , argumentSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/baz~1MyDatum"
                      }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "isTrue"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Bool"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "2nd_arg"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/baz~1ParamConstr"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "primitiveInt"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "primitiveBA"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/ByteArray"
                      }
                  ]
              , validatorCompiled =
                  Just $
                    MkCompiledValidator
                      { compiledValidatorCode = "59015c0101003232323232322322232232253330093232323232533300e3370e900118079baa001132323253323301230013013375400c264646464a66603260360042646464a666032601060346ea80244c8c94ccc06c0644cdc399b803370066e00cdc019b80337000026eb4c07cc074dd50079b8d375c603e6040603a6ea803c010dc680180b1b8d014481e0528299980d1804980d9baa00113232337006eb4c080008dc68009bae301f3020001301c37540022646466e00dd698100011b8d001375c603e604000260386ea8004c074c06cdd50048b1bae301c301d002375a603600260306ea804c58dd7180c800980c8011bad30170013014375400c6e1d200016301430150023013001301037540022646464c6eb8c050c054008dd6980980098081baa00b3011301200230100013010002300e001300b375400229309b2b1bae001375a00266e1d200230033754002ae6955ceaab9e5573eae855d11"
                      , compiledValidatorHash = "8365f50a4b02c51505e73ce2aef01370cd34afe3bbe68a157c5f7301"
                      }
              }
          , MkValidatorBlueprint
              { validatorTitle = "baz.baz.else"
              , validatorDescription = Nothing
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Nothing
                    , argumentDescription = Nothing
                    , argumentPurpose = mempty
                    , argumentSchema = SchemaBuiltInData emptySchemaInfo
                    }
              , validatorDatum = Nothing
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "isTrue"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Bool"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "2nd_arg"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/baz~1ParamConstr"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "primitiveInt"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "primitiveBA"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/ByteArray"
                      }
                  ]
              , validatorCompiled =
                  Just $
                    MkCompiledValidator
                      { compiledValidatorCode = "59015c0101003232323232322322232232253330093232323232533300e3370e900118079baa001132323253323301230013013375400c264646464a66603260360042646464a666032601060346ea80244c8c94ccc06c0644cdc399b803370066e00cdc019b80337000026eb4c07cc074dd50079b8d375c603e6040603a6ea803c010dc680180b1b8d014481e0528299980d1804980d9baa00113232337006eb4c080008dc68009bae301f3020001301c37540022646466e00dd698100011b8d001375c603e604000260386ea8004c074c06cdd50048b1bae301c301d002375a603600260306ea804c58dd7180c800980c8011bad30170013014375400c6e1d200016301430150023013001301037540022646464c6eb8c050c054008dd6980980098081baa00b3011301200230100013010002300e001300b375400229309b2b1bae001375a00266e1d200230033754002ae6955ceaab9e5573eae855d11"
                      , compiledValidatorHash = "8365f50a4b02c51505e73ce2aef01370cd34afe3bbe68a157c5f7301"
                      }
              }
          ]
    , contractDefinitions =
        Map.fromList
          [
            ( mkDefinitionId "Bool"
            , SchemaAnyOf $ NE.fromList [SchemaConstructor (emptySchemaInfo {title = Just "False"}) (MkConstructorSchema 0 []), SchemaConstructor (emptySchemaInfo {title = Just "True"}) (MkConstructorSchema 1 [])]
            )
          ,
            ( mkDefinitionId "ByteArray"
            , SchemaBytes emptySchemaInfo emptyBytesSchema
            )
          ,
            ( mkDefinitionId "Int"
            , SchemaInteger emptySchemaInfo emptyIntegerSchema
            )
          ,
            ( mkDefinitionId "baz/MyDatum"
            , SchemaAnyOf $
                NE.fromList
                  [ SchemaConstructor (emptySchemaInfo {title = Just "DatumA"}) (MkConstructorSchema 0 [SchemaDefinitionRef (mkDefinitionId "#/definitions/Int"), SchemaDefinitionRef (mkDefinitionId "#/definitions/ByteArray")])
                  , SchemaConstructor (emptySchemaInfo {title = Just "DatumB"}) (MkConstructorSchema 1 [SchemaDefinitionRef (mkDefinitionId "#/definitions/Int"), SchemaDefinitionRef (mkDefinitionId "#/definitions/ByteArray")])
                  ]
            )
          ,
            ( mkDefinitionId "baz/MyRedeemer"
            , SchemaAnyOf $
                NE.fromList
                  [ SchemaConstructor (emptySchemaInfo {title = Just "MyRedeemer"}) (MkConstructorSchema 0 [SchemaDefinitionRef (mkDefinitionId "#/definitions/Int"), SchemaDefinitionRef (mkDefinitionId "#/definitions/ByteArray")])
                  ]
            )
          ,
            ( mkDefinitionId "baz/ParamConstr"
            , SchemaAnyOf $
                NE.fromList
                  [ SchemaConstructor (emptySchemaInfo {title = Just "ParamConstr"}) (MkConstructorSchema 0 [SchemaDefinitionRef (mkDefinitionId "#/definitions/Int"), SchemaDefinitionRef (mkDefinitionId "#/definitions/ByteArray")])
                  ]
            )
          ]
    }

$(makeBPTypes "tests/aiken/bar/plutus.json")

$(uponBPTypes "tests/aiken/bar/plutus.json")

blueprintTests :: TestTree
blueprintTests =
  testGroup
    "Blueprint"
    [ testCase "parse-and-match-simple-blueprint" $ testParseResult (@?= simpleBlueprint) (testsDir </> "aiken/bar/plutus.json")
    , testCase "parse-complex-blueprint-and-match-round-trip" $
        let fp = mockBlueprintsDir </> "complex-blueprint.json"
         in testParseResult
              ( \bp -> do
                  let fp' = mockBlueprintsDir </> "complex-blueprint-rt.json"
                  writeBlueprint fp' bp
                  aContents <- readFile fp
                  bContents <- readFile fp'
                  aContents @=? bContents
              )
              fp
    , testCase "work-with-blueprint-th" $
        let
          val = scriptFromBPSerialisedScript $ applyParamsToBPValidator_baz_baz_spend BPBool0False (BPbaz_ParamConstr0ParamConstr 23 mempty) 23 mempty
          valHash = hashScript val
         in
          writeScript "tests/aiken/bar/plutus-compiled-2" val
            >> print valHash
    , testCase "FIXME: delete me" $
        let
          pA = Plutus.Constr 0 []
          pB = Plutus.Constr 0 [Plutus.I 23, Plutus.B mempty]
          pC = Plutus.I 23
          pD = Plutus.B mempty
          toHex = BS16.encode . LBS.toStrict . CBOR.serialise
         in
          putStrLn $ "pA: " <> show (toHex pA) <> ", pB: " <> show (toHex pB) <> ", pC: " <> show (toHex pC) <> ", pD: " <> show (toHex pD)
    ]

testParseResult :: (ContractBlueprint -> Assertion) -> FilePath -> IO ()
testParseResult expectation filePath =
  readJSON filePath >>= expectation

testsDir :: FilePath
testsDir = "tests"

mockBlueprintsDir :: FilePath
mockBlueprintsDir = testsDir </> "mock-blueprints"
