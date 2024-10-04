module GeniusYield.Test.Blueprint (
  blueprintTests,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types.Blueprint
import GeniusYield.Types.PlutusVersion (PlutusVersion (..))
import System.FilePath
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
    ]

testParseResult :: (ContractBlueprint -> Assertion) -> FilePath -> IO ()
testParseResult expectation filePath =
  readJSON filePath >>= expectation

testsDir :: FilePath
testsDir = "tests"

mockBlueprintsDir :: FilePath
mockBlueprintsDir = testsDir </> "mock-blueprints"
