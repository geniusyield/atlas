module GeniusYield.Test.Blueprint (
  blueprintTests,
) where

import System.FilePath

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GeniusYield.ReadJSON (readJSON)
import GeniusYield.Types.Blueprint
import GeniusYield.Types.PlutusVersion (PlutusVersion (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

simpleBlueprint :: ContractBlueprint
simpleBlueprint =
  MkContractBlueprint
    { contractId = Just "test"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "blueprint/test"
          , preambleDescription = Just "Aiken contracts for project 'blueprint/test'"
          , preambleVersion = "0.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Just "Apache-2.0"
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "always_true.spend"
              , validatorDescription = Nothing
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "_redeemer"
                    , argumentDescription = Nothing
                    , argumentPurpose = mempty
                    , argumentSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                    }
              , validatorDatum =
                  Just $
                    MkArgumentBlueprint
                      { argumentTitle = Just "_datum"
                      , argumentDescription = Nothing
                      , argumentPurpose = mempty
                      , argumentSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/List$Int"
                      }
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "_param1"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                      }
                  , MkParameterBlueprint
                      { parameterTitle = Just "_param2"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/ByteArray"
                      }
                  ]
              , validatorCompiled =
                  Just $
                    MkCompiledValidator
                      { compiledValidatorCode = "5848010000323232322322322322322533300a4a229309b2b1bad0013233001001375800444a6660120022930991980180198060011bad300a001375c0026eb40055cd2ab9f5742ae881"
                      , compiledValidatorHash = "d0cf08dca31123692a4b93c9d063eeff8ae5bc71282544bcdbc296d5"
                      }
              }
          , MkValidatorBlueprint
              { validatorTitle = "nested/sometimes_true.spend"
              , validatorDescription = Nothing
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "redeemer"
                    , argumentDescription = Nothing
                    , argumentPurpose = mempty
                    , argumentSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                    }
              , validatorDatum = Nothing
              , validatorParameters =
                  [ MkParameterBlueprint
                      { parameterTitle = Just "param"
                      , parameterDescription = Nothing
                      , parameterPurpose = mempty
                      , parameterSchema = SchemaDefinitionRef $ mkDefinitionId "#/definitions/Int"
                      }
                  ]
              , validatorCompiled =
                  Just $
                    MkCompiledValidator
                      { compiledValidatorCode = "581e01000032232232253330063371000800429309b2b1bad001375a002ae681"
                      , compiledValidatorHash = "ed3f8f41ef1cd903d9ae4304191ab71c97422189df37d70174d73b7d"
                      }
              }
          ]
    , contractDefinitions = Map.fromList [(mkDefinitionId "Int", SchemaInteger emptySchemaInfo emptyIntegerSchema), (mkDefinitionId "List$Int", SchemaList emptySchemaInfo (MkListSchema (ListItemSchemaSchema $ SchemaDefinitionRef (mkDefinitionId "#/definitions/Int")) Nothing Nothing Nothing)), (mkDefinitionId "ByteArray", SchemaBytes emptySchemaInfo emptyBytesSchema)]
    }

-- | These tests check that we can parse configs
blueprintTests :: TestTree
blueprintTests =
  testGroup
    "Blueprint"
    [ testCase "parse-and-match-simple-blueprint" $ testParseResult (@?= simpleBlueprint) "simple-blueprint.json"
    ]

testParseResult :: (ContractBlueprint -> Assertion) -> FilePath -> IO ()
testParseResult expectation filePath =
  readJSON (mockBlueprintsDir </> filePath) >>= expectation

mockBlueprintsDir :: FilePath
mockBlueprintsDir = "tests/mock-blueprints"