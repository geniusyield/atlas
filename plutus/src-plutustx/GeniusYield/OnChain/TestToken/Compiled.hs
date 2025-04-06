{- |
Module      : GeniusYield.OnChain.TestToken.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.TestToken.Compiled (
  originalParameterisedTestTokenPolicy,
  originalParameterisedTestTokenPolicyScript,
  writeTestTokenPolicy,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.TestToken
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V2 qualified
import PlutusTx qualified
import PlutusTx.Blueprint

testTokenPolicyBP :: ContractBlueprint
testTokenPolicyBP =
  MkContractBlueprint
    { contractId = Just "test-token-policy"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Test Token Policy"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "Test token policy"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @PlutusTx.BuiltinData
                  , argumentPurpose = Set.fromList [Mint]
                  , argumentDescription = Nothing
                  }
            , validatorParameters =
                [ MkParameterBlueprint
                    { parameterTitle = Just "count"
                    , parameterSchema = definitionRef @Integer
                    , parameterPurpose = Set.singleton Mint
                    , parameterDescription = Nothing
                    }
                , MkParameterBlueprint
                    { parameterTitle = Just "token-name"
                    , parameterSchema = definitionRef @PlutusLedgerApi.V2.TokenName
                    , parameterPurpose = Set.singleton Mint
                    , parameterDescription = Nothing
                    }
                , MkParameterBlueprint
                    { parameterTitle = Just "outref"
                    , parameterSchema = definitionRef @PlutusLedgerApi.V2.TxOutRef
                    , parameterPurpose = Set.singleton Mint
                    , parameterDescription = Nothing
                    }
                ]
            , validatorDescription = Nothing
            , validatorDatum = Nothing
            , validatorCompiled = Just $ compiledValidator PlutusV2 originalParameterisedTestTokenPolicyScript
            }
    , contractDefinitions = deriveDefinitions @'[Integer, PlutusLedgerApi.V2.TokenName, PlutusLedgerApi.V2.TxOutRef, PlutusTx.BuiltinData]
    }

writeTestTokenPolicy :: FilePath -> IO ()
writeTestTokenPolicy fp = writeBlueprint fp testTokenPolicyBP

originalParameterisedTestTokenPolicyScript :: ByteString
originalParameterisedTestTokenPolicyScript = serialiseCompiledCode originalParameterisedTestTokenPolicy & fromShort

originalParameterisedTestTokenPolicy :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalParameterisedTestTokenPolicy = $$(PlutusTx.compile [||mkTestTokenPolicy'||])
