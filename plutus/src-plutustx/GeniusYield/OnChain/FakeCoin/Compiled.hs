{- |
Module      : GeniusYield.OnChain.FakeCoin.Compiled
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.FakeCoin.Compiled (
  originalParameterisedFakeCoin,
  writeFakeCoin,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.FakeCoin
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V2 qualified
import PlutusTx qualified
import PlutusTx.Blueprint

fakeCoinBP :: ContractBlueprint
fakeCoinBP =
  MkContractBlueprint
    { contractId = Just "fake-coin"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "fake-coin"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "fake-coin"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @()
                  , argumentPurpose = commonPurp
                  , argumentDescription = Nothing
                  }
            , validatorParameters =
                [ MkParameterBlueprint
                    { parameterTitle = Just "token-name"
                    , parameterSchema = definitionRef @PlutusLedgerApi.V2.TokenName
                    , parameterPurpose = commonPurp
                    , parameterDescription = Nothing
                    }
                ]
            , validatorDescription = Nothing
            , validatorDatum = Nothing
            , validatorCompiled = Just $ compiledValidator PlutusV2 originalParameterisedFakeCoinScript
            }
    , contractDefinitions = deriveDefinitions @'[PlutusLedgerApi.V2.TokenName, PlutusTx.BuiltinData, ()]
    }
 where
  commonPurp = Set.singleton Mint

writeFakeCoin :: FilePath -> IO ()
writeFakeCoin fp = writeBlueprint fp fakeCoinBP

originalParameterisedFakeCoinScript :: ByteString
originalParameterisedFakeCoinScript = serialiseCompiledCode originalParameterisedFakeCoin & fromShort

originalParameterisedFakeCoin ::
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalParameterisedFakeCoin =
  $$(PlutusTx.compile [||fakeMintingPolicyUntypedContract||])
