{- |
Module      : GeniusYield.OnChain.Examples.ReadOracle.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.Examples.ReadOracle.Compiled (
  readOracleValidator,
  writeReadOracleValidator,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.Examples.ReadOracle
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx qualified
import PlutusTx.Blueprint

readOracleValidatorBP :: ContractBlueprint
readOracleValidatorBP =
  MkContractBlueprint
    { contractId = Just "read-oracle"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "read-oracle"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "read-oracle"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @PlutusTx.BuiltinData
                  , argumentPurpose = commonPurp
                  , argumentDescription = Nothing
                  }
            , validatorParameters =
                []
            , validatorDescription = Nothing
            , validatorDatum =
                Just $
                  MkArgumentBlueprint
                    { argumentTitle = Nothing
                    , argumentSchema = definitionRef @PlutusTx.BuiltinData
                    , argumentPurpose = commonPurp
                    , argumentDescription = Nothing
                    }
            , validatorCompiled = Just $ compiledValidator PlutusV2 readOracleValidatorScript
            }
    , contractDefinitions = deriveDefinitions @'[PlutusTx.BuiltinData]
    }
 where
  commonPurp = Set.singleton Spend

writeReadOracleValidator :: FilePath -> IO ()
writeReadOracleValidator fp = writeBlueprint fp readOracleValidatorBP

readOracleValidatorScript :: ByteString
readOracleValidatorScript = serialiseCompiledCode readOracleValidator & fromShort

readOracleValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
readOracleValidator = $$(PlutusTx.compile [||mkReadOracleValidator||])
