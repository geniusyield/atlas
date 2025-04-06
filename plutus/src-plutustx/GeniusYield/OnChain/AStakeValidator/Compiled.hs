{- |
Module      : GeniusYield.OnChain.AStakeValidator.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.AStakeValidator.Compiled (
  originalParameterisedAStakeValidator,
  writeAStakeValidator,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.AStakeValidator
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V2 qualified
import PlutusTx qualified
import PlutusTx.Blueprint

aStakeValidatorBP :: ContractBlueprint
aStakeValidatorBP =
  MkContractBlueprint
    { contractId = Just "a-stake-validator"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "simple stake validator"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "simple stake validator"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @PlutusTx.BuiltinData
                  , argumentPurpose = commonPurp
                  , argumentDescription = Nothing
                  }
            , validatorParameters =
                [ MkParameterBlueprint
                    { parameterTitle = Just "address"
                    , parameterSchema = definitionRef @PlutusLedgerApi.V2.Address
                    , parameterPurpose = commonPurp
                    , parameterDescription = Nothing
                    }
                ]
            , validatorDescription = Nothing
            , validatorDatum = Nothing
            , validatorCompiled = Just $ compiledValidator PlutusV2 originalParameterisedAStakeValidatorScript
            }
    , contractDefinitions = deriveDefinitions @'[PlutusLedgerApi.V2.Address, PlutusTx.BuiltinData]
    }
 where
  commonPurp = Set.fromList [Withdraw, Publish]

writeAStakeValidator :: FilePath -> IO ()
writeAStakeValidator fp = writeBlueprint fp aStakeValidatorBP

originalParameterisedAStakeValidatorScript :: ByteString
originalParameterisedAStakeValidatorScript = serialiseCompiledCode originalParameterisedAStakeValidator & fromShort

originalParameterisedAStakeValidator ::
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
originalParameterisedAStakeValidator =
  $$(PlutusTx.compile [||mkAStakeValidator||])
