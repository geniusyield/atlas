{- |
Module      : GeniusYield.OnChain.Examples.GuessRefInputDatum.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.GuessRefInputDatum.Compiled (
  guessRefInputDatumValidator,
  writeGuessRefInputDatumValidator,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.GuessRefInputDatum
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx qualified
import PlutusTx.Blueprint

guessRefInputDatumValidatorBP :: ContractBlueprint
guessRefInputDatumValidatorBP =
  MkContractBlueprint
    { contractId = Just "guess-ref-input-datum"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "guess-ref-input-datum"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "guess-ref-input-datum"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @Guess
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
            , validatorCompiled = Just $ compiledValidator PlutusV2 guessRefInputDatumValidatorScript
            }
    , contractDefinitions = deriveDefinitions @'[Guess, PlutusTx.BuiltinData]
    }
 where
  commonPurp = Set.singleton Spend

writeGuessRefInputDatumValidator :: FilePath -> IO ()
writeGuessRefInputDatumValidator fp = writeBlueprint fp guessRefInputDatumValidatorBP

guessRefInputDatumValidatorScript :: ByteString
guessRefInputDatumValidatorScript = serialiseCompiledCode guessRefInputDatumValidator & fromShort

guessRefInputDatumValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
guessRefInputDatumValidator = $$(PlutusTx.compile [||mkGuessRefInputDatumValidator||])
