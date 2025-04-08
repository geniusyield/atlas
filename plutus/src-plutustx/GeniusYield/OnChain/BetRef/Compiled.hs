{- |
Module      : GeniusYield.OnChain.Examples.BetRef.Compiled
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.OnChain.BetRef.Compiled (
  betRefValidator,
  writeBetRefValidator,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function ((&))
import Data.Set qualified as Set
import GeniusYield.OnChain.BetRef
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx qualified
import PlutusTx.Blueprint

betRefValidatorBP :: ContractBlueprint
betRefValidatorBP =
  MkContractBlueprint
    { contractId = Just "bet-ref"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "bet-ref"
          , preambleDescription = Nothing
          , preambleVersion = "1.0.0"
          , preamblePlutusVersion = PlutusV2
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.singleton
          MkValidatorBlueprint
            { validatorTitle = "bet-ref"
            , validatorRedeemer =
                MkArgumentBlueprint
                  { argumentTitle = Nothing
                  , argumentSchema = definitionRef @BetRefAction
                  , argumentPurpose = commonPurp
                  , argumentDescription = Nothing
                  }
            , validatorParameters =
                [ MkParameterBlueprint
                    { parameterTitle = Just "BetRefParams"
                    , parameterSchema = definitionRef @BetRefParams
                    , parameterPurpose = commonPurp
                    , parameterDescription = Nothing
                    }
                ]
            , validatorDescription = Nothing
            , validatorDatum =
                Just $
                  MkArgumentBlueprint
                    { argumentTitle = Nothing
                    , argumentSchema = definitionRef @BetRefDatum
                    , argumentPurpose = commonPurp
                    , argumentDescription = Nothing
                    }
            , validatorCompiled = Just $ compiledValidator PlutusV2 betRefValidatorScript
            }
    , contractDefinitions = deriveDefinitions @'[BetRefParams, BetRefDatum, BetRefAction]
    }
 where
  commonPurp = Set.singleton Spend

writeBetRefValidator :: FilePath -> IO ()
writeBetRefValidator fp = writeBlueprint fp betRefValidatorBP

betRefValidatorScript :: ByteString
betRefValidatorScript = serialiseCompiledCode betRefValidator & fromShort

betRefValidator :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
betRefValidator = $$(PlutusTx.compile [||mkBetRefValidator||])
