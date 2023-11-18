{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OnChain.Compiled
    ( betRefValidator
    , BetRefParams (..)
    , OracleAnswerDatum (..)
    , BetRefDatum (..)
    , BetRefAction (..)
    ) where

import qualified PlutusTx
import PlutusCore.Version (plcVersion100)


import           OnChain.BetRef

betRefValidator :: BetRefParams -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
betRefValidator betRefParams  =
  $$(PlutusTx.compile [|| mkBetRefValidator ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 betRefParams
