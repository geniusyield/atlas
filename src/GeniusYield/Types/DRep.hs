{-|
Module      : GeniusYield.Types.DRep
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.DRep (
  GYDRep,
  drepToLedger,
  drepFromLedger,
) where

import qualified Cardano.Api.Ledger                  as Ledger
import           GeniusYield.Types.PubKeyHash        (GYPubKeyHash,
                                                      pubKeyHashFromLedger,
                                                      pubKeyHashToLedger)
import           GeniusYield.Types.Script.ScriptHash (GYScriptHash,
                                                      scriptHashFromLedger,
                                                      scriptHashToLedger)

data GYDRep
  = GYDRepKeyHash !GYPubKeyHash
  | GYDRepScriptHash !GYScriptHash
  | GYDRepAlwaysAbstain
  | GYDRepAlwaysNoConfidence
  deriving stock (Show, Eq, Ord)

drepToLedger :: GYDRep -> Ledger.DRep Ledger.StandardCrypto
drepToLedger drep = case drep of
  GYDRepKeyHash kh -> Ledger.DRepCredential $ Ledger.KeyHashObj $ pubKeyHashToLedger kh
  GYDRepScriptHash sh -> Ledger.DRepCredential $ Ledger.ScriptHashObj $ scriptHashToLedger sh
  GYDRepAlwaysAbstain -> Ledger.DRepAlwaysAbstain
  GYDRepAlwaysNoConfidence -> Ledger.DRepAlwaysNoConfidence

drepFromLedger :: Ledger.DRep Ledger.StandardCrypto -> GYDRep
drepFromLedger drep = case drep of
  Ledger.DRepCredential s -> case s of
    Ledger.KeyHashObj kh    -> GYDRepKeyHash $ pubKeyHashFromLedger kh
    Ledger.ScriptHashObj sh -> GYDRepScriptHash $ scriptHashFromLedger sh
  Ledger.DRepAlwaysAbstain -> GYDRepAlwaysAbstain
  Ledger.DRepAlwaysNoConfidence -> GYDRepAlwaysNoConfidence
