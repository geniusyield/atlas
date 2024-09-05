{- |
Module      : GeniusYield.Types.TxCert.Internal
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxCert.Internal (
  GYTxCert (..),
  GYTxCert' (..),
  finaliseTxCert,
  GYTxCertWitness (..),
  txCertToApi,
) where

import Cardano.Api qualified as Api
import Data.Functor ((<&>))
import GeniusYield.Imports ((&))
import GeniusYield.Types.Certificate
import GeniusYield.Types.Credential (stakeCredentialToApi)
import GeniusYield.Types.Era
import GeniusYield.Types.ProtocolParameters (ApiProtocolParameters)
import GeniusYield.Types.Redeemer
import GeniusYield.Types.Script

{- | A transaction certificate.

The parameter @v@ indicates the minimum version of scripts allowed to witness certificates
in the transaction.

Note that witness is not required for registering a stake address and for moving instantaneous rewards. Thus, we provide helper utilities to interact with `GYTxCert` sanely and thus keep it's representation opaque.
-}
data GYTxCert v = GYTxCert
  { gyTxCertCertificate :: !GYCertificatePreBuild
  , gyTxCertWitness :: !(Maybe (GYTxCertWitness v))
  }
  deriving (Eq, Show)

data GYTxCert' v = GYTxCert'
  { gyTxCertCertificate' :: !GYCertificate
  , gyTxCertWitness' :: !(Maybe (GYTxCertWitness v))
  }
  deriving (Eq, Show)

finaliseTxCert :: ApiProtocolParameters -> GYTxCert v -> GYTxCert' v
finaliseTxCert pp (GYTxCert cert wit) = GYTxCert' (finaliseCert pp cert) wit

-- | Represents witness type and associated information for a certificate.
data GYTxCertWitness v
  = -- | Key witness.
    GYTxCertWitnessKey
  | -- | Script witness with associated script and redeemer.
    GYTxCertWitnessScript !(GYStakeValScript v) !GYRedeemer
  deriving stock (Eq, Show)

txCertToApi ::
  GYTxCert' v ->
  (Api.Certificate ApiEra, Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake ApiEra))
txCertToApi (GYTxCert' cert wit) = (certificateToApi cert, wit <&> (\wit' -> (certificateToStakeCredential cert & stakeCredentialToApi, f wit')))
  where
    f :: GYTxCertWitness v -> Api.Witness Api.WitCtxStake ApiEra
    f GYTxCertWitnessKey = Api.KeyWitness Api.KeyWitnessForStakeAddr
    f (GYTxCertWitnessScript v r) =
      Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
        gyStakeValScriptWitnessToApiPlutusSW
          v
          (redeemerToApi r)
          (Api.ExecutionUnits 0 0)
