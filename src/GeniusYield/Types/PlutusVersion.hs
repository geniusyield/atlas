{-|
Module      : GeniusYield.Types.PlutusVersion
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.PlutusVersion (
    -- * Plutus version
    PlutusVersion (..),
    SingPlutusVersion (..),
    SingPlutusVersionI (..),
    PlutusVersionToApi,
    singPlutusVersionToApi,
    VersionIsGreaterOrEqual,
) where

import           GeniusYield.Imports

import Data.GADT.Compare
import GHC.TypeLits

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S

data PlutusVersion
    = 'PlutusV1
    | 'PlutusV2
  deriving (Eq, Show)

data SingPlutusVersion (v :: PlutusVersion) where
    SingPlutusV1 :: SingPlutusVersion 'PlutusV1
    SingPlutusV2 :: SingPlutusVersion 'PlutusV2

class    SingPlutusVersionI (v :: PlutusVersion) where singPlutusVersion :: SingPlutusVersion v
instance SingPlutusVersionI 'PlutusV1             where singPlutusVersion = SingPlutusV1
instance SingPlutusVersionI 'PlutusV2             where singPlutusVersion = SingPlutusV2

instance GEq SingPlutusVersion where
    geq SingPlutusV1 SingPlutusV1 = Just Refl
    geq SingPlutusV1 SingPlutusV2 = Nothing
    geq SingPlutusV2 SingPlutusV1 = Nothing
    geq SingPlutusV2 SingPlutusV2 = Just Refl

instance GCompare SingPlutusVersion where
    gcompare SingPlutusV1 SingPlutusV1 = GEQ
    gcompare SingPlutusV1 SingPlutusV2 = GLT
    gcompare SingPlutusV2 SingPlutusV1 = GGT
    gcompare SingPlutusV2 SingPlutusV2 = GEQ

type family PlutusVersionToApi (v :: PlutusVersion) :: Type where
    PlutusVersionToApi 'PlutusV1 = Api.PlutusScriptV1
    PlutusVersionToApi 'PlutusV2 = Api.PlutusScriptV2

singPlutusVersionToApi :: SingPlutusVersion v -> Api.S.PlutusScriptVersion (PlutusVersionToApi v)
singPlutusVersionToApi SingPlutusV1 = Api.PlutusScriptV1
singPlutusVersionToApi SingPlutusV2 = Api.PlutusScriptV2

-- | Constraint that @v >= u@.
--
-- Used to allow using V2 transaction features only in transactions with V2 inputs.
class VersionIsGreaterOrEqual (v :: PlutusVersion) (u :: PlutusVersion)

-- | Any version is greater or equal to 'PlutusV1'
instance VersionIsGreaterOrEqual 'PlutusV1 'PlutusV1

-- | Any version is greater or equal to 'PlutusV1'
instance VersionIsGreaterOrEqual 'PlutusV2 'PlutusV1

-- | Only 'PlutusV2' is greater or equal to itself at the moment.
instance VersionIsGreaterOrEqual 'PlutusV2 'PlutusV2

-- | Explicitly ruled out instance.
instance TypeError ('Text "V1 is not >= V2") => VersionIsGreaterOrEqual 'PlutusV1 'PlutusV2
