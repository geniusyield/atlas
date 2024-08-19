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
    fromSingPlutusVersion,
    PlutusVersionToApi,
    singPlutusVersionToApi,
    VersionIsGreaterOrEqual,
    VersionIsGreater,
    CmpPlutusVersion,
) where

import           GeniusYield.Imports

import           Data.GADT.Compare
import           GHC.TypeLits

import qualified Cardano.Api         as Api
import qualified Cardano.Api.Shelley as Api.S
import           Data.Type.Bool      (If)
import           Data.Type.Equality  (type (==))

data PlutusVersion
    = PlutusV1
    | PlutusV2
    | PlutusV3
  deriving (Eq, Ord, Show)

data SingPlutusVersion (v :: PlutusVersion) where
    SingPlutusV1 :: SingPlutusVersion 'PlutusV1
    SingPlutusV2 :: SingPlutusVersion 'PlutusV2
    SingPlutusV3 :: SingPlutusVersion 'PlutusV3


class    SingPlutusVersionI (v :: PlutusVersion) where singPlutusVersion :: SingPlutusVersion v
instance SingPlutusVersionI 'PlutusV1             where singPlutusVersion = SingPlutusV1
instance SingPlutusVersionI 'PlutusV2             where singPlutusVersion = SingPlutusV2
instance SingPlutusVersionI 'PlutusV3             where singPlutusVersion = SingPlutusV3

instance GEq SingPlutusVersion where
    geq SingPlutusV1 SingPlutusV1 = Just Refl
    geq SingPlutusV2 SingPlutusV2 = Just Refl
    geq SingPlutusV3 SingPlutusV3 = Just Refl
    geq _ _                       = Nothing

instance GCompare SingPlutusVersion where
    gcompare SingPlutusV1 SingPlutusV1 = GEQ
    gcompare SingPlutusV1 _            = GLT
    gcompare SingPlutusV2 SingPlutusV1 = GGT
    gcompare SingPlutusV2 SingPlutusV2 = GEQ
    gcompare SingPlutusV2 _            = GLT
    gcompare SingPlutusV3 SingPlutusV1 = GGT
    gcompare SingPlutusV3 SingPlutusV2 = GGT
    gcompare SingPlutusV3 SingPlutusV3 = GEQ

type family PlutusVersionToApi (v :: PlutusVersion) :: Type where
    PlutusVersionToApi 'PlutusV1 = Api.PlutusScriptV1
    PlutusVersionToApi 'PlutusV2 = Api.PlutusScriptV2
    PlutusVersionToApi 'PlutusV3 = Api.PlutusScriptV3

singPlutusVersionToApi :: SingPlutusVersion v -> Api.S.PlutusScriptVersion (PlutusVersionToApi v)
singPlutusVersionToApi SingPlutusV1 = Api.PlutusScriptV1
singPlutusVersionToApi SingPlutusV2 = Api.PlutusScriptV2
singPlutusVersionToApi SingPlutusV3 = Api.PlutusScriptV3

fromSingPlutusVersion :: SingPlutusVersion v -> PlutusVersion
fromSingPlutusVersion SingPlutusV3 = PlutusV3
fromSingPlutusVersion SingPlutusV2 = PlutusV2
fromSingPlutusVersion SingPlutusV1 = PlutusV1

-- | Type family to compare `PlutusVersion`s
type family CmpPlutusVersion (v :: PlutusVersion) (u :: PlutusVersion) :: Ordering where
  CmpPlutusVersion 'PlutusV1 'PlutusV1 = 'EQ
  CmpPlutusVersion 'PlutusV1 _ = 'LT
  CmpPlutusVersion 'PlutusV2 'PlutusV1 = 'GT
  CmpPlutusVersion 'PlutusV2 'PlutusV2 = 'EQ
  CmpPlutusVersion 'PlutusV2 _ = 'LT
  CmpPlutusVersion 'PlutusV3 'PlutusV1 = 'GT
  CmpPlutusVersion 'PlutusV3 'PlutusV2 = 'GT
  CmpPlutusVersion 'PlutusV3 'PlutusV3 = 'EQ

-- | Constraint that @v >= u@.
--
-- If transaction is making use of V2 features (such as reference inputs) then as these cannot be represented in script context of V1 scripts, we need to ensure that the involved script version is at least V2. Likewise for other versions.
class VersionIsGreaterOrEqual (v :: PlutusVersion) (u :: PlutusVersion)
instance (If ((v `CmpPlutusVersion` u) == 'LT) (TypeError ('Text "Given version " ':<>: ShowType v ':<>: 'Text ", is not greater or equal to " ':<>: ShowType u)) (() :: Constraint)) => VersionIsGreaterOrEqual v u

-- | Constraint that @v > u@.
class VersionIsGreater (v :: PlutusVersion) (u :: PlutusVersion)
instance ((v `CmpPlutusVersion` u) ~ 'GT) => VersionIsGreater v u
