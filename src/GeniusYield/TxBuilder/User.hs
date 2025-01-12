{-# LANGUAGE PatternSynonyms #-}

module GeniusYield.TxBuilder.User (
  User (..),
  UserCollateral (..),
  pattern User',
  userPkh,
  userPaymentPkh,
  userStakePkh,
  userVKey,
  userPaymentVKey,
  userPaymentSKey',
  userStakeSKey',
  userStakeVKey,
  userCollateralDumb,
  userAddresses',
  userAddr,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import GeniusYield.Imports
import GeniusYield.Types.Address (GYAddress)
import GeniusYield.Types.Key
import GeniusYield.Types.PaymentKeyHash (GYPaymentKeyHash)
import GeniusYield.Types.PubKeyHash (AsPubKeyHash (toPubKeyHash), GYPubKeyHash)
import GeniusYield.Types.StakeKeyHash (GYStakeKeyHash)
import GeniusYield.Types.TxOutRef (GYTxOutRef)

-- | Information on a the designated collateral to use.
data UserCollateral = UserCollateral
  { userCollateralRef :: GYTxOutRef
  , userCollateralCheck :: Bool
  -- ^ If `False` then the given `GYTxOutRef` will be used and reserved as collateral.
  -- If `True`, then collateral will only be used and reserved, if value in the given UTxO is exactly 5 ada.
  }
  deriving stock (Eq, Show)

-- | Note: When signing using 'ToShelleyWitnessSigningKey' instance, it only uses the payment signing key.
data User = User
  { userPaymentSKey :: !GYPaymentSigningKey
  , userStakeSKey :: !(Maybe GYStakeSigningKey)
  , userAddresses :: !(NonEmpty GYAddress)
  , userChangeAddress :: !GYAddress
  , userCollateral :: Maybe UserCollateral
  }
  deriving stock (Eq, Show)

instance Ord User where
  compare = compare `on` userChangeAddress

-- | This only takes the payment signing key, not the stake key.
instance ToShelleyWitnessSigningKey User where
  toShelleyWitnessSigningKey = toShelleyWitnessSigningKey . userPaymentSKey

{-# DEPRECATED userVKey "Use userPaymentVKey." #-}
userVKey :: User -> GYPaymentVerificationKey
userVKey = paymentVerificationKey . userPaymentSKey

userPaymentVKey :: User -> GYPaymentVerificationKey
userPaymentVKey = userVKey

userStakeVKey :: User -> Maybe GYStakeVerificationKey
userStakeVKey = fmap stakeVerificationKey . userStakeSKey

userPkh :: User -> GYPubKeyHash
userPkh = toPubKeyHash . paymentKeyHash . paymentVerificationKey . userPaymentSKey

userPaymentPkh :: User -> GYPaymentKeyHash
userPaymentPkh = paymentKeyHash . paymentVerificationKey . userPaymentSKey

userStakePkh :: User -> Maybe GYStakeKeyHash
userStakePkh = fmap (stakeKeyHash . stakeVerificationKey) . userStakeSKey

userCollateralDumb :: User -> Maybe (GYTxOutRef, Bool)
userCollateralDumb User {userCollateral} =
  (\UserCollateral {userCollateralRef, userCollateralCheck} -> (userCollateralRef, userCollateralCheck)) <$> userCollateral

userAddresses' :: User -> [GYAddress]
userAddresses' = NE.toList . userAddresses

pattern User' :: GYPaymentSigningKey -> Maybe GYStakeSigningKey -> GYAddress -> User
pattern User' {userPaymentSKey', userStakeSKey', userAddr} <-
  User
    { userPaymentSKey = userPaymentSKey'
    , userStakeSKey = userStakeSKey'
    , userAddresses = (NE.head -> userAddr)
    }
  where
    User' userPaymentSKey' userStakeSKey' userAddr =
      User
        { userPaymentSKey = userPaymentSKey'
        , userStakeSKey = userStakeSKey'
        , userAddresses = NE.singleton userAddr
        , userChangeAddress = userAddr
        , userCollateral = Nothing
        }
{-# COMPLETE User' #-}
