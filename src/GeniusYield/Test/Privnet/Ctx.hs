{- |
Module      : GeniusYield.Test.Privnet.Ctx
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Privnet.Ctx (
  -- * Context
  Ctx (..),
  CtxCommittee (..),
  ctxNetworkId,

  -- * User
  User (..),
  CreateUserConfig (..),
  ctxUsers,
  ctxWallets,
  userPkh,
  userPaymentPkh,
  userStakePkh,
  userVKey,
  userPaymentVKey,
  userStakeVKey,

  -- * Operations
  ctxRunGame,
  ctxRun,
  ctxRunQuery,
  ctxRunBuilder,
  ctxRunBuilderWithCollateral,
  ctxSlotOfCurrentBlock,
  ctxWaitNextBlock,
  ctxWaitUntilSlot,
  ctxProviders,
  ctxSlotConfig,

  -- * Helpers
  newTempUserCtx,
  ctxQueryBalance,
  findOutput,
) where

import Cardano.Api qualified as Api
import Data.Default (Default (..))
import GeniusYield.Imports
import GeniusYield.Providers.Node
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty.HUnit (assertFailure)

-- TODO (simplify-genesis): Remove this once 'newTempUserCtx' has been removed.
data CreateUserConfig
  = CreateUserConfig
  { cucGenerateCollateral :: !Bool
  -- ^ Create collateral output of 5 ada?
  , cucGenerateStakeKey :: !Bool
  -- ^ Create a stake key for the user?
  }

instance Default CreateUserConfig where
  def = CreateUserConfig {cucGenerateCollateral = False, cucGenerateStakeKey = False}

data Ctx = Ctx
  { ctxNetworkInfo :: !GYNetworkInfo
  , ctxInfo :: !Api.LocalNodeConnectInfo
  , -- FIXME: There are now multiple genesis users (since cardano-testnet usage).
    -- TODO (simplify-genesis): Remove these fields (except for funder user(s))
    -- once user creation logic is removed from test setup.
    ctxUserF :: !User
  -- ^ Funder. All other users begin with same status of funds.
  , ctxUser2 :: !User
  , ctxUser3 :: !User
  , ctxUser4 :: !User
  , ctxUser5 :: !User
  , ctxUser6 :: !User
  , ctxUser7 :: !User
  , ctxUser8 :: !User
  , ctxUser9 :: !User
  , ctxGold :: !GYAssetClass
  -- ^ asset used in tests
  , ctxIron :: !GYAssetClass
  -- ^ asset used in tests
  , ctxLog :: !GYLogConfiguration
  , ctxLookupDatum :: !GYLookupDatum
  , ctxAwaitTxConfirmed :: !GYAwaitTx
  , ctxQueryUtxos :: !GYQueryUTxO
  , ctxGetParams :: !GYGetParameters
  , ctxCommittee :: !CtxCommittee
  }

data CtxCommittee = CtxCommittee
  { ctxCommitteeMembers :: !(Map (GYSigningKey 'GYKeyRoleColdCommittee) GYEpochNo)
  -- ^ Committee members with epoch number when each of them expires
  , ctxCommitteeThreshold :: !UnitInterval
  -- ^ Threshold of the committee that is necessary for a successful vote
  }

ctxNetworkId :: Ctx -> GYNetworkId
ctxNetworkId Ctx {ctxNetworkInfo} = GYPrivnet ctxNetworkInfo

-- TODO (simplify-genesis): Remove this once user creation logic is removed from test setup.

{- | List of context sibling users - all of which begin with same balance.
FIXME: Some of these users are actually genesis users.
-}
ctxUsers :: Ctx -> [User]
ctxUsers ctx = ($ ctx) <$> [ctxUser2, ctxUser3, ctxUser4, ctxUser5, ctxUser6, ctxUser7, ctxUser8, ctxUser9]

-- TODO (simplify-genesis): Remove this once user creation logic is removed from test setup.
ctxWallets :: Ctx -> Wallets
ctxWallets Ctx {..} =
  Wallets
    { w1 = ctxUserF
    , w2 = ctxUser2
    , w3 = ctxUser3
    , w4 = ctxUser4
    , w5 = ctxUser5
    , w6 = ctxUser6
    , w7 = ctxUser7
    , w8 = ctxUser8
    , w9 = ctxUser9
    }

-- TODO (simplify-genesis): Remove this. See note 'simplify-genesis'.

-- | Creates a new user with the given balance. Note that the actual balance which this user get's could be more than what is provided to satisfy minimum ada requirement of a UTxO.
newTempUserCtx ::
  Ctx ->
  -- | User which will fund this new user.
  User ->
  -- | Describes balance of new user.
  GYValue ->
  CreateUserConfig ->
  IO User
newTempUserCtx ctx fundUser fundValue CreateUserConfig {..} = do
  newPaymentSKey <- generatePaymentSigningKey
  newStakeSKey <- if cucGenerateStakeKey then Just <$> generateStakeSigningKey else pure Nothing
  let newPaymentVKey = paymentVerificationKey newPaymentSKey
      newStakeVKey = stakeVerificationKey <$> newStakeSKey
      newPaymentKeyHash = paymentKeyHash newPaymentVKey
      newStakeKeyHash = stakeKeyHash <$> newStakeVKey
      newAddr = addressFromCredential (ctxNetworkId ctx) (GYPaymentCredentialByKey newPaymentKeyHash) (GYStakeCredentialByKey <$> newStakeKeyHash)
      (adaInValue, otherValue) = valueSplitAda fundValue

  -- We want this new user to have at least 5 ada if we want to create collateral.
  -- Our balancer would add minimum ada required for other utxo in case of equality
  when (cucGenerateCollateral && adaInValue < collateralLovelace) $ fail "Given value for new user has less than 5 ada"

  ctxRun ctx fundUser $ do
    txBody <-
      buildTxBody $
        if cucGenerateCollateral
          then
            mustHaveOutput (mkGYTxOutNoDatum newAddr (otherValue <> (valueFromLovelace adaInValue `valueMinus` collateralValue)))
              <> mustHaveOutput (mkGYTxOutNoDatum newAddr collateralValue)
          else
            mustHaveOutput (mkGYTxOutNoDatum newAddr fundValue)
    signAndSubmitConfirmed_ txBody

  pure $ User' {userPaymentSKey' = newPaymentSKey, userAddr = newAddr, userStakeSKey' = newStakeSKey}

ctxRunGame :: Ctx -> GYTxGameMonadIO a -> IO a
ctxRunGame ctx = runGYTxGameMonadIO (ctxNetworkId ctx) (ctxProviders ctx)

ctxRun :: Ctx -> User -> GYTxMonadIO a -> IO a
ctxRun ctx User' {..} =
  runGYTxMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    (AGYPaymentSigningKey userPaymentSKey')
    (AGYStakeSigningKey <$> userStakeSKey')
    [userAddr]
    userAddr
    Nothing

ctxRunQuery :: Ctx -> GYTxQueryMonadIO a -> IO a
ctxRunQuery ctx = runGYTxQueryMonadIO (ctxNetworkId ctx) (ctxProviders ctx)

ctxRunBuilder :: Ctx -> User -> GYTxBuilderMonadIO a -> IO a
ctxRunBuilder ctx User' {..} = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr Nothing

-- | Variant of `ctxRun` where caller can also give the UTxO to be used as collateral.
ctxRunBuilderWithCollateral ::
  Ctx ->
  User ->
  -- | Reference to UTxO to be used as collateral.
  GYTxOutRef ->
  -- | To check whether this given collateral UTxO has value of exact 5 ada? If it doesn't have exact 5 ada, it would be ignored.
  Bool ->
  GYTxBuilderMonadIO a ->
  IO a
ctxRunBuilderWithCollateral ctx User' {..} coll toCheck5Ada =
  runGYTxBuilderMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    [userAddr]
    userAddr
    (Just (coll, toCheck5Ada))

ctxSlotOfCurrentBlock :: Ctx -> IO GYSlot
ctxSlotOfCurrentBlock (ctxProviders -> providers) =
  gyGetSlotOfCurrentBlock providers

ctxWaitNextBlock :: Ctx -> IO ()
ctxWaitNextBlock (ctxProviders -> providers) = void $ gyWaitForNextBlock providers

ctxWaitUntilSlot :: Ctx -> GYSlot -> IO ()
ctxWaitUntilSlot (ctxProviders -> providers) slot = void $ gyWaitUntilSlot providers slot

ctxSlotConfig :: Ctx -> IO GYSlotConfig
ctxSlotConfig ctx = ctxRunQuery ctx slotConfig

ctxQueryBalance :: Ctx -> User -> IO GYValue
ctxQueryBalance ctx u = ctxRunQuery ctx $ do
  queryBalance $ userAddr u

ctxProviders :: Ctx -> GYProviders
ctxProviders ctx =
  GYProviders
    { gyLookupDatum = ctxLookupDatum ctx
    , gySubmitTx = nodeSubmitTx (ctxInfo ctx)
    , gyAwaitTxConfirmed = ctxAwaitTxConfirmed ctx
    , gySlotActions = nodeSlotActions (ctxInfo ctx)
    , gyGetParameters = ctxGetParams ctx
    , gyQueryUTxO = ctxQueryUtxos ctx
    , gyLog' = ctxLog ctx
    , gyGetStakeAddressInfo = nodeStakeAddressInfo (ctxInfo ctx)
    , gyGetDRepState = nodeGetDRepState (ctxInfo ctx)
    , gyGetDRepsState = nodeGetDRepsState (ctxInfo ctx)
    , gyGetStakePools = nodeStakePools (ctxInfo ctx)
    }

-- | Function to find for the first locked output in the given `GYTxBody` at the given `GYAddress`.
findOutput :: GYAddress -> GYTxBody -> IO GYTxOutRef
findOutput addr txBody = do
  let utxos = txBodyUTxOs txBody
  maybe (assertFailure "expecting an order in utxos") return $
    findFirst (\utxo -> if utxoAddress utxo == addr then Just (utxoRef utxo) else Nothing) $
      utxosToList utxos
