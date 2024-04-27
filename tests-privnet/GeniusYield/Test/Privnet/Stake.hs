module GeniusYield.Test.Privnet.Stake (
    tests,
) where

import           Data.Foldable                  (for_)
import           Data.Maybe                     (fromJust)
import qualified Data.Set                       as Set
import           GeniusYield.Imports
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.Transaction        (GYCoinSelectionStrategy (..))
import           GeniusYield.TxBuilder          (GYTxQueryMonad (stakeAddressInfo),
                                                 mustHaveCertificate,
                                                 mustHaveWithdrawal)
import           GeniusYield.Types
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (assertBool, testCaseSteps)

-- This will check if we are able to register a stake credential without it's signature.
registerStakeCredentialSteps :: GYCoinSelectionStrategy -> (String -> IO ()) -> Ctx -> IO User
registerStakeCredentialSteps strat info ctx = do
  newUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 1_000_000_000) (CreateUserConfig {cucGenerateCollateral = False, cucGenerateStakeKey = True})
  pp <- ctxGetParams ctx & gyGetProtocolParameters'
  info $ "-- Protocol parameters --\n" <> show pp <> "\n-- x --\n"
  txBodyReg <- ctxRunIWithStrategy strat ctx newUser $ do
    return $ mustHaveCertificate (mkStakeAddressRegistrationCertificate (userStakeCredential newUser))
  info $ "-- Registration tx body --\n" <> show txBodyReg <> "\n-- x --\n"
  void $ submitTx ctx newUser txBodyReg
  pure newUser

delegateStakeCredentialSteps :: GYCoinSelectionStrategy -> User -> GYStakePoolId -> (String -> IO ()) -> Ctx -> IO ()
delegateStakeCredentialSteps strat user@User{..} spId info ctx = do
  txBodyDel <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveCertificate (mkStakeAddressPoolDelegationCertificate (userStakePkh user & fromJust & GYStakeCredentialByKey) spId GYTxCertWitnessKey)
  info $ "-- Delegation tx body --\n" <> show txBodyDel <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyDel [GYSomeSigningKey userPaymentSKey, userStakeSKey & fromJust & GYSomeSigningKey]

deregisterStakeCredentialSteps :: GYCoinSelectionStrategy -> User -> (String -> IO ()) -> Ctx -> IO ()
deregisterStakeCredentialSteps strat user@User{..} info ctx = do
  txBodyDereg <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveCertificate (mkStakeAddressDeregistrationCertificate (userStakePkh user & fromJust & GYStakeCredentialByKey) GYTxCertWitnessKey)
  info $ "-- Deregistration tx body --\n" <> show txBodyDereg <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyDereg [GYSomeSigningKey userPaymentSKey, userStakeSKey & fromJust & GYSomeSigningKey]

userStakeAddress :: User -> GYStakeAddress
userStakeAddress user =  userStakeCredential user & stakeAddressFromCredential GYPrivnet

userStakeCredential :: User -> GYStakeCredential
userStakeCredential user = userStakePkh user & fromJust & GYStakeCredentialByKey

withdrawRewardsSteps :: GYCoinSelectionStrategy -> User -> Natural -> (String -> IO ()) -> Ctx -> IO ()
withdrawRewardsSteps strat user@User{..} rewards info ctx = do
  txBodyWithdraw <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveWithdrawal (GYTxWdrl (userStakeAddress user) rewards GYTxWdrlWitnessKey)
  info $ "-- Withdrawal tx body --\n" <> show txBodyWithdraw <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyWithdraw [GYSomeSigningKey userPaymentSKey, userStakeSKey & fromJust & GYSomeSigningKey]

tests :: IO Setup -> TestTree
tests setup = testGroup "stake"
  [ testCaseSteps "exercising stake credential registration, delegation, rewards claiming & de-registration" $ \info -> withSetup setup info $ \ctx -> do
    for_ [GYRandomImproveMultiAsset, GYLegacy] $ \strat -> do
      newUser <- registerStakeCredentialSteps strat info ctx
      sps <- ctx & ctxGetParams & gyGetStakePools'
      info $ "Total stake pools: " <> show sps <> "\n"
      let spId = Set.findMin sps & stakePoolIdFromApi
      info $ "Stake pool id: " <> show spId <> "\n"
      delegateStakeCredentialSteps strat newUser spId info ctx
      GYStakeAddressInfo {..} <- ctxRunC ctx newUser $ stakeAddressInfo (userStakeAddress newUser)
      assertBool "Delegation failed" $ gyStakeAddressInfoDelegatedPool == Just spId
      withdrawRewardsSteps strat newUser gyStakeAddressInfoAvailableRewards info ctx
      deregisterStakeCredentialSteps strat newUser info ctx
  ]
