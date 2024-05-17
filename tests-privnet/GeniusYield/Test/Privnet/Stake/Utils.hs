module GeniusYield.Test.Privnet.Stake.Utils (
  createMangledUser,
  aStakeValidator,
  resolveSigningRequirement,
  resolveStakeCredential,
  resolveStakeAddress,
  registerStakeCredentialSteps,
  delegateStakeCredentialSteps,
  deregisterStakeCredentialSteps,
  withdrawRewardsSteps,
  stakeIntegrationTest,
) where

import           Data.Foldable                                (for_)
import           Data.Maybe                                   (fromJust,
                                                               isNothing)
import qualified Data.Set                                     as Set
import           GeniusYield.Imports
import           GeniusYield.OnChain.AStakeValidator.Compiled (originalAStakeValidator)
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Transaction                      (GYCoinSelectionStrategy (..))
import           GeniusYield.TxBuilder.Class
import           GeniusYield.Types
import           Test.Tasty.HUnit                             (assertBool)

someAddr :: GYAddress
someAddr = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt"

aStakeValidator :: GYStakeValidator 'PlutusV2
aStakeValidator =
    stakeValidatorFromPlutus  @'PlutusV2
        $ originalAStakeValidator (addressToPlutus someAddr)

createMangledUser :: Ctx -> GYStakeCredential -> IO User
createMangledUser ctx stakeCred = do
  newPaymentSKey <- generatePaymentSigningKey
  let newPaymentVKey = paymentVerificationKey newPaymentSKey
      newPaymentKeyHash = paymentKeyHash newPaymentVKey
      newAddr = addressFromCredential GYPrivnet (GYPaymentCredentialByKey newPaymentKeyHash) (Just stakeCred)
      fundUser = ctxUserF ctx
  txBody <- ctxRunI ctx fundUser $
    return $ mustHaveOutput (mkGYTxOutNoDatum newAddr $ valueFromLovelace 1_000_000_000)
  void $ submitTx ctx fundUser txBody
  return $ User {userPaymentSKey = newPaymentSKey, userAddr = newAddr, userStakeSKey = Nothing}

userStakeCredential :: User -> GYStakeCredential
userStakeCredential user = userStakePkh user & fromJust & GYStakeCredentialByKey

resolveStakeCredential :: User -> Maybe GYStakeValidatorHash -> GYStakeCredential
resolveStakeCredential user = maybe (userStakeCredential user) GYStakeCredentialByScript

resolveStakeAddress :: User -> Maybe GYStakeValidatorHash -> GYStakeAddress
resolveStakeAddress user = stakeAddressFromCredential GYPrivnet . resolveStakeCredential user

resolveSigningRequirement :: User -> Maybe GYStakeValidatorHash -> [GYSomeSigningKey]
resolveSigningRequirement User {..} mstakeValHash = GYSomeSigningKey userPaymentSKey : ([userStakeSKey & fromJust & GYSomeSigningKey | isNothing mstakeValHash])

resolveCertWitness :: Bool -> GYTxCertWitness 'PlutusV2
resolveCertWitness isScript = if not isScript then GYTxCertWitnessKey else GYTxCertWitnessScript (GYStakeValScript aStakeValidator) unitRedeemer

resolveWdrlWitness :: Bool -> GYTxWdrlWitness 'PlutusV2
resolveWdrlWitness isScript = if not isScript then GYTxWdrlWitnessKey else GYTxWdrlWitnessScript (GYStakeValScript aStakeValidator) unitRedeemer

-- This will check if we are able to register a stake credential without it's witness.
registerStakeCredentialSteps :: GYCoinSelectionStrategy -> User -> Maybe GYStakeValidatorHash -> (String -> IO ()) -> Ctx -> IO ()
registerStakeCredentialSteps strat user mstakeValHash info ctx = do
  mstakeAddressInfo <- ctxRunC ctx user $ stakeAddressInfo (resolveStakeAddress user mstakeValHash)
  if isJust mstakeAddressInfo then do
    info "Stake credential already registered\n"
  else do
    pp <- ctxGetParams ctx & gyGetProtocolParameters'
    info $ "-- Protocol parameters --\n" <> show pp <> "\n-- x --\n"
    txBodyReg <- ctxRunIWithStrategy strat ctx user $ do
      return $ mustHaveCertificate (mkStakeAddressRegistrationCertificate (resolveStakeCredential user mstakeValHash))
    info $ "-- Registration tx body --\n" <> show txBodyReg <> "\n-- x --\n"
    void $ submitTx ctx user txBodyReg

delegateStakeCredentialSteps :: GYCoinSelectionStrategy -> User -> Maybe GYStakeValidatorHash -> GYStakePoolId -> (String -> IO ()) -> Ctx -> IO ()
delegateStakeCredentialSteps strat user mstakeValHash spId info ctx = do
  txBodyDel <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveCertificate (mkStakeAddressPoolDelegationCertificate (resolveStakeCredential user mstakeValHash) spId (resolveCertWitness (isJust mstakeValHash)))
  info $ "-- Delegation tx body --\n" <> show txBodyDel <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyDel (resolveSigningRequirement user mstakeValHash)

deregisterStakeCredentialSteps :: GYCoinSelectionStrategy -> User -> Maybe GYStakeValidatorHash -> (String -> IO ()) -> Ctx -> IO ()
deregisterStakeCredentialSteps strat user mstakeValHash info ctx = do
  txBodyDereg <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveCertificate (mkStakeAddressDeregistrationCertificate (resolveStakeCredential user mstakeValHash) (resolveCertWitness (isJust mstakeValHash)))
  info $ "-- Deregistration tx body --\n" <> show txBodyDereg <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyDereg (resolveSigningRequirement user mstakeValHash)

withdrawRewardsSteps :: GYCoinSelectionStrategy -> User -> Maybe GYStakeValidatorHash -> Natural -> (String -> IO ()) -> Ctx -> IO ()
withdrawRewardsSteps strat user mstakeValHash rewards info ctx = do
  txBodyWithdraw <- ctxRunIWithStrategy strat ctx user $ do
    return $ mustHaveWithdrawal (GYTxWdrl (resolveStakeAddress user mstakeValHash) rewards (resolveWdrlWitness (isJust mstakeValHash))) <> case mstakeValHash of
      Just _  -> mustHaveOutput (mkGYTxOutNoDatum someAddr mempty)
      Nothing -> mempty
  info $ "-- Withdrawal tx body --\n" <> show txBodyWithdraw <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyWithdraw (resolveSigningRequirement user mstakeValHash)

stakeIntegrationTest :: Maybe GYStakeValidatorHash -> (String -> IO ()) -> Ctx -> IO ()
stakeIntegrationTest mstakeValHash info ctx = do
  for_ [minBound .. maxBound] $ \strat -> do
    user <- case mstakeValHash of
      Just stakeValHash -> createMangledUser ctx (GYStakeCredentialByScript stakeValHash)
      Nothing -> newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 1_000_000_000) (CreateUserConfig {cucGenerateCollateral = False, cucGenerateStakeKey = True})
    registerStakeCredentialSteps strat user mstakeValHash info ctx
    sps <- ctx & ctxGetParams & gyGetStakePools'
    info $ "Total stake pools: " <> show sps <> "\n"
    let spId = Set.findMin sps & stakePoolIdFromApi
    info $ "Stake pool id: " <> show spId <> "\n"
    delegateStakeCredentialSteps strat user mstakeValHash spId info ctx
    Just GYStakeAddressInfo {..} <- ctxRunC ctx user $ stakeAddressInfo (resolveStakeAddress user mstakeValHash)
    assertBool "Delegation failed" $ gyStakeAddressInfoDelegatedPool == Just spId
    withdrawRewardsSteps strat user mstakeValHash gyStakeAddressInfoAvailableRewards info ctx
    deregisterStakeCredentialSteps strat user mstakeValHash info ctx
