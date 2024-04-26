module GeniusYield.Test.Privnet.Stake (
    tests,
) where

import           Data.Maybe                     (fromJust)
import           GeniusYield.Imports
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder          (mustHaveCertificate)
import           GeniusYield.Types
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (testCaseSteps)

-- This will check if we are able to register a stake credential without it's signature.
registerStakeCredentialSteps :: (String -> IO ()) -> Ctx -> IO User
registerStakeCredentialSteps info ctx = do
  newUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 1_000_000_000) (CreateUserConfig {cucGenerateCollateral = False, cucGenerateStakeKey = True})
  pp <- ctxGetParams ctx & gyGetProtocolParameters'
  info $ "-- Protocol parameters --\n" <> show pp <> "\n-- x --\n"
  txBodyReg <- ctxRunI ctx newUser $ do
    return $ mustHaveCertificate (mkStakeAddressRegistrationCertificate (userStakePkh newUser & fromJust & GYStakeCredentialByKey))
  info $ "-- Tx body --\n" <> show txBodyReg <> "\n-- x --\n"
  void $ submitTx ctx newUser txBodyReg
  pure newUser

deregisterStakeCredentialSteps :: User -> (String -> IO ()) -> Ctx -> IO ()
deregisterStakeCredentialSteps user@User{..} info ctx = do
  txBodyDereg <- ctxRunI ctx user $ do
    return $ mustHaveCertificate (mkStakeAddressDeregistrationCertificate (userStakePkh user & fromJust & GYStakeCredentialByKey) GYTxCertWitnessKey)
  info $ "-- Tx body --\n" <> show txBodyDereg <> "\n-- x --\n"
  void $ submitTx' ctx $ signGYTxBody txBodyDereg [GYSomeSigningKey userPaymentSKey, userStakeSKey & fromJust & GYSomeSigningKey]

tests :: IO Setup -> TestTree
tests setup = testGroup "stake"
  [ testCaseSteps "able to deregister a registered stake credential" $ \info -> withSetup setup info $ \ctx -> do
    newUser <- registerStakeCredentialSteps info ctx
    deregisterStakeCredentialSteps newUser info ctx
  ]
