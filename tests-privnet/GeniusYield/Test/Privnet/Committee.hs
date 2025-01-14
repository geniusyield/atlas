module GeniusYield.Test.Privnet.Committee (
  committeeTests,
  delegateHotKey,
) where

import Data.Map.Strict qualified as Map
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

committeeTests :: Setup -> TestTree
committeeTests setup =
  testGroup
    "committeeTests"
    [ testCaseSteps "able to authorize hot key & resign cold key" $ \info -> withSetup info setup $ \ctx -> do
        exerciseCommittee ctx info
    ]

getColdCred :: GYSigningKey kr -> GYCredential kr
getColdCred = GYCredentialByKey . verificationKeyHash . getVerificationKey

delegateHotKey :: Ctx -> (String -> IO ()) -> User -> IO (GYSigningKey 'GYKeyRoleColdCommittee, GYSigningKey 'GYKeyRoleHotCommittee)
delegateHotKey ctx info fundUser = do
  info "Generating a hot committee key"
  hotSKey <- generateSigningKey @'GYKeyRoleHotCommittee
  let hotCred = GYCredentialByKey $ verificationKeyHash $ getVerificationKey hotSKey
  info $ "Generated hot key: " <> show hotSKey <> ", with corresponding credential: " <> show hotCred
  let coldKey = ctxCommittee ctx & ctxCommitteeMembers & Map.findMin & fst
      coldCred = getColdCred coldKey
  info $ "Cold key: " <> show coldKey <> ", with corresponding credential: " <> show coldCred
  txId <- ctxRun ctx fundUser $ do
    txBody <- buildTxBody $ mustHaveCertificate $ mkCommitteeHotKeyAuthCertificate coldCred hotCred
    gyLogInfo' "" $ "txBody: " <> show txBody
    submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey coldKey]
  info $ "Successfully authorized hot key, with tx id: " <> show txId
  pure (coldKey, hotSKey)

exerciseCommittee :: Ctx -> (String -> IO ()) -> IO ()
exerciseCommittee ctx info = do
  let fundUser = ctxUserF ctx
  (coldKey, _) <- delegateHotKey ctx info fundUser
  let anchor = GYAnchor (unsafeTextToUrl "https://www.geniusyield.co") (hashAnchorData "we are awesome")
      coldCred = getColdCred coldKey
  info "Resigning cold key"
  txIdUnreg <- ctxRun ctx fundUser $ do
    txBody <- buildTxBody $ mustHaveCertificate $ mkCommitteeColdKeyResignationCertificate coldCred (Just anchor)
    gyLogInfo' "" $ "txBody: " <> show txBody
    submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey coldKey]
  info $ "Successfully resigned cold key, with tx id: " <> show txIdUnreg
