module GeniusYield.Test.Privnet.DRep (
  drepTests,
) where

import GeniusYield.Test.Privnet.Asserts
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

drepTests :: Setup -> TestTree
drepTests setup =
  testGroup
    "drep"
    [ testCaseSteps "able to register, update & unregister drep" $ \info -> withSetup info setup $ \ctx -> do
        exerciseDRep ctx info
    ]

exerciseDRep :: Ctx -> (String -> IO ()) -> IO ()
exerciseDRep ctx info = do
  info "Generating a drep key"
  drepSKey <- generateSigningKey @'GYKeyRoleDRep
  let drepVKey = getVerificationKey drepSKey
      drepVKH = verificationKeyHash drepVKey
      drepCred = GYCredentialByKey drepVKH
  info $ "Generated drep key: " <> show drepSKey <> ", with verification key hash: " <> show drepVKH
  let fundUser = ctxUserF ctx
  txId <- ctxRun ctx fundUser $ do
    fundAddr <- ownChangeAddress
    fundBalI <- queryBalance fundAddr
    txBody <- buildTxBody $ mustHaveCertificate $ mkDRepRegistrationCertificate drepCred Nothing GYTxCertWitnessKey
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey drepSKey]
    fundBalF <- queryBalance fundAddr
    gyLogInfo' "" $ "Balance lost: " <> show (valueMinus fundBalI fundBalF)
    drepS <- drepState drepCred
    gyLogInfo' "" $ "Drep state: " <> show drepS
    pure tid
  info $ "Successfully registered drep, with tx id: " <> show txId
  info "Updating drep"
  let anchor = GYAnchor (unsafeTextToUrl "https://www.geniusyield.io") (hashAnchorData "we are awesome")
  (txIdUpd, mdrepS) <- ctxRun ctx fundUser $ do
    txBody <- buildTxBody $ mustHaveCertificate $ mkDRepUpdateCertificate drepCred (Just anchor) GYTxCertWitnessKey
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey drepSKey]
    drepS <- drepState drepCred
    gyLogInfo' "" $ "Drep state: " <> show drepS
    pure (tid, drepS)
  info $ "Successfully updated drep, with tx id: " <> show txIdUpd
  assertEqual "Drep state after update" (Just anchor) (mdrepS >>= drepAnchor)
  info "Unregistering drep"
  case mdrepS of
    Nothing -> assertFailure "Drep state not found"
    Just drepS -> do
      txIdUnreg <- ctxRun ctx fundUser $ do
        txBody <- buildTxBody $ mustHaveCertificate $ mkDRepUnregistrationCertificate drepCred (drepDeposit drepS) GYTxCertWitnessKey
        gyLogInfo' "" $ "txBody: " <> show txBody
        submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey drepSKey]
      info $ "Successfully unregistered drep, with tx id: " <> show txIdUnreg
