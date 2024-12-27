module GeniusYield.Test.Privnet.DRep (
  drepTests,
) where

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
    [ testCaseSteps "able to register drep" $ \info -> withSetup info setup $ \ctx -> do
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
    txBody <- buildTxBody $ mustHaveCertificate $ mkDRepRegisterationCertificate drepCred Nothing GYTxCertWitnessKey
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey drepSKey]
    fundBalF <- queryBalance fundAddr
    gyLogInfo' "" $ "Balance lost: " <> show (valueMinus fundBalI fundBalF)
    pure tid
  info $ "Successfully registered drep, with tx id: " <> show txId
