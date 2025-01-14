module GeniusYield.Test.Privnet.StakePool (
  stakePoolTests,
) where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GeniusYield.Test.Privnet.Asserts
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

stakePoolTests :: Setup -> TestTree
stakePoolTests setup =
  testGroup
    "poolTests"
    [ testCaseSteps "able to register, reregister & unregister stake pool" $ \info -> withSetup info setup $ \ctx -> do
        exerciseStakePool ctx info
    ]

exerciseStakePool :: Ctx -> (String -> IO ()) -> IO ()
exerciseStakePool ctx info = do
  info "Generating a stake pool key"
  stakePoolSKey <- generateSigningKey @'GYKeyRoleStakePool
  stakeOwnerSKey <- generateSigningKey @'GYKeyRoleStaking
  stakeRA <- generateSigningKey @'GYKeyRoleStaking
  let stakePoolVKey = getVerificationKey stakePoolSKey
      stakePoolVKH = verificationKeyHash stakePoolVKey
      stakePoolOwnerVKH = verificationKeyHash $ getVerificationKey stakeOwnerSKey
      stakeRAVKH = verificationKeyHash $ getVerificationKey stakeRA
  info $ "Generated stakePool key: " <> show stakePoolSKey <> ", with verification key hash: " <> show stakePoolVKH
  pp <- ctxRunQuery ctx protocolParams
  info $ "Pool deposit: " <> show (pp ^. ppPoolDepositL)
  let fundUser = ctxUserF ctx
      poolParams =
        GYPoolParams
          { poolId = stakePoolVKH
          , poolVrf = either (error "unable to desrialise vrfkeyhash") id $ vrfVerKeyHashFromRawBytesHex "e132b26f3af1bd2a3d6af837ab09a60742625726415a29ca60d4778ff93da74e"
          , poolPledge = 0
          , poolCost = fromIntegral $ pp ^. ppMinPoolCostL
          , poolMargin = fromMaybe (error "Invalid poolMargin") $ boundRational 0.1
          , poolRewardAccount = stakeAddressFromCredential GYTestnetPreprod $ GYStakeCredentialByKey stakeRAVKH
          , poolOwners = Set.singleton stakePoolOwnerVKH
          , poolRelays = []
          , poolMetadata = Nothing
          }
  txId <- ctxRun ctx fundUser $ do
    fundAddr <- ownChangeAddress
    fundBalI <- queryBalance fundAddr
    txBody <- buildTxBody $ mustHaveCertificate $ mkStakePoolRegistrationCertificate poolParams
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey stakePoolSKey, GYSomeSigningKey stakeOwnerSKey]
    fundBalF <- queryBalance fundAddr
    gyLogInfo' "" $ "Balance lost: " <> show (valueMinus fundBalI fundBalF) -- should be close to pool deposit
    pure tid
  sps <- ctxRunQuery ctx stakePools
  info $ "Stake pools: " <> show sps
  assertBool "Stake pool not found" $ Set.member (stakePoolIdToApi stakePoolVKH) sps
  info $ "Successfully registered stakePool, with tx id: " <> show txId
  info "Updating stakePool"
  let anchor = GYAnchor (unsafeTextToUrl "https://www.geniusyield.co") (hashAnchorData "we are awesome")
      poolParams' = poolParams {poolMetadata = Just anchor}
  txIdUpd <- ctxRun ctx fundUser $ do
    txBody <- buildTxBody $ mustHaveCertificate $ mkStakePoolRegistrationCertificate poolParams'
    gyLogInfo' "" $ "txBody: " <> show txBody
    submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey stakePoolSKey, GYSomeSigningKey stakeOwnerSKey]
  info $ "Successfully updated stakePool, with tx id: " <> show txIdUpd
  info "Unregistering stakePool"
  txIdUnreg <- ctxRun ctx fundUser $ do
    currSlot <- slotOfCurrentBlock
    GYEpochNo epoch <- slotToEpoch currSlot
    let retirementEpoch = GYEpochNo $ epoch + 1
    txBody <- buildTxBody $ mustHaveCertificate $ mkStakePoolRetirementCertificate stakePoolVKH retirementEpoch
    gyLogInfo' "" $ "txBody: " <> show txBody
    submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey fundUser, GYSomeSigningKey stakePoolSKey]
  info $ "Successfully unregistered stakePool, with tx id: " <> show txIdUnreg
