module GeniusYield.Test.Privnet.Gov (
  govTests,
) where

import Control.Lens ((^.))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import GeniusYield.Test.Privnet.Committee
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.Test.Privnet.Stake.Utils
import GeniusYield.Transaction.CoinSelection
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

govTests :: Setup -> TestTree
govTests setup =
  testGroup
    "gov"
    [ testCaseSteps "able to exercise proposal & voting procedure" $ \info -> withSetup info setup $ \ctx -> do
        exerciseGov ctx info
    ]

exerciseGov :: Ctx -> (String -> IO ()) -> IO ()
exerciseGov ctx info = do
  newUser <- newTempUserCtx ctx (ctxUserF ctx) (valueFromLovelace 100_000_000) (CreateUserConfig {cucGenerateStakeKey = True, cucGenerateCollateral = True})
  pp <- ctxRunQuery ctx protocolParams
  info $ "Gov action deposit: " <> show (pp ^. ppGovActionDepositL)
  info $ "Generated new user: " <> show newUser
  info "Registering stake credential of this user"
  registerStakeCredentialSteps GYRandomImproveMultiAsset newUser Nothing info ctx
  info "Registered stake credential of this user"
  txId <- ctxRun ctx newUser $ do
    fundAddr <- ownChangeAddress
    fundBalI <- queryBalance fundAddr
    let propProc = GYProposalProcedurePB {propProcPBReturnAddr = fromJust $ userStakeAddress (ctxNetworkId ctx) newUser, propProcPBGovAction = InfoAction, propProcPBAnchor = GYAnchor {anchorUrl = unsafeTextToUrl "https://www.geniusyield.co", anchorDataHash = hashAnchorData "we are awesome"}}
    txBody <- buildTxBody $ mustHaveProposalProcedure @'PlutusV3 propProc GYTxBuildWitnessKey
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey newUser]
    fundBalF <- queryBalance fundAddr
    gyLogInfo' "" $ "Balance lost: " <> show (valueMinus fundBalI fundBalF)
    pure tid
  info $ "Successfully exercised proposal procedure, with tx id: " <> show txId
  (_, hotSKey) <- delegateHotKey ctx info newUser 1
  txIdVote <- ctxRun ctx newUser $ do
    fundAddr <- ownChangeAddress
    fundBalI <- queryBalance fundAddr
    txBody <- buildTxBody $ mustHaveVotingProcedures @'PlutusV3 (Map.fromList [(CommitteeVoter (GYCredentialByKey $ verificationKeyHash $ getVerificationKey hotSKey), (GYTxBuildWitnessKey, Map.fromList [(GYGovActionId {gaidTxId = txId, gaidIx = 0}, GYVotingProcedure Yes Nothing)]))])
    gyLogInfo' "" $ "txBody: " <> show txBody
    tid <- submitTxBodyConfirmed txBody [GYSomeSigningKey $ userPaymentSKey newUser, GYSomeSigningKey hotSKey]
    fundBalF <- queryBalance fundAddr
    gyLogInfo' "" $ "Balance lost: " <> show (valueMinus fundBalI fundBalF)
    pure tid
  info $ "Successfully voted on the proposal, with tx id: " <> show txIdVote
