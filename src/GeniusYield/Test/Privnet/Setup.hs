{- |
Module      : GeniusYield.Test.Privnet.Setup
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Test.Privnet.Setup (
  Setup,
  withPrivnet,
  withSetup,
  withSetup',
  withSetupOld,
  mkPrivnetTestFor,
  mkPrivnetTestFor',

  -- * "Cardano.Testnet" re-exports
  cardanoDefaultTestnetOptionsConway,
  cardanoDefaultTestnetNodeOptions,
  CardanoTestnetOptions (..),
  TestnetNodeOptions (..),
  NodeLoggingFormat (..),
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger
import Cardano.Ledger.Conway.Governance qualified as Ledger
import Cardano.Ledger.Plutus qualified as Ledger
import Cardano.Testnet
import Control.Concurrent (
  ThreadId,
  killThread,
  threadDelay,
 )
import Control.Concurrent.STM qualified as STM
import Control.Exception (finally)
import Control.Monad (forever, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (
  MonadResource (liftResourceT),
  resourceForkIO,
 )
import Data.Default (Default (..))
import Data.Default.Class qualified as DefaultClass
import Data.Map.Strict qualified as Map
import Data.Text qualified as Txt
import Data.Vector qualified as V
import GeniusYield.Api.TestTokens qualified as GY.TestTokens
import GeniusYield.Imports
import GeniusYield.Providers.LiteChainIndex
import GeniusYield.Providers.Node
import GeniusYield.Providers.Node.AwaitTx (nodeAwaitTxConfirmed)
import GeniusYield.Providers.Node.Query (nodeQueryUTxO)
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Utils
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Hedgehog qualified as H
import Hedgehog.Extras.Stock qualified as H'
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational, (%!))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Testnet.Property.Util
import Testnet.Start.Types (GenesisOptions (..), UserNodeConfig (UserNodeConfigNotSubmitted))
import Testnet.Types hiding (shelleyGenesis)

-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

{- | This setup represents a three argument function where first two arguments are for logging & third is for the continuation, in need of `Ctx`.

Once these arguments are given to this function, it will give `Ctx` to the continuation, where the logging part (the `ctxLog`) of `Ctx` would be obtained from the first two arguments of this function.

The first argument is the log severity filter. Only logs of this severity or higher will be passed on to the second argument, which is a logging action.
-}
newtype Setup = Setup (GYLogSeverity -> (String -> IO ()) -> (Ctx -> IO ()) -> IO ())

cardanoDefaultTestnetOptionsConway :: (CardanoTestnetOptions, GenesisOptions)
cardanoDefaultTestnetOptionsConway = (def {cardanoNodeEra = Api.AnyShelleyBasedEra Api.ShelleyBasedEraConway}, def {genesisEpochLength = 2000})
data PrivnetRuntime = PrivnetRuntime
  { runtimeNodeSocket :: !FilePath
  , runtimeNetworkInfo :: !GYNetworkInfo
  , runtimeWallets :: ![PaymentKeyInfo]
  , runtimeThreadId :: !ThreadId
  }

{-# DEPRECATED withSetupOld "Use withSetup." #-}
withSetupOld :: Setup -> (String -> IO ()) -> (Ctx -> IO ()) -> IO ()
withSetupOld = flip withSetup

-- | Calls the `Setup` function with a logging function that receives info severity logs, and the action you wish to use with the privnet.
withSetup :: (String -> IO ()) -> Setup -> (Ctx -> IO ()) -> IO ()
withSetup = withSetup' GYInfo

-- | Calls the `Setup` function with target logging severity, a logging function and the action you wish to use with the privnet.
withSetup' :: GYLogSeverity -> (String -> IO ()) -> Setup -> (Ctx -> IO ()) -> IO ()
withSetup' targetSev putLog (Setup cokont) kont = do
  cokont targetSev putLog kont

-- | Given a test name, runs the test under privnet.
mkPrivnetTestFor :: TestName -> Setup -> (TestInfo -> GYTxGameMonadIO ()) -> TestTree
mkPrivnetTestFor name = mkPrivnetTestFor' name GYDebug

-- | Given a test name, runs the test under privnet with target logging severity.
mkPrivnetTestFor' :: TestName -> GYLogSeverity -> Setup -> (TestInfo -> GYTxGameMonadIO ()) -> TestTree
mkPrivnetTestFor' name targetSev setup action = testCaseSteps name $ \info -> withSetup' targetSev info setup $ \ctx -> do
  ctxRunGame ctx $ action TestInfo {testGoldAsset = ctxGold ctx, testIronAsset = ctxIron ctx, testWallets = ctxWallets ctx}

{-
TODO: WIP: Provide a variant of `withSetup` that can access `Ctx` to return a non-unit result.
TODO: Can below implementation also accept @putLog@?
-- | This is a variant of `withSetup` that can access `Ctx` to return a non-unit result.
withSetup' :: IO Setup -> (Ctx -> IO a) -> IO a
withSetup' ioSetup kont = do
    Setup cokont <- ioSetup
    mvar <- newEmptyMVar
    cokont (const $ return ()) (\ctx -> do
        res <- kont ctx
        putMVar mvar res)
    takeMVar mvar
-}

debug :: String -> IO ()
-- FIXME: change me to debug setup code.
-- debug = putStrLn
debug _ = return ()

conwayGenesis :: CtxCommittee -> ConwayGenesis StandardCrypto
conwayGenesis ctxCommittee =
  let
    upPParams :: UpgradeConwayPParams Identity
    upPParams =
      UpgradeConwayPParams
        { ucppPoolVotingThresholds = poolVotingThresholds
        , ucppDRepVotingThresholds = drepVotingThresholds
        , ucppCommitteeMinSize = 0
        , ucppCommitteeMaxTermLength = EpochInterval 200
        , ucppGovActionLifetime = EpochInterval 1 -- One Epoch
        , ucppGovActionDeposit = Coin 1_000_000
        , ucppDRepDeposit = Coin 500_000_000
        , ucppDRepActivity = EpochInterval 100
        , ucppMinFeeRefScriptCostPerByte = 15 %! 1
        , ucppPlutusV3CostModel = either (error "Couldn't build PlutusV3 cost models") id $ Ledger.mkCostModel Ledger.PlutusV3 [100788, 420, 1, 1, 1000, 173, 0, 1, 1000, 59957, 4, 1, 11183, 32, 201305, 8356, 4, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 16000, 100, 100, 100, 16000, 100, 94375, 32, 132994, 32, 61462, 4, 72010, 178, 0, 1, 22151, 32, 91189, 769, 4, 2, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 1000, 42921, 4, 2, 24548, 29498, 38, 1, 898148, 27279, 1, 51775, 558, 1, 39184, 1000, 60594, 1, 141895, 32, 83150, 32, 15299, 32, 76049, 1, 13169, 4, 22100, 10, 28999, 74, 1, 28999, 74, 1, 43285, 552, 1, 44749, 541, 1, 33852, 32, 68246, 32, 72362, 32, 7243, 32, 7391, 32, 11546, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 90434, 519, 0, 1, 74433, 32, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 1, 85848, 123203, 7305, -900, 1716, 549, 57, 85848, 0, 1, 955506, 213312, 0, 2, 270652, 22588, 4, 1457325, 64566, 4, 20467, 1, 4, 0, 141992, 32, 100788, 420, 1, 1, 81663, 32, 59498, 32, 20142, 32, 24588, 32, 20744, 32, 25933, 32, 24623, 32, 43053543, 10, 53384111, 14333, 10, 43574283, 26308, 10, 16000, 100, 16000, 100, 962335, 18, 2780678, 6, 442008, 1, 52538055, 3756, 18, 267929, 18, 76433006, 8868, 18, 52948122, 18, 1995836, 36, 3227919, 12, 901022, 1, 166917843, 4307, 36, 284546, 36, 158221314, 26549, 36, 74698472, 36, 333849714, 1, 254006273, 72, 2174038, 72, 2261318, 64571, 4, 207616, 8310, 4, 1293828, 28716, 63, 0, 1, 1006041, 43623, 251, 0, 1]
        }
    drepVotingThresholds =
      DRepVotingThresholds
        { dvtMotionNoConfidence = 67 %! 100
        , dvtCommitteeNormal = 67 %! 100
        , dvtCommitteeNoConfidence = 6 %! 10
        , dvtUpdateToConstitution = 75 %! 100
        , dvtHardForkInitiation = 6 %! 10
        , dvtPPNetworkGroup = 67 %! 100
        , dvtPPEconomicGroup = 67 %! 100
        , dvtPPTechnicalGroup = 67 %! 100
        , dvtPPGovGroup = 75 %! 100
        , dvtTreasuryWithdrawal = 67 %! 100
        }
    poolVotingThresholds =
      PoolVotingThresholds
        { pvtMotionNoConfidence = commonPoolVotingThreshold
        , pvtCommitteeNormal = commonPoolVotingThreshold
        , pvtCommitteeNoConfidence = commonPoolVotingThreshold
        , pvtHardForkInitiation = commonPoolVotingThreshold
        , pvtPPSecurityGroup = commonPoolVotingThreshold
        }
    commonPoolVotingThreshold = 51 %! 100
   in
    ConwayGenesis
      { cgUpgradePParams = upPParams
      , cgConstitution = DefaultClass.def
      , cgCommittee =
          Ledger.Committee
            { Ledger.committeeMembers = Map.map epochNoToLedger $ Map.mapKeys (\sk -> let vkh = verificationKeyHash $ getVerificationKey sk in credentialToLedger $ GYCredentialByKey vkh) $ ctxCommitteeMembers ctxCommittee
            , Ledger.committeeThreshold = ctxCommitteeThreshold ctxCommittee
            }
      , cgDelegs = mempty
      , cgInitialDReps = mempty
      }

{- | Spawn a resource managed privnet and do things with it (closing it in the end).

Privnet can be configured using "Cardano.Testnet.CardanoTestnetOptions". Pass 'cardanoDefaultTestnetOptionsConway'
for default configuration.

Note that passed @CardanoTestnetOptions@ must imply Conway era.

Returns continuation on `Setup`, which is essentially a function that performs an action
given a logging -- function and the action itself (which receives the Privnet Ctx).
-}
withPrivnet :: (CardanoTestnetOptions, GenesisOptions) -> (Setup -> IO ()) -> IO ()
withPrivnet (testnetOpts, genesisOpts) setupUser = do
  coldCommitteeMembers :: [GYSigningKey 'GYKeyRoleColdCommittee] <- replicateM 3 generateSigningKey
  let ctxCommittee :: CtxCommittee
      ctxCommittee =
        CtxCommittee
          { ctxCommitteeMembers = Map.fromList $ map (,GYEpochNo 100000000) coldCommitteeMembers
          , ctxCommitteeThreshold = unsafeBoundRational 0.51
          }
  -- Based on: https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Property/Run.hs
  -- They are using hedgehog (property testing framework) to orchestrate a testnet running in the background
  -- ....for some god forsaken reason
  -- the result is very awkward.
  tmvRuntime <- STM.newEmptyTMVarIO

  void . H.check $ integrationWorkspace "tn" $ \workspaceDir -> do
    conf <- mkConf workspaceDir

    -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
    threadId <- H.evalM . liftResourceT . resourceForkIO . forever . liftIO $ threadDelay 10000000

    TestnetRuntime
      { wallets
      , testnetNodes
      , testnetMagic
      } <-
      cardanoTestnet' testnetOpts genesisOpts conf ctxCommittee

    liftIO . STM.atomically $
      STM.writeTMVar
        tmvRuntime
        PrivnetRuntime
          { -- TODO: Consider obtaining everything here from shelleyGenesis rather than testnetOpts.
            -- See: https://www.doitwithlovelace.io/haddock/cardano-ledger-shelley/html/Cardano-Ledger-Shelley-Genesis.html
            -- See: https://github.com/IntersectMBO/cardano-node/blob/43149909fc4942e93e14a2686826543a2d9432bf/cardano-testnet/src/Testnet/Types.hs#L155
            runtimeNodeSocket =
              H'.sprocketSystemName
                . nodeSprocket
                $ head testnetNodes
          , runtimeNetworkInfo =
              GYNetworkInfo
                { gyNetworkEpochSlots = fromIntegral $ genesisEpochLength genesisOpts
                , gyNetworkMagic = fromIntegral testnetMagic
                }
          , runtimeWallets = wallets
          , runtimeThreadId = threadId
          }

    -- Forced failure (just like upstream).
    -- For some god forsaken reason, not making this whole thing fail makes the node workspace directory disappear and the nodes not run.
    -- Assumption: Hedgehog clears the workspace (since it's temp) in case of success.
    -- No clue why the nodes don't run. Laziness?
    H.failure

  PrivnetRuntime
    { runtimeNodeSocket
    , runtimeNetworkInfo
    , runtimeWallets
    , runtimeThreadId
    } <-
    STM.atomically $ STM.readTMVar tmvRuntime

  let runtimeNetworkId = GYPrivnet runtimeNetworkInfo

  -- Kill the resource holding thread at the end of all this to stop the privnet.
  (`finally` killThread runtimeThreadId) $ do
    -- Read pre-existing users.
    -- NOTE: As of writing, cardano-testnet creates three (3) users.
    genesisUsers <- fmap V.fromList . liftIO . forM (zip [1 :: Int ..] runtimeWallets) $
      \(idx, PaymentKeyInfo {paymentKeyInfoPair, paymentKeyInfoAddr}) -> do
        debug $ printf "userF = %s\n" (show idx)
        userAddr <- addressFromBech32 <$> urlPieceFromText paymentKeyInfoAddr
        debug $ printf "userF addr = %s\n" userAddr
        userPaymentSKey' <- readPaymentSigningKey $ Api.unFile $ signingKey paymentKeyInfoPair
        debug $ printf "userF skey = %s\n" userPaymentSKey'
        pure User' {userPaymentSKey', userStakeSKey' = Nothing, userAddr}

    -- Generate upto 9 users.
    let extraIndices = [length genesisUsers + 1 .. 9]
    extraUsers <- fmap V.fromList . forM extraIndices $ \idx -> do
      User' {userPaymentSKey', userAddr, userStakeSKey'} <- generateUser runtimeNetworkId
      debug $ printf "user = %s\n" (show idx)
      debug $ printf "user addr = %s\n" userAddr
      debug $ printf "user skey = %s\n" (show userPaymentSKey')
      debug $ printf "user vkey = %s\n" (show $ paymentVerificationKey userPaymentSKey')
      debug $ printf "user pkh  = %s\n" (show $ paymentKeyHash $ paymentVerificationKey userPaymentSKey')
      pure User' {userPaymentSKey', userAddr, userStakeSKey'}

    -- Further down we need local node connection
    let info :: Api.LocalNodeConnectInfo
        info =
          Api.LocalNodeConnectInfo
            { Api.localConsensusModeParams = Api.CardanoModeParams . Api.EpochSlots $ gyNetworkEpochSlots runtimeNetworkInfo
            , Api.localNodeNetworkId = networkIdToApi runtimeNetworkId
            , Api.localNodeSocketPath = Api.File runtimeNodeSocket
            }

    -- ask current slot, so we know local node connection works
    slot <- nodeGetSlotOfCurrentBlock info
    debug $ printf "slotOfCurrentBlock = %s\n" slot

    withLCIClient info [] $ \lci -> do
      let localLookupDatum :: GYLookupDatum
          localLookupDatum = lciLookupDatum lci

      let localAwaitTxConfirmed :: GYAwaitTx
          localAwaitTxConfirmed = nodeAwaitTxConfirmed info

      let localQueryUtxo :: GYQueryUTxO
          localQueryUtxo = nodeQueryUTxO info

      localGetParams <- nodeGetParameters info

      -- context used for tests
      --
      let allUsers = genesisUsers <> extraUsers
      let ctx0 :: Ctx
          ctx0 =
            Ctx
              { ctxNetworkInfo = runtimeNetworkInfo
              , ctxInfo = info
              , -- FIXME: Some of the users which are supposed to be non genesis are actually genesis.
                -- This is because we have multiple genesis users with cardano testnet.
                -- Need a better (more dynamic mechanism for users).
                ctxUserF = V.head allUsers
              , ctxUser2 = allUsers V.! 1
              , ctxUser3 = allUsers V.! 2
              , ctxUser4 = allUsers V.! 3
              , ctxUser5 = allUsers V.! 4
              , ctxUser6 = allUsers V.! 5
              , ctxUser7 = allUsers V.! 6
              , ctxUser8 = allUsers V.! 7
              , ctxUser9 = allUsers V.! 8
              , ctxGold = GYLovelace -- temporarily
              , ctxIron = GYLovelace -- temporarily
              , ctxLog = noLogging
              , ctxLookupDatum = localLookupDatum
              , ctxAwaitTxConfirmed = localAwaitTxConfirmed
              , ctxQueryUtxos = localQueryUtxo
              , ctxGetParams = localGetParams
              , ctxCommittee
              }

      V.imapM_
        ( \i User' {userAddr = userIaddr} -> do
            userIbalance <- ctxRunQuery ctx0 $ queryBalance userIaddr
            when (isEmptyValue userIbalance) $ do
              debug $ printf "User %d balance is empty, giving some ada\n" $ i + 1
              giveAda ctx0 userIaddr
              when (i == 0) (giveAda ctx0 . userAddr $ ctxUserF ctx0) -- we also give ada to itself to create some small utxos
        )
        allUsers

      -- mint test tokens
      goldAC <- mintTestTokens ctx0 "GOLD"
      debug $ printf "gold = %s\n" goldAC

      ironAC <- mintTestTokens ctx0 "IRON"
      debug $ printf "iron = %s\n" ironAC

      let ctx :: Ctx
          ctx =
            ctx0
              { ctxGold = goldAC
              , ctxIron = ironAC
              }

      -- distribute tokens
      V.imapM_
        ( \i User' {userAddr = userIaddr} -> do
            userIbalance <- ctxRunQuery ctx0 $ queryBalance userIaddr
            when (isEmptyValue $ snd $ valueSplitAda userIbalance) $ do
              debug $ printf "User %d has no tokens, giving some\n" $ i + 1
              giveTokens ctx userIaddr
        )
        allUsers

      let setup = Setup $ \targetSev putLog kont -> kont $ ctx {ctxLog = simpleLogging targetSev (putLog . Txt.unpack)}
      setupUser setup
 where
  -- \| This is defined same as `cardanoTestnetDefault` except we use our own conway genesis parameters.
  cardanoTestnet' testnetOptions shelleyOptions conf ctxCommittee = do
    Api.AnyShelleyBasedEra sbe <- pure cardanoNodeEra
    alonzoGenesis <- getDefaultAlonzoGenesis sbe
    shelleyGenesis <- getDefaultShelleyGenesis cardanoNodeEra cardanoMaxSupply shelleyOptions
    cardanoTestnet testnetOptions conf UserNodeConfigNotSubmitted shelleyGenesis alonzoGenesis (conwayGenesis ctxCommittee)
   where
    CardanoTestnetOptions {cardanoNodeEra, cardanoMaxSupply} = testnetOptions

-------------------------------------------------------------------------------
-- Generating users
-------------------------------------------------------------------------------

-- TODO (simplify-genesis): Remove this. See note 'simplify-genesis'.
generateUser :: GYNetworkId -> IO User
generateUser network = do
  -- generate new key
  skey <- Api.generateSigningKey Api.AsPaymentKey

  -- construct address (no stake)
  let vkey = Api.getVerificationKey skey
      vkeyHash = Api.verificationKeyHash vkey

  let addr :: GYAddress
      addr =
        addressFromApi' $
          Api.AddressInEra
            (Api.ShelleyAddressInEra Api.ShelleyBasedEraConway)
            ( Api.makeShelleyAddress
                (networkIdToApi network)
                (Api.PaymentCredentialByKey vkeyHash)
                stake
            )

  pure User' {userPaymentSKey' = paymentSigningKeyFromApi skey, userAddr = addr, userStakeSKey' = Nothing}
 where
  stake = Api.NoStakeAddress

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

-- TODO (simplify-genesis): Remove this once 'generateUser' and similar have been removed. Use 'createUserWithLovelace' instead.
giveAda :: Ctx -> GYAddress -> IO ()
giveAda ctx addr = ctxRun ctx (ctxUserF ctx) $ do
  txBody <-
    buildTxBody $
      mconcat $
        replicate 5 $
          mustHaveOutput $
            mkGYTxOutNoDatum addr (valueFromLovelace 1_000_000_000)
  signAndSubmitConfirmed_ txBody

-- TODO (simplify-genesis): Remove this once 'generateUser' and similar have been removed. Use 'createUserWithAssets' instead.
giveTokens :: Ctx -> GYAddress -> IO ()
giveTokens ctx addr = ctxRun ctx (ctxUserF ctx) $ do
  txBody <-
    buildTxBody $
      mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxGold ctx) 10_000_000))
        <> mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxIron ctx) 10_000_000))
  signAndSubmitConfirmed_ txBody

-------------------------------------------------------------------------------
-- minting tokens
-------------------------------------------------------------------------------

-- TODO (simplify-genesis): Remove this once 'generateUser' and similar have been removed.
mintTestTokens :: Ctx -> String -> IO GYAssetClass
mintTestTokens ctx tn' = do
  ctxRun ctx (ctxUserF ctx) $ do
    (ac, txBody) <- GY.TestTokens.mintTestTokens tn 1_000_000_000 >>= traverse buildTxBody
    signAndSubmitConfirmed_ txBody
    pure ac
 where
  tn :: GYTokenName
  tn = fromString tn'
