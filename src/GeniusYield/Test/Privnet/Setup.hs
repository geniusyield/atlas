{-# LANGUAGE LambdaCase #-}
{-|
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
    cardanoDefaultTestnetOptions
) where

import           Control.Concurrent                   (ThreadId, threadDelay, killThread)
import qualified Control.Concurrent.STM               as STM
import           Control.Exception                    (finally)
import           Control.Monad                        (forever)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Resource         (resourceForkIO, MonadResource (liftResourceT))
import qualified Data.Vector                          as V

import qualified Hedgehog                             as H
import qualified Hedgehog.Extras.Stock                as H'

import qualified Cardano.Api                          as Api
import           Cardano.Testnet
import           Testnet.Runtime
import           Testnet.Property.Utils


import qualified GeniusYield.Api.TestTokens           as GY.TestTokens
import           GeniusYield.Imports
import           GeniusYield.Providers.LiteChainIndex
import           GeniusYield.Providers.Node
import           GeniusYield.Providers.Node.AwaitTx   (nodeAwaitTxConfirmed)
import           GeniusYield.Providers.Node.Query     (nodeQueryUTxO)
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types


-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

-- | This setup represents a two argument function where first argument is for logging & second represents for continuation, in need of `Ctx`.
--
-- Once these two arguments are given to this function, it will give `Ctx` to the continuation, where the logging part (the `ctxLog`) of `Ctx` would be obtained from first argument of this function.
newtype Setup = Setup ((String -> IO ()) -> (Ctx -> IO ()) -> IO ())

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

data PrivnetRuntime = PrivnetRuntime
  { runtimeNodeSocket  :: !FilePath
  , runtimeNetworkInfo :: !GYNetworkInfo
  , runtimeWallets     :: ![PaymentKeyInfo]
  , runtimeThreadId    :: !ThreadId
  }

debug :: String -> IO ()
-- FIXME: change me to debug setup code.
debug = putStrLn
-- debug _ = return ()

-- | Calls the `Setup` function with a logging function and the action you wish to use with the privnet.
withSetup :: Setup -> (String -> IO ()) -> (Ctx -> IO ()) -> IO ()
withSetup (Setup cokont) putLog kont = do
    cokont putLog kont

-- | Spawn a resource managed privnet and do things with it (closing it in the end).
-- Returns `Setup`, which is essentially a function that performs an action given a logging
-- function and the action itself (which receives the Privnet Ctx).
withPrivnet :: CardanoTestnetOptions -> (Setup -> IO ()) -> IO ()
withPrivnet testnetOpts setupUser = do
    -- Based on: https://github.com/IntersectMBO/cardano-node/blob/master/cardano-testnet/src/Testnet/Property/Run.hs
    -- They are using hedgehog (property testing framework) to orchestrate a testnet running in the background
    -- ....for some god forsaken reason
    -- the result is very awkward.
    tmvRuntime <- STM.newEmptyTMVarIO

    void . H.check $ integrationWorkspace "testnet" $ \workspaceDir -> do
        conf <- mkConf workspaceDir

        -- Fork a thread to keep alive indefinitely any resources allocated by testnet.
        threadId <- H.evalM . liftResourceT . resourceForkIO  . forever . liftIO $ threadDelay 10000000

        TestnetRuntime
            { wallets
            , poolNodes
            , testnetMagic
            } <- cardanoTestnetDefault testnetOpts conf

        era <- case cardanoNodeEra testnetOpts of
            Api.AnyCardanoEra Api.AlonzoEra -> pure GYAlonzo
            Api.AnyCardanoEra Api.BabbageEra -> pure GYBabbage
            Api.AnyCardanoEra x -> liftIO . die $ printf "Unsupported era: %s" (show x)
        liftIO . STM.atomically
            $ STM.writeTMVar tmvRuntime PrivnetRuntime
                -- TODO: Consider obtaining everything here from shelleyGenesis rather than testnetOpts.
                -- See: https://www.doitwithlovelace.io/haddock/cardano-ledger-shelley/html/Cardano-Ledger-Shelley-Genesis.html
                -- See: https://github.com/IntersectMBO/cardano-node/blob/43149909fc4942e93e14a2686826543a2d9432bf/cardano-testnet/src/Testnet/Types.hs#L155
                { runtimeNodeSocket = H'.sprocketSystemName
                        . nodeSprocket
                        . poolRuntime
                        $ head poolNodes
                , runtimeNetworkInfo = GYNetworkInfo
                    { gyNetworkEra        = era
                        -- TODO: Conway support.
                    , gyNetworkEpochSlots = fromIntegral $ cardanoEpochLength testnetOpts
                    , gyNetworkMagic      = fromIntegral testnetMagic
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
        } <- STM.atomically $ STM.readTMVar tmvRuntime

    let runtimeNetworkId = GYPrivnet runtimeNetworkInfo

    (`finally` killThread runtimeThreadId) $ do

        -- throwIO @IOError @() $ userError "Forceful"

        -- Read pre-existing users.
        -- NOTE: As of writing, cardano-testnet creates three (3) users.
        genesisUsers <- fmap V.fromList . liftIO . forM (zip [0 :: Int ..] runtimeWallets)
            $ \(idx, PaymentKeyInfo {paymentKeyInfoPair, paymentKeyInfoAddr}) -> do
                debug $ printf "userF = %s\n" (show idx)
                userAddr <- addressFromBech32 <$> urlPieceFromText paymentKeyInfoAddr
                debug $ printf "userF addr = %s\n" userAddr
                userPaymentSKey <- readPaymentSigningKey $ paymentSKey paymentKeyInfoPair
                debug $ printf "userF skey = %s\n" userPaymentSKey
                pure User {userPaymentSKey, userStakeSKey=Nothing, userAddr}

        -- Generate upto 9 users.
        let extraIndices = [length genesisUsers + 1..9]
        extraUsers <- fmap V.fromList . forM extraIndices $ \idx -> do
            User {userPaymentSKey, userAddr, userStakeSKey} <- generateUser runtimeNetworkId
            debug $ printf "user = %s\n" (show idx)
            debug $ printf "user addr = %s\n" userAddr
            debug $ printf "user skey = %s\n" (show userPaymentSKey)
            debug $ printf "user vkey = %s\n" (show $ paymentVerificationKey userPaymentSKey)
            debug $ printf "user pkh  = %s\n" (show $ paymentKeyHash $ paymentVerificationKey userPaymentSKey)
            pure User {userPaymentSKey, userAddr, userStakeSKey}

        -- Further down we need local node connection
        let info :: Api.LocalNodeConnectInfo
            info = Api.LocalNodeConnectInfo
                { Api.localConsensusModeParams = Api.CardanoModeParams . Api.EpochSlots $ gyNetworkEpochSlots runtimeNetworkInfo
                , Api.localNodeNetworkId       = networkIdToApi runtimeNetworkId
                , Api.localNodeSocketPath      = Api.File runtimeNodeSocket
                }

        -- ask current slot, so we know local node connection works
        slot <- nodeGetSlotOfCurrentBlock info
        debug $ printf "slotOfCurrentBlock = %s\n" slot

        withLCIClient info [] $ \lci -> do
            let era = gyNetworkEra runtimeNetworkInfo

            let localLookupDatum :: GYLookupDatum
                localLookupDatum = lciLookupDatum lci

            let localAwaitTxConfirmed :: GYAwaitTx
                localAwaitTxConfirmed = nodeAwaitTxConfirmed era info

            let localQueryUtxo :: GYQueryUTxO
                localQueryUtxo = nodeQueryUTxO era info

            let localGetParams :: GYGetParameters
                localGetParams = nodeGetParameters era info

            -- context used for tests
            --
            let allUsers = genesisUsers <> extraUsers
            let ctx0 :: Ctx
                ctx0 = Ctx
                    { ctxNetworkInfo      = runtimeNetworkInfo
                    , ctxInfo             = info
                    -- FIXME: Some of the users which are supposed to be non genesis are actually genesis.
                    -- This is because we have multiple genesis users with cardano testnet.
                    -- Need a better (more dynamic mechanism for users).
                    , ctxUserF            = V.head allUsers
                    , ctxUser2            = allUsers V.! 1
                    , ctxUser3            = allUsers V.! 2
                    , ctxUser4            = allUsers V.! 3
                    , ctxUser5            = allUsers V.! 4
                    , ctxUser6            = allUsers V.! 5
                    , ctxUser7            = allUsers V.! 6
                    , ctxUser8            = allUsers V.! 7
                    , ctxUser9            = allUsers V.! 8
                    , ctxGold             = GYLovelace -- temporarily
                    , ctxIron             = GYLovelace -- temporarily
                    , ctxLog              = noLogging
                    , ctxLookupDatum      = localLookupDatum
                    , ctxAwaitTxConfirmed = localAwaitTxConfirmed
                    , ctxQueryUtxos       = localQueryUtxo
                    , ctxGetParams        = localGetParams
                    }

            userBalances <- V.mapM
                (\(i, User{userAddr=userIaddr}) -> do
                    userIbalance <- ctxRunC ctx0 (ctxUserF ctx0) $ queryBalance userIaddr
                    when (isEmptyValue userIbalance) $ do
                        debug $ printf "User %s balance is empty, giving some ada\n" (show i)
                        giveAda ctx0 userIaddr
                        when (i == 0) (giveAda ctx0 . userAddr $ ctxUserF ctx0) -- we also give ada to itself to create some small utxos
                    ctxRunC ctx0 (ctxUserF ctx0) $ queryBalance userIaddr
                ) $ V.zip
                    (V.fromList extraIndices)
                    extraUsers

            -- mint test tokens
            goldAC <- mintTestTokens ctx0 "GOLD"
            debug $ printf "gold = %s\n" goldAC

            ironAC <- mintTestTokens ctx0 "IRON"
            debug $ printf "iron = %s\n" ironAC

            let ctx :: Ctx
                ctx = ctx0
                    { ctxGold = goldAC
                    , ctxIron = ironAC
                    }

            -- distribute tokens
            mapM_
                (\(i, userIbalance, user) -> do
                    when (isEmptyValue $ snd $ valueSplitAda userIbalance) $ do
                        debug $ printf "User%s has no tokens, giving some\n" (show i)
                        giveTokens ctx (userAddr user)
                )
                $ V.zip3
                    (V.fromList extraIndices)
                    userBalances
                    extraUsers

            let setup = Setup $ \putLog kont -> kont $ ctx { ctxLog = simpleConsoleLogging putLog }
            setupUser setup

-------------------------------------------------------------------------------
-- Generating users
-------------------------------------------------------------------------------

generateUser :: GYNetworkId -> IO User
generateUser network = do
    -- generate new key
    skey <- Api.generateSigningKey Api.AsPaymentKey

    -- construct address (no stake)
    let vkey     = Api.getVerificationKey skey
        vkeyHash = Api.verificationKeyHash vkey

    let addr :: GYAddress
        addr = addressFromApi' $ Api.AddressInEra
            (Api.ShelleyAddressInEra Api.ShelleyBasedEraBabbage)
            (Api.makeShelleyAddress
                (networkIdToApi network)
                (Api.PaymentCredentialByKey vkeyHash)
                stake
            )

    pure User {userPaymentSKey=paymentSigningKeyFromApi skey, userAddr=addr, userStakeSKey=Nothing}
  where
    stake   = Api.NoStakeAddress

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

giveAda :: Ctx -> GYAddress -> IO ()
giveAda ctx addr = do
    ownRefs <- gyQueryUtxosAtAddress' (ctxQueryUtxos ctx) (userAddr $ ctxUserF ctx) Nothing
    print ownRefs
    txBody <- ctxRunI ctx (ctxUserF ctx) $ return $ mconcat $ replicate 5 $
        mustHaveOutput $ mkGYTxOutNoDatum addr (valueFromLovelace 1_000_000_000)
    void $ submitTx ctx (ctxUserF ctx) txBody

giveTokens :: Ctx -> GYAddress -> IO ()
giveTokens ctx addr = do
    txBody <- ctxRunI ctx (ctxUserF ctx) $ return $
        mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxGold ctx) 1_000_000)) <>
        mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxIron ctx) 1_000_000))
    void $ submitTx ctx (ctxUserF ctx) txBody

-------------------------------------------------------------------------------
-- minting tokens
-------------------------------------------------------------------------------

mintTestTokens :: Ctx -> String -> IO GYAssetClass
mintTestTokens ctx tn' = do
    (ac, txBody) <- ctxRunF ctx (ctxUserF ctx) $
        GY.TestTokens.mintTestTokens tn 10_000_000
    void $ submitTx ctx (ctxUserF ctx) txBody

    pure ac
  where
    tn :: GYTokenName
    tn = fromString tn'
