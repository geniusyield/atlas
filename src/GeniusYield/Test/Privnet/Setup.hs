{-# LANGUAGE LambdaCase #-}
{-|
Module      : GeniusYield.Test.Privnet.Setup
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Setup (
    makeSetup,
    closeSetup,
    Setup,
    withSetup,
) where

import           Control.Exception                    (IOException)
import           System.Environment                   (lookupEnv)
import           System.Exit                          (exitFailure)
import           System.FilePath                      ((</>))

import qualified Cardano.Api                          as Api

import qualified Data.Vector.Fixed                    as V

import qualified GeniusYield.Api.TestTokens           as GY.TestTokens
import           GeniusYield.Imports
import           GeniusYield.Providers.CardanoDbSync
import           GeniusYield.Providers.LiteChainIndex
import           GeniusYield.Providers.Node
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Options
import           GeniusYield.Test.Privnet.Paths
import           GeniusYield.Test.Privnet.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types

-- | Era in which privnet runs.
era :: GYEra
era = GYBabbage

-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

data Setup = Setup (IO ()) ((String -> IO ()) -> (Ctx -> IO ()) -> IO ())

withSetup :: IO Setup -> (String -> IO ()) -> (Ctx -> IO ()) -> IO ()
withSetup ioSetup putLog kont = do
    Setup _ cokont <- ioSetup
    cokont putLog kont

makeSetup :: DbSyncOpts -> IO Setup
makeSetup x = do
    privnetPath' <- lookupEnv "GENIUSYIELD_PRIVNET_DIR"
    case privnetPath' of
        Nothing -> return $ Setup (return ()) $ \info _kont -> do
            info "GENIUSYIELD_PRIVNET_DIR envvar is not set"

        Just privnetPath -> makeSetup' x privnetPath

closeSetup :: Setup -> IO ()
closeSetup (Setup close _) = close

debug :: String -> IO ()
-- FIXME: change me to debug setup code.
-- debug = putStrLn
debug _ = return ()

makeSetup' :: DbSyncOpts -> FilePath -> IO Setup
makeSetup' DbSyncOpts {..} privnetPath = do
    -- init paths
    paths <- initPaths privnetPath

    -- read user address
    userFaddr <- addressFromBech32 <$> urlPieceFromFile (pathUserAddr $ pathUserF paths)
    debug $ printf "userFaddr = %s\n" userFaddr

    userFskey <- readPaymentSigningKey $ pathUserSKey $ pathUserF paths
    debug $ printf "userFskey = %s\n" (show userFskey)
    debug $ printf "userFvkey = %s\n" (show $ paymentVerificationKey userFskey)
    debug $ printf "userFpkh  = %s\n" (show $ pubKeyHash $ paymentVerificationKey userFskey)

    -- Generate user 2 .. 9
    userSkeyAddr <- forM (pathUsers paths) generateUser
    let getUserIdx (i :: Int) = i + 2
    V.imapM_ (
      \i (userIskey, userIaddr) -> do
        debug $ printf "user = %s\n" (show $ getUserIdx i)
        debug $ printf "user addr = %s\n" userIaddr
        debug $ printf "user skey = %s\n" (show userIskey)
        debug $ printf "user vkey = %s\n" (show $ paymentVerificationKey userIskey)
        debug $ printf "user pkh  = %s\n" (show $ pubKeyHash $ paymentVerificationKey userIskey)
      ) userSkeyAddr

    -- Further down we need local node connection
    let info :: Api.LocalNodeConnectInfo Api.CardanoMode
        info = Api.LocalNodeConnectInfo
            { Api.localConsensusModeParams = Api.CardanoModeParams $ Api.EpochSlots 500
            , Api.localNodeNetworkId       = networkIdToApi GYPrivnet
            , Api.localNodeSocketPath      = Api.File $ pathNodeSocket paths
            }

    -- ask current slot, so we know local node connection works
    slot <- nodeGetCurrentBlock'sSlot info
    debug $ printf "currentBlock'sSlot = %s\n" slot

    lci <- newLCIClient info []

    dbSync <- traverse openDbSyncConn dbSyncConnInfo

    let localLookupDatum :: GYLookupDatum
        localLookupDatum = case dbSync of
            Just dbSync' | dbSyncOptsLookupDatum -> dbSyncLookupDatum dbSync'
            _                                    -> lciLookupDatum lci

    let localAwaitTxConfirmed :: GYAwaitTx
        localAwaitTxConfirmed = case dbSync of
            Just dbSync' | dbSyncOptsAwaitTx -> dbSyncAwaitTxConfirmed dbSync'
            _                                -> lciAwaitTxConfirmed lci

    let localQueryUtxo :: GYQueryUTxO
        localQueryUtxo = case dbSync of
            Just dbSync' | dbSyncOptsQueryUtxos -> dbSyncQueryUtxo dbSync'
            _                                   -> nodeQueryUTxO era info

    let localGetParams :: GYGetParameters
        localGetParams = case dbSync of
            Just dbSync' | dbSyncOptsGetParameters -> dbSyncGetParameters dbSync'
            _                                      -> nodeGetParameters era info

    -- context used for tests
    let ctx0 :: Ctx
        ctx0 = Ctx
            { ctxEra              = era
            , ctxInfo             = info
            , ctxLCI              = lci
            , ctxDbSync           = dbSync
            , ctxUserF            = User userFskey userFaddr
            , ctxUser2            = uncurry User (V.index userSkeyAddr (Proxy @0))
            , ctxUser3            = uncurry User (V.index userSkeyAddr (Proxy @1))
            , ctxUser4            = uncurry User (V.index userSkeyAddr (Proxy @2))
            , ctxUser5            = uncurry User (V.index userSkeyAddr (Proxy @3))
            , ctxUser6            = uncurry User (V.index userSkeyAddr (Proxy @4))
            , ctxUser7            = uncurry User (V.index userSkeyAddr (Proxy @5))
            , ctxUser8            = uncurry User (V.index userSkeyAddr (Proxy @6))
            , ctxUser9            = uncurry User (V.index userSkeyAddr (Proxy @7))
            , ctxGold             = GYLovelace -- temporarily
            , ctxIron             = GYLovelace -- temporarily
            , ctxLog              = noLogging
            , ctxLookupDatum      = localLookupDatum
            , ctxAwaitTxConfirmed = localAwaitTxConfirmed
            , ctxQueryUtxos       = localQueryUtxo
            , ctxGetParams        = localGetParams
            }

    userBalances <- V.imapM
      (\i (_, userIaddr) -> do
        userIbalance <- ctxRunC ctx0 (ctxUserF ctx0) $ queryBalance userIaddr
        when (isEmptyValue userIbalance) $ do
            debug $ printf "User %s balance is empty, giving some ada\n" (show $ getUserIdx i)
            giveAda ctx0 userIaddr
            when (i == 0) (giveAda ctx0 userFaddr) -- we also give ada to itself to create some small utxos
        ctxRunC ctx0 (ctxUserF ctx0) $ queryBalance userIaddr
      ) userSkeyAddr

    -- mint test tokens
    goldAC <- mintTestTokens paths ctx0 "GOLD"
    debug $ printf "gold = %s\n" goldAC

    ironAC <- mintTestTokens paths ctx0 "IRON"
    debug $ printf "iron = %s\n" ironAC

    let ctx :: Ctx
        ctx = ctx0
            { ctxGold = goldAC
            , ctxIron = ironAC
            }

    -- distribute tokens
    V.imapM_
      (\i userIbalance -> do
        when (isEmptyValue $ snd $ valueSplitAda userIbalance) $ do
          debug $ printf "User%s has no tokens, giving some\n" (show $ getUserIdx i)
          giveTokens ctx (snd $ userSkeyAddr V.! i)
      ) userBalances

    return $ Setup
        (closeLCIClient lci)
        (\putLog kont -> kont $ ctx { ctxLog = simpleConsoleLogging putLog })

-------------------------------------------------------------------------------
-- Generating users
-------------------------------------------------------------------------------

generateUser :: UserPaths -> IO (GYPaymentSigningKey, GYAddress)
generateUser UserPaths {..} =
    existing `catchIOException` const new
  where
    existing = do
        skey <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsPaymentKey) (Api.File pathUserSKey) >>=
          \case
          Right skey -> return $ paymentSigningKeyFromApi skey
          Left err   -> throwIO $ userError $ show err

        addr <- addressFromBech32 <$> urlPieceFromFile pathUserAddr
        return (skey, addr)

    new = do
        -- generate new key
        skey <- runAddressKeyGen pathUserSKey

        -- construct address (no stake)
        let vkey     = Api.getVerificationKey skey
            vkeyHash = Api.verificationKeyHash vkey

        let addr :: GYAddress
            addr = addressFromApi' $ Api.AddressInEra
                (Api.ShelleyAddressInEra Api.ShelleyBasedEraBabbage)
                (Api.makeShelleyAddress network (Api.PaymentCredentialByKey vkeyHash) stake)

        urlPieceToFile pathUserAddr (addressToBech32 addr)

        return (paymentSigningKeyFromApi skey, addr)

    network = networkIdToApi GYPrivnet
    stake   = Api.NoStakeAddress

runAddressKeyGen
    :: FilePath  -- ^ signing key path
    -> IO (Api.SigningKey Api.PaymentKey)
runAddressKeyGen skeyPath = do
      skey <- Api.generateSigningKey Api.AsPaymentKey
      res <- Api.writeFileTextEnvelope (Api.File skeyPath) (Just skeyDesc) skey
      case res of
          Right () -> return skey
          Left err -> do
              printf "Failed to write %s: %s\n" skeyPath (show err)
              exitFailure

  where
    skeyDesc :: Api.TextEnvelopeDescr
    skeyDesc = "Payment Signing Key"

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

giveAda :: Ctx -> GYAddress -> IO ()
giveAda ctx addr = do
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

mintTestTokens :: Paths -> Ctx -> String -> IO GYAssetClass
mintTestTokens Paths {pathGeniusYield} ctx tn' = do
    debug $ printf "Minting token %s\n" (show tn)
    existing `catchIOException` const new
  where
    tn :: GYTokenName
    tn = fromString tn'

    path :: FilePath
    path = pathGeniusYield </> tn'

    existing :: IO GYAssetClass
    existing = urlPieceFromFile path

    new :: IO GYAssetClass
    new = do
        (ac, txBody) <- ctxRunF ctx (ctxUserF ctx) $
            GY.TestTokens.mintTestTokens tn 10_000_000
        void $ submitTx ctx (ctxUserF ctx) txBody

        urlPieceToFile path ac

        return ac

catchIOException :: IO a -> (IOException -> IO a) -> IO a
catchIOException = catch
