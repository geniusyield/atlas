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

import qualified GeniusYield.Api.TestTokens           as GY.TestTokens
import           GeniusYield.Imports
import           GeniusYield.Providers.CardanoDbSync
import           GeniusYield.Providers.LiteChainIndex
import           GeniusYield.Providers.Node
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Options
import           GeniusYield.Test.Privnet.Paths
import           GeniusYield.Test.Privnet.Utils
import           GeniusYield.TxBuilder                hiding (getCollateral)
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
-- change me to debug setup code.
--debug = putStrLn
debug _ = return ()

makeSetup' :: DbSyncOpts -> FilePath -> IO Setup
makeSetup' DbSyncOpts {..} privnetPath = do
    -- init paths
    paths <- initPaths privnetPath

    -- read user address
    user1addr <- addressFromBech32 <$> urlPieceFromFile (pathUserAddr $ pathUser1 paths)
    debug $ printf "user1addr = %s\n" user1addr

    user1skey <- readPaymentSigningKey $ pathUserSKey $ pathUser1 paths
    debug $ printf "user1skey = %s\n" (show user1skey)
    debug $ printf "user1vkey = %s\n" (show $ paymentVerificationKey user1skey)
    debug $ printf "user1pkh  = %s\n" (show $ pubKeyHash $ paymentVerificationKey user1skey)

    -- Generate user 2, 3
    (user2skey, user2addr) <- generateUser $ pathUser2 paths
    debug $ printf "user2addr = %s\n" user2addr
    debug $ printf "user2skey = %s\n" (show user2skey)
    debug $ printf "user2vkey = %s\n" (show $ paymentVerificationKey user2skey)
    debug $ printf "user2pkh  = %s\n" (show $ pubKeyHash $ paymentVerificationKey user2skey)

    (user3skey, user3addr) <- generateUser $ pathUser3 paths
    debug $ printf "user3addr = %s\n" user3addr
    debug $ printf "user3skey = %s\n" (show user3skey)
    debug $ printf "user3vkey = %s\n" (show $ paymentVerificationKey user3skey)
    debug $ printf "user3pkh  = %s\n" (show $ pubKeyHash $ paymentVerificationKey user3skey)

    -- Further down we need local node connection
    let info :: Api.LocalNodeConnectInfo Api.CardanoMode
        info = Api.LocalNodeConnectInfo
            { Api.localConsensusModeParams = Api.CardanoModeParams $ Api.EpochSlots 500
            , Api.localNodeNetworkId       = networkIdToApi GYPrivnet
            , Api.localNodeSocketPath      = pathNodeSocket paths
            }

    -- ask current slot, so we know local node connection works
    slot <- nodeGetCurrentSlot info
    debug $ printf "currentSlot = %s\n" slot

    lci <- newLCIClient info []

    dbSync <- traverse openDbSyncConn dbSyncConnInfo

    -- select a collateral oref
    user1coll <- getCollateral (pathUser1 paths) info user1addr
    debug $ printf "user1coll = %s\n" user1coll

    let localLookupDatum :: GYLookupDatum
        localLookupDatum = case dbSync of
            Just dbSync' | dbSyncOptsLookupDatum -> dbSyncLookupDatum dbSync'
            _                                    -> lciLookupDatum lci

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
            { ctxEra             = era
            , ctxInfo            = info
            , ctxLCI             = lci
            , ctxDbSync          = dbSync
            , ctxUser1           = User user1skey user1addr user1coll
            , ctxUser2           = User user2skey user2addr user1coll -- collateral is temporarily wrong
            , ctxUser3           = User user3skey user3addr user1coll -- collateral is temporarily wrong
            , ctxGold            = GYLovelace -- temporarily
            , ctxIron            = GYLovelace -- temporarily
            , ctxLog             = noLogging
            , ctxLookupDatum     = localLookupDatum
            , ctxQueryUtxos      = localQueryUtxo
            , ctxGetParams       = localGetParams
            }

    user2balance <- ctxRunC ctx0 (ctxUser1 ctx0) $ queryBalance user2addr
    when (isEmptyValue user2balance) $ do
        debug $ printf "User2 balance is empty, giving some ada\n"
        giveAda ctx0 user2addr

    user3balance <- ctxRunC ctx0 (ctxUser1 ctx0) $ queryBalance user3addr
    when (isEmptyValue user3balance) $ do
        debug $ printf "User3 balance is empty, giving some ada\n"
        giveAda ctx0 user3addr

        -- we also give ada to itself to create some small utxos
        giveAda ctx0 user1addr

    -- user 2 and 3 collaterals
    user2coll <- getCollateral (pathUser2 paths) info user2addr
    debug $ printf "user2coll = %s\n" user2coll
    user3coll <- getCollateral (pathUser3 paths) info user3addr
    debug $ printf "user3coll = %s\n" user3coll

    -- mint test tokens
    goldAC <- mintTestTokens paths ctx0 "GOLD"
    debug $ printf "gold = %s\n" goldAC

    ironAC <- mintTestTokens paths ctx0 "IRON"
    debug $ printf "iron = %s\n" ironAC

    let ctx :: Ctx
        ctx = ctx0
            { ctxGold = goldAC
            , ctxIron = ironAC

            , ctxUser2     = User user2skey user2addr user2coll
            , ctxUser3     = User user3skey user3addr user3coll
            }

    -- distribute tokens
    when (isEmptyValue $ snd $ valueSplitAda user2balance) $ do
        debug $ printf "User2 has no tokens, giving some\n"
        giveTokens ctx user2addr
    when (isEmptyValue $ snd $ valueSplitAda user3balance) $ do
        debug $ printf "User3 has no tokens, giving some\n"
        giveTokens ctx user3addr

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
        skey <- Api.readFileTextEnvelope (Api.AsSigningKey Api.AsPaymentKey) pathUserSKey >>=
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
      res <- Api.writeFileTextEnvelope skeyPath (Just skeyDesc) skey
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
    txBody <- ctxRunI ctx (ctxUser1 ctx) $ return $ mconcat $ replicate 5 $
        mustHaveOutput $ mkGYTxOutNoDatum addr (valueFromLovelace 1_000_000_000)
    void $ submitTx ctx (ctxUser1 ctx) txBody

giveTokens :: Ctx -> GYAddress -> IO ()
giveTokens ctx addr = do
    txBody <- ctxRunI ctx (ctxUser1 ctx) $ return $
        mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxGold ctx) 1_000_000)) <>
        mustHaveOutput (mkGYTxOutNoDatum addr (valueSingleton (ctxIron ctx) 1_000_000))
    void $ submitTx ctx (ctxUser1 ctx) txBody

-------------------------------------------------------------------------------
-- Picking txoutref to be the collateral
-------------------------------------------------------------------------------

getCollateral :: UserPaths -> Api.LocalNodeConnectInfo Api.CardanoMode -> GYAddress -> IO GYTxOutRef
getCollateral UserPaths {pathUserColl} info addr = do
    userUtxos <- nodeUtxosAtAddress era info addr
    collateral <- urlPieceFromFileSafe userUtxos
    case utxosLookup collateral userUtxos of
        Just coll -> return $ utxoRef coll
        Nothing   -> new userUtxos
   where
    urlPieceFromFileSafe :: GYUTxOs -> IO GYTxOutRef
    urlPieceFromFileSafe utxos =
        urlPieceFromFile pathUserColl `catchIOException` const (new utxos)

    new :: GYUTxOs -> IO GYTxOutRef
    new utxos = do
        collateral <- getSomeTxOutRef utxos
        urlPieceToFile pathUserColl collateral
        return collateral

getSomeTxOutRef :: GYUTxOs -> IO GYTxOutRef
getSomeTxOutRef utxos = do
    let onlyAdaUtxos = filterUTxOs isOnlyAdaUtxo utxos
    case someTxOutRef onlyAdaUtxos of
        Nothing        -> die "getSomeTxOutRef: empty utxo"
        Just (oref, _) -> return oref
  where
    isOnlyAdaUtxo :: GYUTxO -> Bool
    isOnlyAdaUtxo utxo = valueTotalAssets (utxoValue utxo) == 1
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
        (ac, txBody) <- ctxRunF ctx (ctxUser1 ctx) $
            GY.TestTokens.mintTestTokens tn 5_000_000
        void $ submitTx ctx (ctxUser1 ctx) txBody

        urlPieceToFile path ac

        return ac

catchIOException :: IO a -> (IOException -> IO a) -> IO a
catchIOException = catch
