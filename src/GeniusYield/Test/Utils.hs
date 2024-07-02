{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : GeniusYield.Test.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Utils
    ( Clb.Clb
    , mkTestFor
    , Wallets (..)
    , runWallet
    , runWallet'
    , walletAddress
    , walletPubKeyHash
    , balance
    , withBalance
    , withWalletBalancesCheck
    , withWalletBalancesCheckSimple
    , withWalletBalancesCheckSimpleIgnoreMinDepFor
    , getBalance
    , getBalances
    , waitUntilSlot
    , findLockedUtxosInBody
    , utxosInBody
    , addRefScript
    , addRefInput
    , fakeCoin, fakeGold, fakeIron
    , afterAllSucceed
    , feesFromLovelace
    , withMaxQCTests
    , pattern (:=)
    , logInfoS
    ) where

import           Control.Lens                     ((^.))
import           Control.Monad.Random
import           Control.Monad.State
import           Data.List                        (findIndex)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust)
import           Data.Semigroup                   (Sum (getSum))

import qualified Data.Maybe.Strict                as StrictMaybe
import qualified Data.Sequence.Strict             as StrictSeq
import qualified Data.Set                         as Set
import           Prettyprinter                    (PageWidth (AvailablePerLine),
                                                   defaultLayoutOptions,
                                                   layoutPageWidth,
                                                   layoutPretty)
import           Prettyprinter.Render.String      (renderString)

import qualified Cardano.Api                      as Api
import qualified Cardano.Api.Shelley              as Api.S
import qualified Cardano.Ledger.Address           as L
import qualified Cardano.Ledger.Api               as L
import qualified Cardano.Ledger.Babbage.Tx        as L.B
import qualified Cardano.Ledger.Babbage.TxOut     as L.B
import qualified Cardano.Ledger.Binary            as L
import qualified Cardano.Ledger.Plutus.TxInfo     as L.Plutus
import qualified Cardano.Ledger.Shelley.API       as L.S
import qualified Clb                              (Clb, ClbState (mockInfo),
                                                   ClbT, LogEntry (..),
                                                   LogLevel (..), MockConfig,
                                                   OnChainTx (getOnChainTx),
                                                   checkErrors, defaultBabbage,
                                                   initClb, intToKeyPair,
                                                   logInfo, ppLog, runClb,
                                                   waitSlot)
import qualified PlutusLedgerApi.V1.Value         as Plutus
import qualified PlutusLedgerApi.V2               as PlutusV2

import qualified Test.Cardano.Ledger.Core.KeyPair as TL

import qualified Test.Tasty                       as Tasty
import           Test.Tasty.HUnit                 (assertFailure, testCaseInfo)
import qualified Test.Tasty.QuickCheck            as Tasty
import qualified Test.Tasty.Runners               as Tasty

import           GeniusYield.Imports
import           GeniusYield.Test.Address
import           GeniusYield.Test.FakeCoin
import           GeniusYield.TxBuilder
import           GeniusYield.TxBuilder.Clb
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- tasty tools
-------------------------------------------------------------------------------

-- | Runs the second 'Tasty.TestTree' after all tests in the first 'Tasty.TestTree' succeed
afterAllSucceed :: Tasty.TestTree -> Tasty.TestTree -> Tasty.TestTree
afterAllSucceed = Tasty.after Tasty.AllSucceed . pat where
    pat :: Tasty.TestTree -> String
    pat dep = case dep of
        Tasty.SingleTest tn _        -> tn
        Tasty.TestGroup tn _         -> tn
        Tasty.After _ _ dep'         -> pat dep'
        Tasty.PlusTestOptions _ dep' -> pat dep'
        Tasty.WithResource _ f       -> pat (f (fail "Not running IO"))
        Tasty.AskOptions f           -> pat (f mempty)

-------------------------------------------------------------------------------
-- QC
-------------------------------------------------------------------------------

-- | Adjust the number of QuickCheck cases to generate.
withMaxQCTests :: Int -> Tasty.TestTree -> Tasty.TestTree
withMaxQCTests n = Tasty.adjustOption f where
    f :: Tasty.QuickCheckTests -> Tasty.QuickCheckTests
    f (Tasty.QuickCheckTests m) = Tasty.QuickCheckTests (min m n)

-------------------------------------------------------------------------------
-- test assets
-------------------------------------------------------------------------------

class    FromFakeCoin a                 where fromFakeCoin :: FakeCoin -> a
instance FromFakeCoin FakeCoin          where fromFakeCoin = id
instance FromFakeCoin GYAssetClass      where fromFakeCoin = fromRight (error "invalid asset class") . assetClassFromPlutus . fakeCoin
instance FromFakeCoin Plutus.AssetClass where fromFakeCoin = fakeCoin

-- | This allows to write e.g. @'fakeGold' 1000 :: GYValue@.
instance (a ~ Integer, b ~ GYValue) => FromFakeCoin (a -> b) where
    fromFakeCoin c = fromRight (error "invalid value") . valueFromPlutus . fakeValue c

-- | Fake \"Gold\" coin to use during tests.
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeGold :: FromFakeCoin a => a
fakeGold = fromFakeCoin $ FakeCoin "Gold"

-- | Fake \"Iron\" coin to use during tests
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeIron :: FromFakeCoin a => a
fakeIron = fromFakeCoin $ FakeCoin "Iron"

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

{- | Given a test name, runs the trace for every wallet, checking there weren't
     errors.
-}
mkTestFor :: String -> (Wallets -> GYTxMonadClb a) -> Tasty.TestTree
mkTestFor name action =
    testNoErrorsTraceClb v w Clb.defaultBabbage name $ do
      asClb pureGen (w1 wallets) $ action wallets
  where
    v = valueFromLovelace 1_000_000_000_000_000 <>
        fakeGold                  1_000_000_000 <>
        fakeIron                  1_000_000_000

    w = valueFromLovelace 1_000_000_000_000 <>
        fakeGold                  1_000_000 <>
        fakeIron                  1_000_000

    wallets :: Wallets
    wallets = Wallets (mkSimpleWallet "w1" (Clb.intToKeyPair 1))
                      (mkSimpleWallet "w2" (Clb.intToKeyPair 2))
                      (mkSimpleWallet "w3" (Clb.intToKeyPair 3))
                      (mkSimpleWallet "w4" (Clb.intToKeyPair 4))
                      (mkSimpleWallet "w5" (Clb.intToKeyPair 5))
                      (mkSimpleWallet "w6" (Clb.intToKeyPair 6))
                      (mkSimpleWallet "w7" (Clb.intToKeyPair 7))
                      (mkSimpleWallet "w8" (Clb.intToKeyPair 8))
                      (mkSimpleWallet "w9" (Clb.intToKeyPair 9))

    -- | Helper for building tests
    testNoErrorsTraceClb :: GYValue -> GYValue -> Clb.MockConfig -> String -> Clb.Clb a -> Tasty.TestTree
    testNoErrorsTraceClb funds walletFunds cfg msg act =
        testCaseInfo msg
            $ maybe (pure mockLog) assertFailure
            $ mbErrors >>= \errors -> pure (mockLog <> "\n\nError :\n-------\n" <>  errors)
        where
            -- _errors since we decided to store errors in the log as well.
            (mbErrors, mock) = Clb.runClb (act >> Clb.checkErrors) $ Clb.initClb cfg (valueToApi funds) (valueToApi walletFunds)
            mockLog = "\nEmulator log :\n--------------\n" <> logString
            options = defaultLayoutOptions { layoutPageWidth = AvailablePerLine 150 1.0}
            logDoc = Clb.ppLog $ Clb.mockInfo mock
            logString = renderString $ layoutPretty options logDoc


    mkSimpleWallet :: WalletName -> TL.KeyPair r L.StandardCrypto -> Wallet
    mkSimpleWallet n kp =
        Wallet
            { walletPaymentSigningKey = paymentSigningKeyFromLedgerKeyPair kp
            , walletNetworkId         = GYTestnetPreprod
            , walletName              = n
            }

-- | Available wallets.
data Wallets = Wallets
    { w1 :: !Wallet
    , w2 :: !Wallet
    , w3 :: !Wallet
    , w4 :: !Wallet
    , w5 :: !Wallet
    , w6 :: !Wallet
    , w7 :: !Wallet
    , w8 :: !Wallet
    , w9 :: !Wallet
    } deriving (Show, Eq, Ord)

-- | Runs a `GYTxMonadClb` action using the given wallet.
runWallet :: Wallet -> GYTxMonadClb a -> GYTxMonadClb (Maybe a)
runWallet w action = liftClb $ flip evalRandT pureGen $ asRandClb w action

-- | Version of `runWallet` that fails if `Nothing` is returned by the action.
runWallet' :: Wallet -> GYTxMonadClb a -> GYTxMonadClb a
runWallet' w action = do
    ma <- runWallet w action
    case ma of
        Nothing -> fail $ printf "Run wallet action returned Nothing"
        Just a  -> return a

-- | Gets a GYPubKeyHash of a testing wallet.
walletPubKeyHash :: Wallet -> GYPubKeyHash
walletPubKeyHash = fromJust . addressToPubKeyHash . walletAddress

{- | Gets the balance from anything that `HasAddress`. The usual case will be a
     testing wallet.
-}
balance :: HasAddress a => a -> GYTxMonadClb GYValue
balance a = do
    nid <- networkId
    case addressFromPlutus nid $ toAddress a of
        Left err   -> fail $ show err
        Right addr -> do
            utxos <- utxosAtAddress addr Nothing
            return $ foldMapUTxOs utxoValue utxos

{- | Computes a `GYTxMonadClb` action and returns the result and how this action
     changed the balance of some "Address".
-}
withBalance :: HasAddress a => String -> a -> GYTxMonadClb b -> GYTxMonadClb (b, GYValue)
withBalance n a m = do
    old <- balance a
    b   <- m
    new <- balance a
    let diff = new `valueMinus` old
    gyLogDebug' "" $ printf "%s:\nold balance: %s\nnew balance: %s\ndiff: %s" n old new diff
    return (b, diff)

{- | Computes a 'GYTxMonadClb' action, checking that the 'Wallet' balances
        change according to the input list.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheck :: [(Wallet, GYValue)] -> GYTxMonadClb a -> GYTxMonadClb a
withWalletBalancesCheck []            m = m
withWalletBalancesCheck ((w, v) : xs) m = do
    (b, diff) <- withBalance (walletName w) w $ withWalletBalancesCheck xs m
    unless (diff == v) $ do
        fail $ printf "expected balance difference of %s for wallet %s, but the actual difference was %s" v (walletName w) diff
    return b

{- | Computes a 'GYTxMonadClb' action, checking that the 'Wallet' balances
        change according to the input list. This is a simplified version of `withWalletBalancesCheck` where the input list need not consider lovelaces required for fees & to satisfy the min ada requirements as these are added automatically. It is therefore recommended to use this function over `withWalletBalancesCheck` to avoid hardcoding the lovelaces required for fees & min ada constraints.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheckSimple :: [(Wallet, GYValue)] -> GYTxMonadClb a -> GYTxMonadClb a
withWalletBalancesCheckSimple wallValueDiffs = withWalletBalancesCheckSimpleIgnoreMinDepFor wallValueDiffs mempty

-- | Variant of `withWalletBalancesCheckSimple` that only accounts for transaction fees and not minimum ada deposits.
withWalletBalancesCheckSimpleIgnoreMinDepFor :: [(Wallet, GYValue)] -> Set WalletName -> GYTxMonadClb a -> GYTxMonadClb a
withWalletBalancesCheckSimpleIgnoreMinDepFor wallValueDiffs ignoreMinDepFor m = do
  bs <- mapM (balance . fst) wallValueDiffs
  a <- m
  walletExtraLovelaceMap <- gets walletExtraLovelace
  bs' <- mapM (balance . fst) wallValueDiffs

  forM_ (zip3 wallValueDiffs bs' bs) $
    \((w, v), b', b) ->
      let wn = walletName w
          newBalance = case Map.lookup wn walletExtraLovelaceMap of
            Nothing -> b'
            Just (extraLovelaceForFees, extraLovelaceForMinAda) -> b' <> valueFromLovelace (getSum $ extraLovelaceForFees <> if Set.member wn ignoreMinDepFor then mempty else extraLovelaceForMinAda)
          diff = newBalance `valueMinus` b
        in unless (diff == v) $ fail $
            printf "Wallet: %s. Old balance: %s. New balance: %s. New balance after adding extra lovelaces %s. Expected balance difference of %s, but the actual difference was %s" (walletName w) b b' newBalance v diff
  return a


-- | Given a wallet returns its balance.
getBalance :: HasCallStack => Wallet -> GYTxMonadClb GYValue
getBalance w = fromJust <$> runWallet w (balance w)

-- | Given a list of wallets returns its balances.
getBalances :: HasCallStack => [Wallet] -> GYTxMonadClb [GYValue]
getBalances = mapM getBalance

{- | Returns the list of outputs of the transaction for the given address.
     Returns Nothing if it fails to decode an address contained in the
      transaction outputs.
-}
findLockedUtxosInBody :: Num a => GYAddress -> Clb.OnChainTx -> Maybe [a]
findLockedUtxosInBody addr tx =
  let
    os = getTxOutputs tx
    findAllMatches (_, [], acc) = Just acc
    findAllMatches (index, txOut : os', acc) =
        let txOutAddr = addressFromApi . Api.S.fromShelleyAddrToAny . either id L.decompactAddr $ L.B.getEitherAddrBabbageTxOut txOut
        in if txOutAddr == addr
            then findAllMatches (index + 1, os', index : acc)
            else findAllMatches (index + 1, os', acc)
  in
    findAllMatches (0, os, [])

-- | Given a transaction and the corresponding transaction id, gives the list of UTxOs generated by that body /provided they still exist/. This function is usually expected to be called immediately after the transaction's submission.
utxosInBody :: Clb.OnChainTx -> GYTxId -> GYTxMonadClb [Maybe GYUTxO]
utxosInBody tx txId = do
    let os = getTxOutputs tx
    mapM (\i -> utxoAtTxOutRef (txOutRefFromTuple (txId, fromInteger $ toInteger i))) [0 .. (length os - 1)]


-- | Adds the given script to the given address and returns the reference for it.
addRefScript :: GYAddress -> GYValidator 'PlutusV2 -> GYTxMonadClb (Maybe GYTxOutRef)
addRefScript addr script = do
    let script' = validatorToScript script
    (tx, txId) <- sendSkeleton' (mustHaveOutput (mkGYTxOutNoDatum addr mempty) { gyTxOutRefS = Just $ GYPlutusScript script' }) []

    let index = findIndex
            (\o ->
                let lsh = fmap (apiHashToPlutus . Api.ScriptHash) $ L.hashScript <$> (o ^. L.B.referenceScriptBabbageTxOutL)
                in lsh == StrictMaybe.SJust (scriptPlutusHash script')
            )
            $ getTxOutputs tx
    return $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< index

-- | Adds an input (whose datum we'll refer later) and returns the reference to it.
addRefInput:: Bool       -- ^ Whether to inline this datum?
           -> GYAddress  -- ^ Where to place this output?
           -> GYDatum    -- ^ Our datum.
           -> GYTxMonadClb (Maybe GYTxOutRef)
addRefInput toInline addr dat = do
    (tx, txId) <- sendSkeleton'
        (mustHaveOutput
            $ GYTxOut addr mempty (Just (dat, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing
        )
        []

    outputsWithResolvedDatums <- mapM
        (\o ->
            resolveDatumFromLedger $ o ^. L.B.datumBabbageTxOutL
        )
        $ getTxOutputs tx
    let mIndex = findIndex (\d -> Just dat == d) outputsWithResolvedDatums
    pure $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< mIndex

resolveDatumFromLedger :: (GYTxQueryMonad m, L.Era era) => L.Datum era -> m (Maybe GYDatum)
resolveDatumFromLedger (L.Datum d)      = pure
                                            . Just
                                            . datumFromPlutusData
                                            . PlutusV2.BuiltinData
                                            . L.getPlutusData
                                            $ L.binaryDataToData d
resolveDatumFromLedger (L.DatumHash dh) = lookupDatum . unsafeDatumHashFromPlutus $ L.Plutus.transDataHash dh
resolveDatumFromLedger L.NoDatum        = pure Nothing

-- TODO: Add to CLB upstream?
getTxOutputs :: Clb.OnChainTx -> [L.B.BabbageTxOut (L.BabbageEra L.StandardCrypto)]
getTxOutputs = fmap L.sizedValue
    . toList
    . StrictSeq.fromStrict
    . L.B.btbOutputs
    . L.B.body
    . L.S.extractTx
    . Clb.getOnChainTx

{- | Abstraction for explicitly building a Value representing the fees of a
     transaction.
-}
feesFromLovelace :: Integer -> GYValue
feesFromLovelace = valueFromLovelace

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- | Pattern to create pairs easily.
pattern (:=) :: x -> y -> (x, y)
pattern (:=) x y = (x, y)

infix 0 :=

-------------------------------------------------------------------------------
-- Preset StdGen
-------------------------------------------------------------------------------

pureGen :: StdGen
pureGen = mkStdGen 42

{- -----------------------------------------------------------------------------
  CLB
----------------------------------------------------------------------------- -}

-- | Waits until a certain 'GYSlot'.
-- Silently returns if the given slot is greater than the current slot.
waitUntilSlot :: GYSlot -> GYTxMonadClb ()
waitUntilSlot slot = liftClb $ Clb.waitSlot $ slotToApi slot

-- | Variant of `logInfo` from @Clb@ that logs a string with @Info@ severity.
logInfoS :: Monad m => String -> Clb.ClbT m ()
logInfoS s = Clb.logInfo $ Clb.LogEntry Clb.Info s
