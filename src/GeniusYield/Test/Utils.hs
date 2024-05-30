{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : GeniusYield.Test.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Utils
    (
    mkTestFor
    , Wallets (..)
    , runWalletGYClb
    , walletPubKeyHash
    , balance
    , utxosInBody
    , findLockedUtxosInBody
    , withWalletBalanceCheck
    , withWalletBalancesCheckSimple
    , addRefScript
    , addRefInput
    , afterAllSucceed
    , feesFromLovelace
    , withMaxQCTests
    , pattern (:=)
    , fakeCoin
    , fakeGold
    , fakeIron
    ) where
import           Control.Monad.Random
import           Control.Monad.State
import Data.Semigroup (Sum(getSum))
import qualified Data.Map.Strict            as Map
import Control.Lens ((^.))
import           Data.Maybe                 (fromJust)
import qualified PlutusLedgerApi.V1.Value   as Plutus
import qualified PlutusLedgerApi.V2         as Plutus2
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.QuickCheck      as Tasty
import qualified Test.Tasty.Runners         as Tasty
import           Test.Tasty.HUnit (assertFailure, testCaseInfo)

import           GeniusYield.Imports
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import qualified Cardano.Api.Shelley as Api.S
import Cardano.Ledger.Address (decompactAddr)
import qualified Cardano.Ledger.Plutus.TxInfo as L.Plutus

import GeniusYield.Test.Address
import Clb qualified (
  Clb,
  ClbState (mockInfo),
  initClb,
  checkErrors,
  OnChainTx (getOnChainTx),
  MockConfig,
  ppLog,
  runClb,
  intToKeyPair,
  defaultBabbage,
  waitSlot,
 )
import qualified Cardano.Ledger.Api as L
import qualified Test.Cardano.Ledger.Core.KeyPair as TL
import Cardano.Ledger.Shelley.API (extractTx)
import Cardano.Ledger.Babbage.Tx (AlonzoTx(body), BabbageTxBody (btbOutputs))
import Cardano.Ledger.Babbage.TxOut (referenceScriptBabbageTxOutL, datumBabbageTxOutL, getEitherAddrBabbageTxOut)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Maybe.Strict as StrictMaybe
import qualified Cardano.Api                      as Api
import Cardano.Ledger.Binary (Sized(sizedValue))
import Test.Tasty (TestTree)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol)


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

newtype FakeCoin = FakeCoin {fakeCoin'tag :: Plutus2.BuiltinByteString}

fakeValue :: FakeCoin -> Integer -> Plutus2.Value
fakeValue tag = Plutus.assetClassValue (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> Plutus.AssetClass
fakeCoin (FakeCoin tag) = Plutus.assetClass sym tok
  where
    sym =
      Plutus2.CurrencySymbol $ Plutus2.toBuiltin $
        Api.serialiseToRawBytes $ Api.hashScript $ Api.PlutusScript Api.PlutusScriptV2
          $ Api.S.PlutusScriptSerialised $ Plutus2.serialiseCompiledCode $ fakeMintingPolicy tok
    tok = Plutus2.TokenName tag

fakeMintingPolicy :: Plutus2.TokenName -> PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
fakeMintingPolicy mintParam =
  $$(PlutusTx.compile [|| fakeMintingPolicyUntypedContract ||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 mintParam

-- | Can mint new coins if token name equals to fixed tag.
{-# INLINEABLE fakeMintingPolicyContract #-}
fakeMintingPolicyContract :: Plutus2.TokenName -> () -> Plutus2.ScriptContext -> Bool
fakeMintingPolicyContract tag _ ctx =
  Plutus.valueOf (Plutus2.txInfoMint (Plutus2.scriptContextTxInfo ctx)) (ownCurrencySymbol ctx) tag PlutusTx.> 0

-- | See `fakeMintingPolicyContract`.
{-# INLINEABLE fakeMintingPolicyUntypedContract #-}
fakeMintingPolicyUntypedContract :: Plutus2.TokenName -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
fakeMintingPolicyUntypedContract tag red ctx = PlutusTx.check (fakeMintingPolicyContract tag (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx))


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
      asClb pureGen TODO $ action wallets

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
    testNoErrorsTraceClb :: GYValue -> GYValue -> Clb.MockConfig -> String -> Clb.Clb a -> TestTree
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

runWalletGYClb :: Wallet -> GYTxMonadClb a -> GYTxMonadClb (Maybe a)
runWalletGYClb w action = liftClb $ flip evalRandT pureGen $ asRandClb w action

-- | Gets a GYPubKeyHash of a testing wallet.
walletPubKeyHash :: Wallet -> GYPubKeyHash
walletPubKeyHash = fromJust . addressToPubKeyHash . walletAddress

{- | Returns the list of outputs of the transaction for the given address.
     Returns Nothing if it fails to decode an address contained in the
      transaction outputs.
-}
findLockedUtxosInBody :: Num a => GYAddress -> Clb.OnChainTx -> Maybe [a]
findLockedUtxosInBody addr tx =
  let
    os = toList . StrictSeq.fromStrict . fmap sizedValue . btbOutputs . body . extractTx $ Clb.getOnChainTx tx
    findAllMatches (_    , []                             , acc) = Just acc
    findAllMatches (index, txOut : os', acc) = either
        (const Nothing)
        (\addr'' -> if addr'' == addr
            then findAllMatches (index + 1, os', index : acc)
            else findAllMatches (index + 1, os', acc))
        (addressFromApi . Api.S.fromShelleyAddrToAny . decompactAddr <$> getEitherAddrBabbageTxOut txOut)
  in
    findAllMatches (0, os, [])

-- | Given a transaction and the corresponding transaction id, gives the list of UTxOs generated by that body /provided they still exist/. This function is usually expected to be called immediately after the transaction's submission.
utxosInBody :: Clb.OnChainTx -> GYTxId -> GYTxMonadClb [Maybe GYUTxO]
utxosInBody tx txId = do
    let os = toList . StrictSeq.fromStrict . btbOutputs . body . extractTx $ Clb.getOnChainTx tx
    mapM (\i -> utxoAtTxOutRef (txOutRefFromTuple (txId, fromInteger $ toInteger i))) [0 .. (length os - 1)]

addRefScript :: GYAddress -> GYValidator 'PlutusV2 -> GYTxMonadClb (Maybe GYTxOutRef)
addRefScript addr script = do
    let script' = validatorToScript script
    (tx, txId) <- sendSkeleton' (mustHaveOutput (mkGYTxOutNoDatum addr mempty) { gyTxOutRefS = Just $ GYPlutusScript script' })
    let outputs = btbOutputs $ body $ extractTx $ Clb.getOnChainTx tx

    -- TODO: factor out
    let index = StrictSeq.findIndexL
            (\o ->
                let lsh = fmap (apiHashToPlutus . Api.ScriptHash) $ L.hashScript <$> (sizedValue o ^. referenceScriptBabbageTxOutL)
                in lsh == StrictMaybe.SJust (scriptPlutusHash script')
            )
            outputs
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
    let outputs = btbOutputs $ body $ extractTx $ Clb.getOnChainTx tx

    outputsWithResolvedDatums <- mapM
            (\o ->
                resolveDatumFromLedger $ sizedValue o ^. datumBabbageTxOutL
            )
            outputs
    let mIndex = StrictSeq.findIndexL (\d -> Just dat == d) outputsWithResolvedDatums
    pure $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< mIndex

resolveDatumFromLedger :: (GYTxQueryMonad m, L.Era era) => L.Datum era -> m (Maybe GYDatum)
resolveDatumFromLedger (L.Datum d)      = pure . Just . datumFromPlutusData . Plutus2.BuiltinData . L.getPlutusData $ L.binaryDataToData d
resolveDatumFromLedger (L.DatumHash dh) = lookupDatum . unsafeDatumHashFromPlutus $ L.Plutus.transDataHash dh
resolveDatumFromLedger L.NoDatum        = pure Nothing

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

balance :: HasAddress a => a -> GYTxMonadClb GYValue
balance a = do
    nid <- networkId
    case addressFromPlutus nid $ toAddress a of
        Left err   -> fail $ show err
        Right addr -> do
            utxos <- utxosAtAddress addr Nothing
            return $ foldMapUTxOs utxoValue utxos

withBalance :: HasAddress a => String -> a -> GYTxMonadClb b -> GYTxMonadClb (b, GYValue)
withBalance n a m = do
    old <- balance a
    b   <- m
    new <- balance a
    let diff = new `valueMinus` old
    gyLogDebug' "" $ printf "%s:\nold balance: %s\nnew balance: %s\ndiff: %s" n old new diff
    return (b, diff)

withWalletBalanceCheck :: [(Wallet, GYValue)] -> GYTxMonadClb a -> GYTxMonadClb a
withWalletBalanceCheck []            m = m
withWalletBalanceCheck ((w, v) : xs) m = do
    (b, diff) <- withBalance (walletName w) w $ withWalletBalanceCheck xs m
    unless (diff == v) $ do
        fail $ printf "expected balance difference of %s for wallet %s, but the actual difference was %s" v (walletName w) diff
    return b

{- | Computes a 'GYTxMonadRun' action, checking that the 'Wallet' balances
        change according to the input list. This is a simplified version of `withWalletBalancesCheck` where the input list need not consider lovelaces required for fees & to satisfy the min ada requirements as these are added automatically. It is therefore recommended to use this function over `withWalletBalancesCheck` to avoid hardcoding the lovelaces required for fees & min ada constraints.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheckSimple :: [(Wallet, GYValue)] -> GYTxMonadClb a -> GYTxMonadClb a
withWalletBalancesCheckSimple wallValueDiffs m = do
  bs <- mapM (balance . fst) wallValueDiffs
  a <- m
  walletExtraLovelaceMap <- gets walletExtraLovelace
  bs' <- mapM (balance . fst) wallValueDiffs

  forM_ (zip3 wallValueDiffs bs' bs) $
    \((w, v), b', b) ->
      let newBalance = case Map.lookup (walletName w) walletExtraLovelaceMap of
            Nothing -> b'
            Just (extraLovelaceForFees, extraLovelaceForMinAda) -> b' <> valueFromLovelace (getSum $ extraLovelaceForFees <> extraLovelaceForMinAda)
          diff = newBalance `valueMinus` b
        in unless (diff == v) $ fail $
            printf "Wallet: %s. Old balance: %s. New balance: %s. New balance after adding extra lovelaces %s. Expected balance difference of %s, but the actual difference was %s" (walletName w) b b' newBalance v diff
  return a

-- | Waits until a certain 'GYSlot'.
-- Silently returns if the given slot is greater than the current slot.
waitUntilSlot :: GYSlot -> GYTxMonadClb ()
waitUntilSlot slot = liftClb $ Clb.waitSlot $ slotToApi slot
