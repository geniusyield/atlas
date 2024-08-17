{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : GeniusYield.Test.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Utils
    ( TestInfo (..)
    , Wallets (..)
    , createUserWithLovelace
    , createUserWithAssets
    , createUserFull
    , withBalance
    , withWalletBalancesCheck
    , findLockedUtxosInBody
    , getRefInfos
    , findRefScriptsInBody
    , addRefScript
    , addRefInput
    , mintTestAssets
    , generateCollateral
    , fakeValue, fakeCoin, fakeGold, fakeIron, fakePolicy
    , afterAllSucceed
    , feesFromLovelace
    , withMaxQCTests
    , pattern (:=)
    , module X
    ) where

import           Control.Monad.Except        (ExceptT, runExceptT)
import           Control.Monad.Random
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as T

import qualified Test.Tasty                  as Tasty
import qualified Test.Tasty.QuickCheck       as Tasty
import qualified Test.Tasty.Runners          as Tasty

import           GeniusYield.HTTP.Errors
import           GeniusYield.Imports
import           GeniusYield.Test.FakeCoin
import           GeniusYield.TxBuilder
import           GeniusYield.Types

import           GeniusYield.Test.FeeTracker as X

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

-- | Fake \"Gold\" coin to use during tests.
fakeGold :: FakeCoin
fakeGold = FakeCoin "Gold"

-- | Fake \"Iron\" coin to use during tests
fakeIron :: FakeCoin
fakeIron = FakeCoin "Iron"

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

{- Note [simplify-genesis]

Currently, both our testing providers (privnet and CLB) generate users during test setup. Previously,
this was necessary since there was no obvious way to generate new users with funds etc. However, with the
addition of unified machinery to create new users and fund them, it should be left to the tests to create their
own users and fund them however they like.

This will vastly simplify the genesis code and remove problematically dynamic (yet static) types like 'Wallets', 'TestInfo'
and the user fields in "GeniusYield.Test.Privnet.Ctx".

TL;DR: Remove all user creation code from test setups and point Atlas users to use functions like 'createUser', 'createUserWithAssets',
'createUserFull' etc.
-}

-- TODO (simplify-genesis): Remove 'TestInfo'. The only thing test setup should do is to make one or more genesis/funder user(s)
-- and pass that 'User' onto the tests.
-- | General information about the test environment to help in running polymorphic tests.
data TestInfo = TestInfo { testGoldAsset :: !GYAssetClass, testIronAsset :: !GYAssetClass, testWallets :: !Wallets }

-- TODO (simplify-genesis): Remove this type once user creation logic is removed from test setup.
-- | Available wallets.
data Wallets = Wallets
    { w1 :: !User
    , w2 :: !User
    , w3 :: !User
    , w4 :: !User
    , w5 :: !User
    , w6 :: !User
    , w7 :: !User
    , w8 :: !User
    , w9 :: !User
    } deriving (Show, Eq, Ord)

-- | Create an user and fund them with the given amount of lovelace provided by the given funder user.
createUserWithLovelace :: GYTxGameMonad m => User -> Natural -> m User
createUserWithLovelace funder lovelace = do
    u <- createUser
    asUser funder $ do
        -- Fragment the lovelace amount into at least 5 utxos.
        let utxosCount = 5
            eachUtxo = toInteger $ lovelace `quot` utxosCount
            extraUtxo = toInteger $ lovelace `rem` utxosCount
            mustHaveLovelace 0 = mempty
            mustHaveLovelace l = mustHaveOutput $ mkGYTxOutNoDatum (userChangeAddress u) (valueFromLovelace l)
        txBody <- buildTxBody $ mconcat $ mustHaveLovelace extraUtxo : replicate 5 (mustHaveLovelace eachUtxo)
        signAndSubmitConfirmed_ txBody
    pure u

{- | `createUserWithAssets funder lovelaces tokens` is equivalent to
  `createUserWithLovelace funder lovelace', followed by 'mintTestAssets tokens'
  as the newly created user.

  Note: This will obviously require the user to have enough lovelace to cover the fees
  and min ada deposits for the mints.
-}
createUserWithAssets :: GYTxGameMonad m => User -> Natural -> [(FakeCoin, Natural)] -> m User
createUserWithAssets funder lovelace tokens = do
    user <- createUserWithLovelace funder lovelace
    asUser user $ mintTestAssets tokens
    pure user

-- | Create a collateral utxo out of the existing ada within a user wallet. Returns the collateral reference.
generateCollateral :: GYTxMonad m => m GYTxOutRef
generateCollateral = do
    addr <- ownChangeAddress
    txBody <- buildTxBody $ mustHaveOutput (mkGYTxOutNoDatum addr collateralValue)
    txId <- signAndSubmitConfirmed txBody
    pure $ txOutRefFromTuple (txId, 0)

-- | This is a combination of 'createUserWithAssets' and 'generateCollateral'.
-- It creates a user with ada, non-ada assets, and a collateral.
-- Thereby making a user ready to participate in smart contracts.
createUserFull :: GYTxGameMonad m => User -> Natural -> [(FakeCoin, Natural)] -> m User
createUserFull funder lovelace tokens = do
    user <- createUserWithAssets funder lovelace tokens
    userCollateralRef <- asUser user generateCollateral
    pure user { userCollateral = Just UserCollateral { userCollateralRef, userCollateralCheck = True } }

-- | Mint given amount of test tokens.
mintTestAssets :: GYTxMonad m => [(FakeCoin, Natural)] -> m ()
mintTestAssets tokens = do
    txBody <- buildTxBody @PlutusV2 $ foldMap
        (\(tk, amt) ->
            mustMint (GYMintScript $ fakePolicy tk) unitRedeemer (fakeCoinName tk) $ toInteger amt
        )
        tokens
    signAndSubmitConfirmed_ txBody

{- | Computes a `GYTx*Monad` action and returns the result and how this action
     changed the balance of some "Address".
-}
withBalance :: GYTxQueryMonad m => String -> User -> m b -> m (b, GYValue)
withBalance n a m = do
    old <- queryBalance $ userAddr a
    b   <- m
    new <- queryBalance $ userAddr a
    let diff = new `valueMinus` old
    gyLogDebug' "" $ printf "%s:\nold balance: %s\nnew balance: %s\ndiff: %s" n old new diff
    return (b, diff)

{- | Computes a `GYTx*Monad` action, checking that the 'Wallet' balances
        change according to the input list.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheck :: GYTxQueryMonad m => [(User, GYValue)] -> m a -> m a
withWalletBalancesCheck []            m = m
withWalletBalancesCheck ((w, v) : xs) m = do
    (b, diff) <- withBalance (show $ userAddr w) w $ withWalletBalancesCheck xs m
    unless (diff == v) $ do
        throwAppError . someBackendError . T.pack $ printf "expected balance difference of %s for wallet %s, but the actual difference was %s" v (userAddr w) diff
    return b

{- | Returns the list of outputs of the transaction for the given address.
     Returns Nothing if it fails to decode an address contained in the
      transaction outputs.
-}
findLockedUtxosInBody :: Num a => GYAddress -> GYTx -> Maybe [a]
findLockedUtxosInBody addr tx =
  let
    os = utxosToList . txBodyUTxOs $ getTxBody tx
    findAllMatches (_, [], acc) = Just acc
    findAllMatches (index, txOut : os', acc) = if utxoAddress txOut == addr
        then findAllMatches (index + 1, os', index : acc)
        else findAllMatches (index + 1, os', acc)
  in
    findAllMatches (0, os, [])

-- | Find reference scripts at given address.
getRefInfos :: GYTxQueryMonad m => GYAddress -> m (Map GYAnyScript GYTxOutRef)
getRefInfos addr = do
    utxo <- utxosAtAddress addr Nothing
    return $ utxoToRefMap utxo

utxoToRefMap :: GYUTxOs ->  Map GYAnyScript GYTxOutRef
utxoToRefMap utxo = Map.fromList
    [ (sc, ref)
    | GYUTxO { utxoRef = ref, utxoRefScript = Just sc} <- utxosToList utxo
    ]

-- | Find reference scripts in transaction body.
findRefScriptsInBody :: GYTxBody -> Map GYAnyScript GYTxOutRef
findRefScriptsInBody body = do
    let utxo = txBodyUTxOs body
    utxoToRefMap utxo

-- | Adds the given script to the given address and returns the reference for it.
-- Note: The new utxo is given an inline unit datum.
addRefScript :: forall m. GYTxMonad m => GYAddress -> GYScript 'PlutusV2 -> m GYTxOutRef
addRefScript addr sc = throwAppError absurdError `runEagerT` do
    existingUtxos <- lift $ utxosAtAddress addr Nothing
    let refs = utxoToRefMap existingUtxos
    maybeToEager $ Map.lookup (GYPlutusScript sc) refs
    txBody <- lift $ buildTxBody
        $ mustHaveOutput @'PlutusV2 GYTxOut
            { gyTxOutAddress     = addr
            , gyTxOutValue       = mempty
            , gyTxOutDatum       = Just (unitDatum, GYTxOutUseInlineDatum)
            , gyTxOutRefS        = Just $ GYPlutusScript sc
            }
    lift $ signAndSubmitConfirmed_ txBody
    maybeToEager . Map.lookup (GYPlutusScript sc) $ findRefScriptsInBody txBody
  where
    absurdError = someBackendError "Shouldn't happen: no ref in body"

-- | Adds an input (whose datum we'll refer later) and returns the reference to it.
addRefInput :: GYTxMonad m
            => Bool       -- ^ Whether to inline this datum?
            -> GYAddress  -- ^ Where to place this output?
            -> GYDatum    -- ^ Our datum.
            -> m GYTxOutRef
addRefInput toInline addr dat = throwAppError absurdError `runEagerT` do
    existingUtxos <- lift $ utxosAtAddress addr Nothing
    maybeToEager $ findRefWithDatum existingUtxos
    txBody <- lift . buildTxBody .
        mustHaveOutput @'PlutusV2
            $ GYTxOut addr mempty (Just (dat, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing

    lift $ signAndSubmitConfirmed_ txBody
    maybeToEager . findRefWithDatum $ txBodyUTxOs txBody
  where
    findRefWithDatum :: GYUTxOs -> Maybe GYTxOutRef
    findRefWithDatum utxos = fmap utxoRef
        . find
            (\GYUTxO {utxoOutDatum} ->
                case utxoOutDatum of
                    GYOutDatumHash dh     -> hashDatum dat == dh
                    GYOutDatumInline dat' -> dat == dat'
                    _                     -> False
            )
        $ utxosToList utxos
    absurdError = someBackendError "Shouldn't happen: no output with expected datum in body"

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

{- | Utilizing 'ExceptT' as a "eager monad" transformer.

'Left' does not indicate failure, rather it indicates that "target value has been obtained"
and that we can exit eagerly.
-}
type EagerT m a = ExceptT a m ()

-- | If we have a 'Just' value, we can exit with it immediately. So it gets converted
-- to 'Left'.
maybeToEager :: Monad m => Maybe a -> EagerT m a
maybeToEager (Just a) = throwError a
maybeToEager Nothing  = pure ()

-- If all goes well, we should finish with a 'Left'. if not, we perform the
-- given action to signal error.
runEagerT :: Monad m => m a -> ExceptT a m () -> m a
runEagerT whenError = runExceptT >=> either pure (const whenError)
