{-|
Module      : GeniusYield.Test.Privnet.Asserts
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Asserts (
    assertBool,
    assertEqual,
    assertFailure,
    assertFee,
    assertThrown,
    assertUserFunds,
    isTxBodyErrorAutoBalance,
) where

import           Data.IORef        (newIORef, readIORef, writeIORef)
import           Data.Typeable     (typeRep)
import           Test.Tasty.HUnit  (assertBool, assertEqual, assertFailure)

import           GeniusYield.Imports
import           GeniusYield.Types
import           GeniusYield.Transaction
import           GeniusYield.Test.Privnet.Ctx

assertFee :: HasCallStack => GYTxBody -> Integer -> Integer -> IO ()
assertFee (txBodyFee -> fee) lb ub
    | fee < lb  = assertFailure $ printf "Fee: %d less than %d" fee lb
    | fee > ub  = assertFailure $ printf "Fee: %d greater than %d" fee ub
    | otherwise = return ()

assertThrown :: forall e a . Exception e => (e -> Bool) -> IO a -> IO ()
assertThrown p action = do
    thrownRef <- newIORef False
    void action `catch` \ e ->
        if p e
        then writeIORef thrownRef True
        else assertFailure $ "Exception doesn't match predicate: " ++ name

    thrown <- readIORef thrownRef
    unless thrown $ assertFailure $ "Expecting an exception: " ++ name
  where
    name = show (typeRep (Proxy @e))

{- Asserts if the user funds change as expected.
   The `fees` argument is an estimation for the transaction fees.
-}
assertUserFunds :: Integer -> Ctx -> UserIdx -> GYValue -> IO ()
assertUserFunds fees ctx uid expectedValue = do
    currentValue <- ctxQueryBalance ctx uid
    let (cLovelace, cAssets) = valueSplitAda currentValue
        (eLovelace, eAssets) = valueSplitAda expectedValue
    assertBool (unwords ["The non-Ada token didn't change as expected",
                         "\nExpected: ", show eAssets,
                         "\nCurrent: ", show cAssets])
               (cAssets == eAssets)
    assertBool (unwords ["The lovelaces didn't change as expected,",
                         "\nExpected: ", show eLovelace,
                         "\nCurrent: ", show cLovelace])
               ((cLovelace + fees >= eLovelace) &&
                (cLovelace < eLovelace))

isTxBodyErrorAutoBalance :: BuildTxException -> Bool
isTxBodyErrorAutoBalance (BuildTxBodyErrorAutoBalance _) = True
isTxBodyErrorAutoBalance _                               = False
