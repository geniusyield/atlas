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
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

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

-- | Asserts if the user funds change as expected. This function subtracts fees from the given expected value.
assertUserFunds :: Integer -> Ctx -> User -> GYValue -> IO ()
assertUserFunds fees ctx u expectedValue = do
    currentValue <- ctxQueryBalance ctx u
    let expectedValue' = expectedValue `valueMinus` valueFromLovelace fees
    assertBool (unwords ["The value didn't change as expected",
                         "\nExpected: ", show expectedValue',
                         "\nCurrent: ", show currentValue])
               (currentValue == expectedValue')

isTxBodyErrorAutoBalance :: GYTxMonadException -> Bool
isTxBodyErrorAutoBalance (GYBuildTxException (GYBuildTxBodyErrorAutoBalance _)) = True
isTxBodyErrorAutoBalance _                                                      = False
