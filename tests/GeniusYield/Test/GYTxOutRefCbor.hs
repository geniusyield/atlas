module GeniusYield.Test.GYTxOutRefCbor (
  gyTxOutRefCborTests,
) where

import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, arbitrary, counterexample, elements, forAllShrink, property, shrink, testProperty, (===))

import Data.Aeson (eitherDecode', encode)

import GeniusYield.Types (
  GYTxOutRef,
  GYTxOutRefCbor (GYTxOutRefCbor, getTxOutRefHex),
  txIdFromHex,
  txOutRefFromTuple,
  txOutRefToTuple,
 )

-- | Test group for the 'GYTxOutRefCbor' type
gyTxOutRefCborTests :: TestTree
gyTxOutRefCborTests = testGroup "GYTxOutRefCbor" basicTests

basicTests :: [TestTree]
basicTests =
  [ testProperty "Roundtrip GYTxOutRefCbor --> JSON --> GYTxOutRefCbor === id" $
      forAllShrink genGyTxOutRefCbor shrinkGyTxOutRefCbor $ \gyTxOutRefCbor ->
        let encodedGyTxOutRefCbor = encode gyTxOutRefCbor
         in counterexample ("JSON encoded GYTxOutRefCbor " ++ show encodedGyTxOutRefCbor) $
              case eitherDecode' (encode gyTxOutRefCbor) of
                Right decodedGyTxOutRefCbor ->
                  counterexample
                    ("JSON decoded of the encoded GYTxOutRefCbor " ++ show decodedGyTxOutRefCbor)
                    $
                    -- NOTE(jaredponn) January 31, 2025: it's a bit
                    -- weird that 'GYTxOutRefCbor' doesn't have an
                    -- 'Eq' instance, so we unwrap it and use the
                    -- underlying 'GYTxOutRef' 'Eq' instance.
                    getTxOutRefHex gyTxOutRefCbor === getTxOutRefHex decodedGyTxOutRefCbor
                Left _err -> property False
  ]

-- | Generator for 'GYTxOutRefCbor'
genGyTxOutRefCbor :: Gen GYTxOutRefCbor
genGyTxOutRefCbor = GYTxOutRefCbor <$> genGyTxOutRef

-- | Generator for 'GYTxOutRef'
genGyTxOutRef :: Gen GYTxOutRef
genGyTxOutRef = do
  txId <- fmap
    ( fromJust
        -- NOTE(jaredponn) January 31, 2025: this 'fromJust' is safe -- we
        -- know that TxIds are 32 bytes long, and we generate the strings
        -- s.t. they are always 32 bytes long
        . txIdFromHex
        . concat
    )
    $ replicateM 32
    $ do
      firstHexDigit <- elements $ ['0' .. '9'] ++ ['a' .. 'f']
      secondHexDigit <- elements $ ['0' .. '9'] ++ ['a' .. 'f']
      return [firstHexDigit, secondHexDigit]
  txIx <- arbitrary

  return $ txOutRefFromTuple (txId, txIx)

-- | Shrinks 'GYTxOutRefCbor' using the underlying 'shrinkGyTxOutRef'
shrinkGyTxOutRefCbor :: GYTxOutRefCbor -> [GYTxOutRefCbor]
shrinkGyTxOutRefCbor (GYTxOutRefCbor gyTxOutRef) =
  map GYTxOutRefCbor $ shrinkGyTxOutRef gyTxOutRef

-- | Shrinks 'GYTxOutRef'. This only shrinks the transaction index.
shrinkGyTxOutRef :: GYTxOutRef -> [GYTxOutRef]
shrinkGyTxOutRef gyTxOutRef =
  let (txId, txIx) = txOutRefToTuple gyTxOutRef
   in map (\shrunkTxIx -> txOutRefFromTuple (txId, shrunkTxIx)) $ shrink txIx
