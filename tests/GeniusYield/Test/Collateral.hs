{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module GeniusYield.Test.Collateral
    ( collateralTests
    ) where

import qualified Plutus.Model                   as Model
import           Test.QuickCheck.Instances.Time ()
import           Test.Tasty                     (TestTree, testGroup)
import           Text.Printf                    (printf)

import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types

collateralTests :: TestTree
collateralTests = testGroup "Collateral"
  [ testRun "collateral UTxO not spent for transactions not needing collateral" collateralTrace1
  , testRun "transferring total balance minus collateral should work" collateralTrace2
  ]

collateralTrace1 :: Wallets -> Run ()
collateralTrace1 ws@Wallets{..} = collateralTraceCore ws 4_000_000 $ expectInsufficientFunds w1

collateralTrace2 :: Wallets -> Run ()
collateralTrace2 ws@Wallets{..} = collateralTraceCore ws 9_000_000 $ \skeleton -> do
    mtid <- runWallet w1 $ sendSkeleton skeleton
    case mtid of
        Nothing  -> Model.logError "transaction failed unexpectedly"
        Just tid -> Model.logInfo $ printf "transaction successfully submitted: %s" tid

collateralTraceCore :: Wallets -> Integer -> (GYTxSkeleton PlutusV2 -> Run ()) -> Run ()
collateralTraceCore Wallets{..} diff cont = do
    m <- runWallet w1 $ do
        l <- flip valueAssetClass GYLovelace <$> balance w1 -- how many lovelace does wallet w1 hold?
        return $ mustHaveOutput $ GYTxOut
                { gyTxOutAddress = walletAddress w2
                , gyTxOutValue   = valueFromLovelace $ l - diff
                , gyTxOutDatum   = Nothing
                , gyTxOutRefS    = Nothing
                }
    case m of
        Nothing       -> Model.logError "couldn't construct skeleton"
        Just skeleton -> cont skeleton
