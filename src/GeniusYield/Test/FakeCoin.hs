{-# LANGUAGE TemplateHaskell #-}

module GeniusYield.Test.FakeCoin (FakeCoin (..), fakeValue, fakeCoin, fakePolicy) where

import Data.FileEmbed
import GeniusYield.Types

-- | Test assets.
newtype FakeCoin = FakeCoin {fakeCoinName :: GYTokenName}

fakePolicy :: FakeCoin -> GYScript PlutusV2
fakePolicy = fakeMintingPolicy . fakeCoinName

fakeValue :: FakeCoin -> Integer -> GYValue
fakeValue tag = valueSingleton (fakeCoin tag)

-- | Fake coin class generated from fixed tag.
fakeCoin :: FakeCoin -> GYAssetClass
fakeCoin (FakeCoin tag) = mintingPolicyId (fakeMintingPolicy tag) `GYToken` tag

fakeMintingPolicy :: GYTokenName -> GYScript PlutusV2
fakeMintingPolicy tn =
  let fileBS = $(makeRelativeToProject "./plutus/data/compiled-scripts/fake-coin.bp" >>= embedFile)
   in case extractBlueprintValidator fileBS of
        Nothing -> error "unable to read fake coin"
        Just s -> applyParam s (tokenNameToPlutus tn)