{- |
Module      : GeniusYield.Types.Anchor
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Anchor (
  Url,
  textToUrl,
  AnchorData,
  hashAnchorData,
  GYAnchor,
  mkAnchor,
  anchorUrl,
  anchorDataHash,
) where

import GeniusYield.Imports

import Cardano.Api.Ledger qualified as Ledger
import Data.ByteString.Char8 (ByteString)

-- | URL to a JSON payload of metadata. Note that we require URL to be at most 128 bytes.
type Url = Ledger.Url

-- | Convert a 'Text' to a 'Url' checking that it is at most 128 bytes in the process.
textToUrl :: MonadFail m => Text -> m Url
textToUrl = Ledger.textToUrl 128

-- | Anchor data.
type AnchorData = ByteString

-- | Hash anchor data.
hashAnchorData :: AnchorData -> Ledger.SafeHash Ledger.StandardCrypto Ledger.AnchorData
hashAnchorData = Ledger.hashAnchorData . Ledger.AnchorData

-- | Anchor.
type GYAnchor = Ledger.Anchor Ledger.StandardCrypto

-- | Make an anchor.
mkAnchor :: Url -> Ledger.SafeHash Ledger.StandardCrypto Ledger.AnchorData -> GYAnchor
mkAnchor = Ledger.Anchor

anchorUrl :: GYAnchor -> Url
anchorUrl = Ledger.anchorUrl

anchorDataHash :: GYAnchor -> Ledger.SafeHash Ledger.StandardCrypto Ledger.AnchorData
anchorDataHash = Ledger.anchorDataHash