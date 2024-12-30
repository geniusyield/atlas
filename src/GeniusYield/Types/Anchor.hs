{- |
Module      : GeniusYield.Types.Anchor
Copyright   : (c) 2024 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Anchor (
  GYUrl,
  textToUrl,
  unsafeTextToUrl,
  urlToText,
  GYAnchorData,
  GYAnchorDataHash,
  hashAnchorData,
  GYAnchor (..),
  anchorToLedger,
  anchorFromLedger,
) where

import GeniusYield.Imports

import Cardano.Api.Ledger qualified as Ledger
import Control.Monad ((>=>))
import Data.ByteString.Char8 (ByteString)

{- | URL to a JSON payload of metadata. Note that we require URL to be at most 128 bytes.
>>> textToUrl "https://geniusyield.co"
GYUrl (Url {urlToText = "https://geniusyield.co"})
-}
newtype GYUrl = GYUrl Ledger.Url
  deriving stock Show
  deriving newtype (Eq, Ord)

-- | Convert a 'Text' to a 'GYUrl' checking that it is at most 128 bytes in the process.
textToUrl :: MonadFail m => Text -> m GYUrl
textToUrl = Ledger.textToUrl 128 >=> pure . coerce

unsafeTextToUrl :: Text -> GYUrl
unsafeTextToUrl t = fromMaybe (error "textToUrl: failed") $ textToUrl t

-- | Convert a 'GYUrl' to a 'Text'.
urlToText :: GYUrl -> Text
urlToText = Ledger.urlToText . coerce

-- | Anchor data.
type GYAnchorData = ByteString

{- | Hash of anchor data.
>>> hashAnchorData "Hello, World!"
GYAnchorDataHash (SafeHash "511bc81dde11180838c562c82bb35f3223f46061ebde4a955c27b3f489cf1e03")
-}
newtype GYAnchorDataHash = GYAnchorDataHash (Ledger.SafeHash Ledger.StandardCrypto Ledger.AnchorData)
  deriving stock Show
  deriving newtype (Eq, Ord)

-- | Hash anchor data.
hashAnchorData :: GYAnchorData -> GYAnchorDataHash
hashAnchorData = GYAnchorDataHash . Ledger.hashAnchorData . Ledger.AnchorData

-- | Anchor.
data GYAnchor = GYAnchor {anchorUrl :: !GYUrl, anchorDataHash :: !GYAnchorDataHash}
  deriving stock (Eq, Ord, Show)

anchorToLedger :: GYAnchor -> Ledger.Anchor Ledger.StandardCrypto
anchorToLedger GYAnchor {..} = Ledger.Anchor (coerce anchorUrl) (coerce anchorDataHash)

anchorFromLedger :: Ledger.Anchor Ledger.StandardCrypto -> GYAnchor
anchorFromLedger (Ledger.Anchor url hash) = GYAnchor (coerce url) (coerce hash)
