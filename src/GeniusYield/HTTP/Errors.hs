{-# LANGUAGE DefaultSignatures #-}

{- |
Module      : GeniusYield.HTTP.Errors
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.HTTP.Errors (
  IsGYApiError (..),
  GYApiError (..),
  someBackendError,
) where

import Control.Exception (displayException)
import Data.Text qualified as Txt

import Network.HTTP.Types (Status, status500)

import GeniusYield.Imports

-------------------------------------------------------------------------------
-- HTTP Api Errors
-------------------------------------------------------------------------------

-- | Class of types that can be converted into an HTTP API error.
type IsGYApiError :: Type -> Constraint
class IsGYApiError e where
  toApiError :: e -> GYApiError
  default toApiError :: (Exception e) => e -> GYApiError
  toApiError e = someBackendError . Txt.pack $ displayException e

{- | An example error code can be: "INSUFFICIENT_BALANCE" (i.e.
  it is not the HTTP status error message)

  The message can be any textual representation of the error with more information.

  The status code should be the HTTP status code.
-}
data GYApiError = GYApiError
  { gaeErrorCode :: Text
  , gaeHttpStatus :: Status
  , gaeMsg :: Text
  }
  deriving stock (Show, Eq)

instance Exception GYApiError

instance IsGYApiError GYApiError where
  toApiError = id

-- | Create a typical BACKEND_ERROR internal serval error with given message.
someBackendError :: Text -> GYApiError
someBackendError msg =
  GYApiError
    { gaeErrorCode = "BACKEND_ERROR"
    , gaeHttpStatus = status500
    , gaeMsg = msg
    }
