{-|
Module      : GeniusYield.Test.Privnet.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Utils (
    die,
    urlPieceFromFile,
    urlPieceToFile,
    urlPieceFromText,
) where

import           Data.Text       (Text)
import           System.Exit     (exitFailure)
import           Text.Printf     (printf)
import           Type.Reflection (Typeable, typeRep)

import qualified Data.Text.IO    as T.IO
import qualified Web.HttpApiData as Web

die :: String -> IO a
die msg = do
    putStrLn msg
    exitFailure

urlPieceFromFile :: forall a. (Typeable a, Web.FromHttpApiData a) => FilePath -> IO a
urlPieceFromFile p = do
    t <- T.IO.readFile p
    urlPieceFromText @a t

urlPieceFromText :: forall a. (Typeable a, Web.FromHttpApiData a) => Text -> IO a
urlPieceFromText t = case Web.parseUrlPiece t of
    Right x ->
        return x

    Left msg -> do
        printf "Failed to parse %s from %s: %s\n" (show (typeRep @a)) t msg
        exitFailure

urlPieceToFile :: forall a. Web.ToHttpApiData a => FilePath -> a -> IO ()
urlPieceToFile p x = T.IO.writeFile p (Web.toUrlPiece x)
