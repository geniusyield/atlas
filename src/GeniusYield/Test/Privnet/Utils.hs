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
    readFileTextEnvelope,
) where

import           System.Exit     (exitFailure)
import           Text.Printf     (printf)
import           Type.Reflection (Typeable, typeRep)

import qualified Cardano.Api     as Api
import qualified Data.Text.IO    as T.IO
import qualified Web.HttpApiData as Web

die :: String -> IO a
die msg = do
    putStrLn msg
    exitFailure

urlPieceFromFile :: forall a. (Typeable a, Web.FromHttpApiData a) => FilePath -> IO a
urlPieceFromFile p = do
    t <- T.IO.readFile p
    case Web.parseUrlPiece t of
        Right x ->
            return x

        Left msg -> do
            printf "Failed to parse %s from %s: %s\n" (show (typeRep @a)) p msg
            exitFailure

urlPieceToFile :: forall a. Web.ToHttpApiData a => FilePath -> a -> IO ()
urlPieceToFile p x = T.IO.writeFile p (Web.toUrlPiece x)

readFileTextEnvelope :: forall a. (Api.HasTextEnvelope a, Typeable a) => Api.AsType a -> FilePath -> IO a
readFileTextEnvelope asType p = do
    res <- Api.readFileTextEnvelope asType p
    case res of
        Right x ->
            return x

        Left err -> do
            printf "Failed to parse %s from %s: %s" (show (typeRep @a)) p (Api.displayError err)
            exitFailure
