{-|
Module      : GeniusYield.CardanoApi.Query
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.CardanoApi.Query (
    -- * Low-level query runners
    queryCardanoMode,
    queryConwayEra,
    queryUTxO,
    -- * Exception
    CardanoQueryException (..),
) where

import           Control.Exception                               (Exception,
                                                                  throwIO)

import qualified Cardano.Api                                     as Api
import qualified Cardano.Api.Shelley                             as Api.S
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Ouroboros

import           GeniusYield.Types

-------------------------------------------------------------------------------
-- Exception
-------------------------------------------------------------------------------

newtype CardanoQueryException = CardanoQueryException String
  deriving stock (Show)
  deriving anyclass (Exception)

-------------------------------------------------------------------------------
-- Low-level query runners
-------------------------------------------------------------------------------

queryCardanoMode :: Api.LocalNodeConnectInfo -> Api.QueryInMode a -> IO a
queryCardanoMode info q = do
    e <- Api.runExceptT $ Api.queryNodeLocalState info Ouroboros.VolatileTip q
    case e of
        Left err -> throwIO $ CardanoQueryException $ show err
        Right x  -> return x

queryConwayEra :: Api.LocalNodeConnectInfo -> Api.QueryInShelleyBasedEra ApiEra a -> IO a
queryConwayEra info q = do
    e <- queryCardanoMode info $ Api.QueryInEra $ Api.QueryInShelleyBasedEra Api.ShelleyBasedEraConway q
    case e of
        Left err -> throwIO $ CardanoQueryException $ show err
        Right x  -> return x

queryUTxO :: Api.S.LocalNodeConnectInfo -> Api.QueryUTxOFilter -> IO GYUTxOs
queryUTxO info q = fmap utxosFromApi $ queryConwayEra info $ Api.QueryUTxO q
