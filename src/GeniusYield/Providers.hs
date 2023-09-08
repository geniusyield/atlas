{-|
Module      : GeniusYield.Providers
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers
    ( module X
    , simpleProviders
    ) where

import           Data.Text                              (Text)

import           GeniusYield.Providers.Blockfrost       as X
import           GeniusYield.Providers.CachedQueryUTxOs as X
import           GeniusYield.Providers.CardanoDbSync    as X
import           GeniusYield.Providers.Katip            as X
import           GeniusYield.Providers.Maestro          as X
import           GeniusYield.Providers.Node             as X
import           GeniusYield.Providers.SubmitApi        as X
import           GeniusYield.Types

-- TODO: Delete the following `simpleProviders` method?
-- | Creates simples providers using a local node with Maestro API key.
--
simpleProviders :: GYEra                -- ^ Era in which local node operates
                -> GYNetworkId          -- ^ The network identifier.
                -> FilePath             -- ^ Path to the local node socket.
                -> Text                 -- ^ The Maestro Api key
                -> GYLog                -- ^ The logging provider.
                -> IO GYProviders
simpleProviders era nid nodeSocket mKey logging = do
    let info  = networkIdToLocalNodeConnectInfo nid nodeSocket
    mEnv <- X.networkIdToMaestroEnv mKey nid

    return GYProviders
      { gyLookupDatum      = maestroLookupDatum mEnv
      , gyAwaitTxConfirmed = maestroAwaitTxConfirmed mEnv
      , gySubmitTx         = nodeSubmitTx info
      , gySlotActions      = nodeSlotActions info
      , gyGetParameters    = nodeGetParameters era info
      , gyQueryUTxO        = nodeQueryUTxO era info
      , gyLog'             = logging
      }
