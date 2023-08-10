{-|
Module      : GeniusYield.Test.Privnet.Options
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Options where

import           Data.Proxy                     (Proxy (..))
import           Data.Tagged                    (Tagged (..))
import           Test.Tasty                     (TestTree, askOption)
import           Test.Tasty.Options             (IsOption (..), OptionDescription (..), safeReadBool)

import qualified Database.PostgreSQL.Simple     as PQ
import           Database.PostgreSQL.Simple.URL (parseDatabaseUrl)

optionDescriptions :: [OptionDescription]
optionDescriptions =
    [ Option (Proxy :: Proxy DbSyncConnInfo)
    , Option (Proxy :: Proxy DbSyncLookupDatum)
    , Option (Proxy :: Proxy DbSyncAwaitTx)
    , Option (Proxy :: Proxy DbSyncQueryUtxos)
    , Option (Proxy :: Proxy DbSyncGetParameters)
    ]

data DbSyncOpts = DbSyncOpts
    { dbSyncConnInfo          :: Maybe PQ.ConnectInfo
    , dbSyncOptsLookupDatum   :: Bool
    , dbSyncOptsAwaitTx       :: Bool
    , dbSyncOptsQueryUtxos    :: Bool
    , dbSyncOptsGetParameters :: Bool
    }

askDbSyncOpts :: (DbSyncOpts -> TestTree) -> TestTree
askDbSyncOpts kont =
    askOption $ \(DbSyncConnInfo dbSyncConnInfo) ->
    askOption $ \(DbSyncLookupDatum dbSyncOptsLookupDatum) ->
    askOption $ \(DbSyncGetParameters dbSyncOptsAwaitTx) ->
    askOption $ \(DbSyncQueryUtxos dbSyncOptsQueryUtxos) ->
    askOption $ \(DbSyncGetParameters dbSyncOptsGetParameters) ->
    kont DbSyncOpts {..}

newtype DbSyncConnInfo = DbSyncConnInfo (Maybe PQ.ConnectInfo)

instance IsOption DbSyncConnInfo where
    defaultValue = DbSyncConnInfo Nothing
    parseValue s = DbSyncConnInfo . Just <$> parseDatabaseUrl s
    optionName   = Tagged "db-sync"
    optionHelp   = Tagged "PostgreSQL URL of cardano-db-sync database"

newtype DbSyncLookupDatum = DbSyncLookupDatum Bool

instance IsOption DbSyncLookupDatum where
    defaultValue = DbSyncLookupDatum False
    parseValue s = DbSyncLookupDatum <$> safeReadBool s
    optionName   = Tagged "db-sync-lookup-datum"
    optionHelp   = Tagged "Use cardano-db-sync for datum lookup"

newtype DbSyncAwaitTx = DbSyncAwaitTx Bool

instance IsOption DbSyncAwaitTx where
    defaultValue = DbSyncAwaitTx False
    parseValue s = DbSyncAwaitTx <$> safeReadBool s
    optionName   = Tagged "db-sync-await-tx-confirmed"
    optionHelp   = Tagged "Use cardano-db-sync for checking tx confirmed status"

newtype DbSyncQueryUtxos = DbSyncQueryUtxos Bool

instance IsOption DbSyncQueryUtxos where
    defaultValue = DbSyncQueryUtxos False
    parseValue s = DbSyncQueryUtxos <$> safeReadBool s
    optionName   = Tagged "db-sync-query-utxos"
    optionHelp   = Tagged "Use cardano-db-sync to query utxos"

newtype DbSyncGetParameters = DbSyncGetParameters Bool

instance IsOption DbSyncGetParameters where
    defaultValue = DbSyncGetParameters False
    parseValue s = DbSyncGetParameters <$> safeReadBool s
    optionName   = Tagged "db-sync-get-params"
    optionHelp   = Tagged "Use cardano-db-sync to get network parameters"
