{- |
Module      : GeniusYield.Types.Certificate
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Pool (
  GYStakePoolRelay (..),
  Port (..),
  DnsName (..),
  Network (..),
  BoundedRational (..),
  UnitInterval,
  GYPoolParams (..),
  poolParamsToLedger,
  poolParamsFromLedger,
) where

import Cardano.Api.Address qualified as Api
import Cardano.Api.ReexposeLedger qualified as Ledger
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.BaseTypes
import Data.IP (IPv4, IPv6)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.IsList (IsList (..))
import GHC.Natural (Natural)
import GeniusYield.Imports (Generic, Set)
import GeniusYield.Types.Address
import GeniusYield.Types.Anchor
import GeniusYield.Types.KeyHash
import GeniusYield.Types.KeyRole

data GYStakePoolRelay
  = -- | One or both of IPv4 & IPv6
    GYSingleHostAddr !(Maybe Port) !(Maybe IPv4) !(Maybe IPv6)
  | -- | An @A@ or @AAAA@ DNS record
    GYSingleHostName !(Maybe Port) !DnsName
  | -- | A @SRV@ DNS record
    GYMultiHostName !DnsName
  deriving (Eq, Ord, Generic, Show)

-- | Stake pool parameters.
data GYPoolParams = GYPoolParams
  { poolId :: !(GYKeyHash 'GYKeyRoleStakePool)
  , poolVrf :: !(GYVRFVerKeyHash 'GYKeyRoleVRFStakePool)
  , poolPledge :: !Natural
  , poolCost :: !Natural
  , poolMargin :: !UnitInterval
  , poolRewardAccount :: !GYStakeAddress
  , poolOwners :: !(Set (GYKeyHash 'GYKeyRoleStaking))
  , poolRelays :: ![GYStakePoolRelay]
  , poolMetadata :: !(Maybe GYAnchor)
  }
  deriving stock (Show, Eq, Ord)

poolParamsToLedger :: GYPoolParams -> Ledger.PoolParams Ledger.StandardCrypto
poolParamsToLedger GYPoolParams {..} =
  Ledger.PoolParams
    { Ledger.ppId = keyHashToLedger poolId
    , Ledger.ppVrf = vrfVerKeyHashToLedger poolVrf
    , Ledger.ppPledge = fromIntegral poolPledge
    , Ledger.ppCost = fromIntegral poolCost
    , Ledger.ppMargin = poolMargin
    , Ledger.ppRewardAccount = Ledger.RewardAccount nw sc
    , Ledger.ppOwners = Set.map keyHashToLedger poolOwners
    , Ledger.ppRelays = fromList $ relayToLedger <$> poolRelays
    , Ledger.ppMetadata = ms $ anchorToLedgerPoolMetadata <$> poolMetadata
    }
 where
  relayToLedger :: GYStakePoolRelay -> Ledger.StakePoolRelay
  relayToLedger = \case
    GYSingleHostAddr port ipv4 ipv6 ->
      Ledger.SingleHostAddr (ms port) (ms ipv4) (ms ipv6)
    GYSingleHostName port dnsName ->
      Ledger.SingleHostName (ms port) dnsName
    GYMultiHostName dnsName ->
      Ledger.MultiHostName dnsName

  anchorToLedgerPoolMetadata :: GYAnchor -> Ledger.PoolMetadata
  anchorToLedgerPoolMetadata GYAnchor {..} =
    Ledger.PoolMetadata
      { Ledger.pmUrl = urlToLedger anchorUrl
      , Ledger.pmHash = anchorDataHashToByteString anchorDataHash
      }
  ms = maybeToStrictMaybe

  Api.StakeAddress nw sc = stakeAddressToApi poolRewardAccount

poolParamsFromLedger :: Ledger.PoolParams Ledger.StandardCrypto -> GYPoolParams
poolParamsFromLedger Ledger.PoolParams {..} =
  GYPoolParams
    { poolId = keyHashFromLedger ppId
    , poolVrf = vrfVerKeyHashFromLedger ppVrf
    , poolPledge = fromIntegral ppPledge
    , poolCost = fromIntegral ppCost
    , poolMargin = ppMargin
    , poolRewardAccount = stakeAddressFromApi $ Api.StakeAddress nw sc
    , poolOwners = Set.map keyHashFromLedger ppOwners
    , poolRelays = toList $ relayFromLedger <$> ppRelays
    , poolMetadata = sm $ anchorFromLedgerPoolMetadata <$> ppMetadata
    }
 where
  relayFromLedger :: Ledger.StakePoolRelay -> GYStakePoolRelay
  relayFromLedger = \case
    Ledger.SingleHostAddr port ipv4 ipv6 ->
      GYSingleHostAddr (sm port) (sm ipv4) (sm ipv6)
    Ledger.SingleHostName port dnsName ->
      GYSingleHostName (sm port) dnsName
    Ledger.MultiHostName dnsName ->
      GYMultiHostName dnsName

  sm = strictMaybeToMaybe

  anchorFromLedgerPoolMetadata :: Ledger.PoolMetadata -> GYAnchor
  anchorFromLedgerPoolMetadata Ledger.PoolMetadata {..} =
    GYAnchor
      { anchorUrl = urlFromLedger pmUrl
      , anchorDataHash = fromMaybe (error "GeniusYield.Types.Pool.anchorFromLedgerPoolMetadata: Invalid metadata hash") (anchorDataHashFromByteString pmHash)
      }
  Ledger.RewardAccount nw sc = ppRewardAccount