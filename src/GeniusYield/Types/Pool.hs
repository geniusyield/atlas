{- |
Module      : GeniusYield.Types.Certificate
Copyright   : (c) 2025 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Pool (
  GYStakePoolRelay (..),
  GYPoolParams (..),
  poolParamsToLedger,
  poolParamsFromLedger,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.State qualified as Ledger
import Data.Array.Byte qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Short.Internal qualified as SBSI
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

poolParamsToLedger :: GYPoolParams -> Ledger.StakePoolParams
poolParamsToLedger GYPoolParams {..} =
  Ledger.StakePoolParams
    { Ledger.sppId = keyHashToLedger poolId
    , Ledger.sppVrf = vrfVerKeyHashToLedger poolVrf
    , Ledger.sppPledge = fromIntegral poolPledge
    , Ledger.sppCost = fromIntegral poolCost
    , Ledger.sppMargin = poolMargin
    , Ledger.sppAccountAddress = stakeAddressToLedger poolRewardAccount
    , Ledger.sppOwners = Set.map keyHashToLedger poolOwners
    , Ledger.sppRelays = fromList $ relayToLedger <$> poolRelays
    , Ledger.sppMetadata = ms $ anchorToLedgerPoolMetadata <$> poolMetadata
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
      , Ledger.pmHash = byteStringToByteArray $ anchorDataHashToByteString anchorDataHash
      }
  ms = maybeToStrictMaybe

poolParamsFromLedger :: Ledger.StakePoolParams -> GYPoolParams
poolParamsFromLedger Ledger.StakePoolParams {..} =
  GYPoolParams
    { poolId = keyHashFromLedger sppId
    , poolVrf = vrfVerKeyHashFromLedger sppVrf
    , poolPledge = fromIntegral sppPledge
    , poolCost = fromIntegral sppCost
    , poolMargin = sppMargin
    , poolRewardAccount = stakeAddressFromLedger sppAccountAddress
    , poolOwners = Set.map keyHashFromLedger sppOwners
    , poolRelays = toList $ relayFromLedger <$> sppRelays
    , poolMetadata = sm $ anchorFromLedgerPoolMetadata <$> sppMetadata
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
      , anchorDataHash = fromMaybe (error "GeniusYield.Types.Pool.anchorFromLedgerPoolMetadata: Invalid metadata hash") (anchorDataHashFromByteString $ byteArrayToByteString pmHash)
      }

byteStringToByteArray :: BS.ByteString -> BA.ByteArray
byteStringToByteArray bs =
  case SBS.toShort bs of
    SBSI.SBS bytes -> BA.ByteArray bytes

byteArrayToByteString :: BA.ByteArray -> BS.ByteString
byteArrayToByteString (BA.ByteArray bytes) = SBS.fromShort $ SBSI.SBS bytes
