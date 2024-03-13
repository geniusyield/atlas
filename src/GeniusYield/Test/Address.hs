-- | Classes to get addresses and work with addresses
module GeniusYield.Test.Address (
  HasAddress (..),
  HasStakingCredential (..),
  AppendStaking (..),
  -- appendStakingCredential,
  appendCredential,
  appendStakingPubKey,
  -- appendStakingScript,
) where

-- import Cardano.Simple.PlutusLedgerApi.V1.Scripts
-- import Cardano.Simple.TxExtra (keyToStaking)
import Data.Coerce
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V2
import Prelude

-- | Everything that has address
class HasAddress a where
  toAddress :: a -> Address

instance HasAddress Address where
  toAddress = id

instance HasAddress PubKeyHash where
  toAddress = pubKeyHashAddress

-- instance HasAddress ScriptHash where
--   toAddress = scriptHashAddress

-- | Everything that has staking credential
class HasStakingCredential a where
  toStakingCredential :: a -> StakingCredential

instance HasStakingCredential StakingCredential where
  toStakingCredential = id

-- instance HasStakingCredential PubKeyHash where
--   toStakingCredential = keyToStaking

-- | Encodes appening of staking address
data AppendStaking a
  = AppendStaking StakingCredential a

instance HasAddress a => HasAddress (AppendStaking a) where
  toAddress (AppendStaking stakeCred a) = appendStake (toAddress a)
    where
      appendStake addr = addr {addressStakingCredential = Just stakeCred}

-- -- | Appends staking credential to a script
-- appendStakingCredential :: StakingCredential -> script -> AppendStaking script
-- appendStakingCredential sCred script =
--   case sCred of
--     StakingHash cred ->
--       case cred of
--         PubKeyCredential pkh ->
--           appendStakingPubKey pkh script
--         ScriptCredential (ScriptHash hash) ->
--           appendStakingScript (StakeValidatorHash hash) script
--     StakingPtr {} -> error "StakingPtr is not supported"

-- | Append staking credential info
appendCredential :: Credential -> a -> AppendStaking a
appendCredential cred = AppendStaking (StakingHash cred)

-- | Append staking public key info
appendStakingPubKey :: PubKeyHash -> a -> AppendStaking a
appendStakingPubKey pkh = appendCredential (PubKeyCredential pkh)

-- -- | Append staking script info
-- appendStakingScript :: StakeValidatorHash -> a -> AppendStaking a
-- appendStakingScript sh = appendCredential (ScriptCredential $ coerce sh)
