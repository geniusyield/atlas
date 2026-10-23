{-# LANGUAGE PatternSynonyms #-}

module Clb.Tx (
  CardanoTx (..),
  OnChainTx (..),
  pattern CardanoEmulatorEraTx,
  getEmulatorEraTx,
) where

import Cardano.Api qualified as C

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as Core (Tx, TxLevel (TopTx))
import Cardano.Ledger.Shelley.API qualified as L (Validated, extractTx)
import Clb.Era (CardanoLedgerEra)
import Codec.Serialise (Serialise (..))

-- | Cardano tx from any Shelley-based era.
data CardanoTx era where
  CardanoTx :: C.Tx era -> C.ShelleyBasedEra era -> CardanoTx era

-- TODO: do we need it? Should it print a tx hash?
instance Show (CardanoTx era) where
  show = const "CardanoTx"

getEmulatorEraTx :: CardanoTx era -> C.Tx era
getEmulatorEraTx (CardanoTx tx _) = tx

pattern CardanoEmulatorEraTx :: C.IsShelleyBasedEra era => C.Tx era -> CardanoTx era
pattern CardanoEmulatorEraTx tx <- (getEmulatorEraTx -> tx)
  where
    CardanoEmulatorEraTx tx = CardanoTx tx C.shelleyBasedEra

{-# COMPLETE CardanoEmulatorEraTx #-}

{- | A validated Tx, that made it to the ledger state (might be mempool/blockchain).
Might has IsValid = False in which case the collateral will be collected.
-}
newtype OnChainTx era = OnChainTx
  {getOnChainTx :: L.Validated (Core.Tx Core.TopTx (CardanoLedgerEra era))}

deriving newtype instance
  Show (Core.Tx Core.TopTx (CardanoLedgerEra era)) =>
  Show (OnChainTx era)

{- | We need this instance to be able to generate block ids, this is why
we only neew 'encode' part.
-}
instance CBOR.ToCBOR (Core.Tx Core.TopTx (CardanoLedgerEra era)) => Serialise (OnChainTx era) where
  encode = CBOR.toCBOR . L.extractTx . getOnChainTx
  decode = fail "Not allowed to use `decode` on `OnChainTx`"
