{- |
Module      : GeniusYield.Types.Wallet
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Wallet (
  WalletKeys,
  Mnemonic,
  walletKeysFromMnemonic,
  walletKeysFromMnemonicWithAccIndex,
  walletKeysFromMnemonicIndexed,
  walletKeysToExtendedPaymentSigningKey,
  walletKeysToExtendedStakeSigningKey,
  writeExtendedPaymentSigningKeyTextEnvelope,
  writeStakeSigningKeyTextEnvelope,
  walletKeysToAddress,
) where

import Cardano.Address (bech32)
import Cardano.Address.Derivation
import Cardano.Address.Style.Shelley qualified as S
import Cardano.Api
import Cardano.Mnemonic (
  MkSomeMnemonicError (..),
  mkSomeMnemonic,
 )
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.IO (throwIO)
import GeniusYield.Imports ((&))
import GeniusYield.Types.Address (
  GYAddress,
  unsafeAddressFromText,
 )
import GeniusYield.Types.Key (
  GYExtendedPaymentSigningKey,
  GYExtendedStakeSigningKey,
  extendedPaymentSigningKeyFromApi,
  extendedStakeSigningKeyFromApi,
  writeExtendedPaymentSigningKey,
  writeExtendedStakeSigningKey,
 )
import GeniusYield.Types.NetworkId (GYNetworkId (..))

type Mnemonic = [T.Text]

-- | Opaque type to represent keys of wallet.
data WalletKeys = WalletKeys
  { wkRootKey :: !(S.Shelley 'RootK XPrv)
  -- ^ The wallet's root key, aka _master key_.
  , wkAcctKey :: !(S.Shelley 'AccountK XPrv)
  -- ^ The wallet's account key.
  , wkPaymentKey :: !(S.Shelley 'PaymentK XPrv)
  -- ^ The wallet's payment key.
  , wkStakeKey :: !(S.Shelley 'DelegationK XPrv)
  -- ^ The wallet's stake key.
  }

-- | Derives @WalletKeys@ from mnemonic with the given account index and payment address index, thus using derivation path @1852H/1815H/iH/2/0@ for stake key and derivation path @1852H/1815H/iH/0/p@ for payment key where @i@ denotes the account index and @p@ denotes the given payment address index.
walletKeysFromMnemonicIndexed :: Mnemonic -> Word32 -> Word32 -> Either String WalletKeys
walletKeysFromMnemonicIndexed mns nAcctIndex nAddrIndex =
  case mkSomeMnemonic @'[9, 12, 15, 18, 21, 24] mns of
    Left err -> Left $ getMkSomeMnemonicError err
    Right mw ->
      let rootK = genMasterKeyFromMnemonic mw mempty :: S.Shelley 'RootK XPrv
          accIx = indexFromWord32 $ minHardenedPathValue + nAcctIndex
          addrIx = indexFromWord32 nAddrIndex
       in deriveWalletKeys rootK accIx addrIx
  where
    deriveWalletKeys ::
      S.Shelley 'RootK XPrv ->
      -- \^ The Root Key
      Maybe (Index 'Hardened 'AccountK) ->
      -- \^ The Index for Account
      Maybe (Index 'Soft 'PaymentK) ->
      -- \^ The Index for Address
      Either String WalletKeys
    deriveWalletKeys _ Nothing _ = Left $ "Invalid Account Index: " <> show nAcctIndex
    deriveWalletKeys _ _ Nothing = Left $ "Invalid Address Index: " <> show nAddrIndex
    deriveWalletKeys rootK (Just accIx) (Just addIx) =
      let acctK = deriveAccountPrivateKey rootK accIx
          paymentK = deriveAddressPrivateKey acctK S.UTxOExternal addIx
          stakeK = S.deriveDelegationPrivateKey acctK
       in Right WalletKeys {wkRootKey = rootK, wkAcctKey = acctK, wkPaymentKey = paymentK, wkStakeKey = stakeK}

    -- value for '0H' index
    minHardenedPathValue = 0x80000000

-- | Derives @WalletKeys@ from mnemonic with first account index, using derivation path @1852H/1815H/0H/2/0@ for stake key and derivation path @1852H/1815H/0H/0/0@ for payment key.
walletKeysFromMnemonic :: Mnemonic -> Either String WalletKeys
walletKeysFromMnemonic ms = walletKeysFromMnemonicIndexed ms 0 0

-- | Derives @WalletKeys@ from mnemonic for the given account index, using derivation path `1852H/1815H/iH/2/0` for stake key and derivation path @1852H/1815H/iH/0/0@ for payment key where @i@ denotes account index.
walletKeysFromMnemonicWithAccIndex :: Mnemonic -> Word32 -> Either String WalletKeys
walletKeysFromMnemonicWithAccIndex ms accIx = walletKeysFromMnemonicIndexed ms accIx 0

walletKeysToExtendedPaymentSigningKey :: WalletKeys -> GYExtendedPaymentSigningKey
walletKeysToExtendedPaymentSigningKey WalletKeys {wkPaymentKey} = S.getKey wkPaymentKey & PaymentExtendedSigningKey & extendedPaymentSigningKeyFromApi

walletKeysToExtendedStakeSigningKey :: WalletKeys -> GYExtendedStakeSigningKey
walletKeysToExtendedStakeSigningKey WalletKeys {wkStakeKey} = S.getKey wkStakeKey & StakeExtendedSigningKey & extendedStakeSigningKeyFromApi

{-# DEPRECATED writeExtendedPaymentSigningKeyTextEnvelope "Use combination of walletKeysFromMnemonic, walletKeysToExtendedPaymentSigningKey and writeExtendedPaymentSigningKey." #-}

-- | Writes @TextEnvelope@ with type @PaymentExtendedSigningKeyShelley_ed25519_bip32@ from mnemonic.
writeExtendedPaymentSigningKeyTextEnvelope :: Mnemonic -> FilePath -> IO ()
writeExtendedPaymentSigningKeyTextEnvelope mnemonic fPath = do
  case walletKeysFromMnemonic mnemonic of
    Left err -> throwIO $ userError err
    Right wk -> walletKeysToExtendedPaymentSigningKey wk & writeExtendedPaymentSigningKey fPath

{-# DEPRECATED writeStakeSigningKeyTextEnvelope "Use combination of walletKeysFromMnemonic, walletKeysToExtendedStakeSigningKey and writeExtendedStakeSigningKey." #-}

-- | Writes @TextEnvelope@ with type @StakeExtendedSigningKeyShelley_ed25519_bip32@ from mnemonic.
writeStakeSigningKeyTextEnvelope :: Mnemonic -> FilePath -> IO ()
writeStakeSigningKeyTextEnvelope mnemonic fPath = do
  case walletKeysFromMnemonic mnemonic of
    Left err -> throwIO $ userError err
    Right wk -> walletKeysToExtendedStakeSigningKey wk & writeExtendedStakeSigningKey fPath

-- | Gives the delegation address made using extended payment and stake keys.
walletKeysToAddress :: WalletKeys -> GYNetworkId -> GYAddress
walletKeysToAddress WalletKeys {wkPaymentKey, wkStakeKey} netId =
  let paymentCredential = S.PaymentFromExtendedKey $ toXPub <$> wkPaymentKey
      delegationCredential = S.DelegationFromExtendedKey $ toXPub <$> wkStakeKey
   in S.delegationAddress netId' paymentCredential delegationCredential & bech32 & unsafeAddressFromText
  where
    netId' = case netId of
      GYMainnet -> S.shelleyMainnet
      GYTestnetPreprod -> S.shelleyTestnet
      GYTestnetPreview -> S.shelleyTestnet
      GYTestnetLegacy -> S.shelleyTestnet
      GYPrivnet {} -> S.shelleyTestnet
