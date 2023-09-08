module GeniusYield.Types.Wallet where

import           Cardano.Address.Derivation
import qualified Cardano.Address.Style.Shelley as S
import           Cardano.Api
import           Cardano.Mnemonic              (MkSomeMnemonicError (..),
                                                mkSomeMnemonic)
import qualified Data.Text                     as T

type Mnemonic = [T.Text]

data WalletKeys = WalletKeys
  { wkRootKey    :: !XPrv     -- ^ The wallet root Key
  , wkAcctKey    :: !XPrv     -- ^ The wallet Account Key
  , wkPaymentKey :: !XPrv     -- ^ The Wallet payment Key
  , wkStakeKey   :: !XPrv     -- ^ The Wallet Stake Key
  }

-- | derives Wallet keys from mnemonic words
--
walletKeysFromMnemonicIndexed :: Mnemonic -> Integer -> Integer -> Either String WalletKeys
walletKeysFromMnemonicIndexed mns nAcctIndex nAddrIndex =
  case mkSomeMnemonic @'[9, 12, 15, 18, 21, 24] mns of
    Left err -> Left $ getMkSomeMnemonicError err
    Right mw ->
      let rootK = genMasterKeyFromMnemonic mw mempty :: S.Shelley 'RootK XPrv
          accIx = indexFromWord32 $ minHardenedPathValue + fromInteger nAcctIndex
          addrIx = indexFromWord32 $ fromInteger nAddrIndex

      in deriveWalletKeys rootK accIx addrIx

    where
      deriveWalletKeys :: S.Shelley 'RootK XPrv             -- ^ The Root Key
                       -> Maybe (Index 'Hardened 'AccountK) -- ^ The Index for Account
                       -> Maybe (Index 'Soft 'PaymentK)     -- ^ The Index for Address
                       -> Either String WalletKeys
      deriveWalletKeys _ Nothing _ = Left $ "Invalid Account Index: " <> show nAcctIndex
      deriveWalletKeys _ _ Nothing = Left $ "Invalid Address Index: " <> show nAddrIndex
      deriveWalletKeys rootK (Just accIx) (Just addIx) =
        let acctK = deriveAccountPrivateKey rootK accIx
            addrK = deriveAddressPrivateKey acctK S.UTxOExternal addIx
            stakeK = S.deriveDelegationPrivateKey acctK

         in Right WalletKeys {wkRootKey = S.getKey rootK, wkAcctKey = S.getKey acctK, wkPaymentKey = S.getKey addrK, wkStakeKey = S.getKey stakeK}

      -- value for '0H' index
      minHardenedPathValue = 0x80000000

-- | gives wallet keys with fist index
--   with derivation path  `1852H/1815H/0H/2/0` for Stake Key
--   with derivation path  `1852H/1815H/0H/0/0` for Payment Key
--
walletKeysFromMnemonic :: Mnemonic -> Either String WalletKeys
walletKeysFromMnemonic ms = walletKeysFromMnemonicIndexed ms 0 0

-- | writes TextEnvelope with type `PaymentExtendedSigningKeyShelley_ed25519_bip32` from mnemonic
--
writeExtendedPaymentSigningKeyTextEnvelope :: Mnemonic -> FilePath -> IO ()
writeExtendedPaymentSigningKeyTextEnvelope mnemonic fPath = do
  case walletKeysFromMnemonic mnemonic of
    Left err -> error err
    Right WalletKeys{wkPaymentKey} -> do
      e <- writeFileTextEnvelope (File fPath) Nothing $ PaymentExtendedSigningKey wkPaymentKey
      either (error . show) pure e

-- | writes TextEnvelope with type `StakeExtendedSigningKeyShelley_ed25519_bip32` from mnemonic
--
writeStakeSigningKeyTextEnvelope :: Mnemonic -> FilePath -> IO ()
writeStakeSigningKeyTextEnvelope mnemonic fPath = do
  case walletKeysFromMnemonic mnemonic of
    Left err -> error err
    Right WalletKeys{wkStakeKey} -> do
      e <- writeFileTextEnvelope (File fPath) Nothing $ StakeExtendedSigningKey wkStakeKey
      either (error . show) pure e
