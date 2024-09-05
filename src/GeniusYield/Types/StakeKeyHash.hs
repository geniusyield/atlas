{- |
Module      : GeniusYield.Types.StakeKeyHash
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.StakeKeyHash (
  GYStakeKeyHash,
  stakeKeyHashToApi,
  stakeKeyHashFromApi,
  stakeKeyHashToLedger,
  stakeKeyHashFromLedger,
) where

import Control.Lens ((?~))
import GeniusYield.Imports

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Keys qualified as Ledger
import Data.Aeson.Types qualified as Aeson
import Data.Csv qualified as Csv
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text.Encoding qualified as Text
import GeniusYield.Types.PubKeyHash (
  AsPubKeyHash (..),
  CanSignTx,
  pubKeyHashFromLedger,
  pubKeyHashToLedger,
 )
import Text.Printf qualified as Printf

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import qualified Text.Printf                as Printf
-}

newtype GYStakeKeyHash = GYStakeKeyHash (Api.Hash Api.StakeKey)
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

instance AsPubKeyHash GYStakeKeyHash where
  toPubKeyHash = stakeKeyHashToLedger >>> Ledger.coerceKeyRole >>> pubKeyHashFromLedger
  fromPubKeyHash = pubKeyHashToLedger >>> Ledger.coerceKeyRole >>> stakeKeyHashFromLedger

instance CanSignTx GYStakeKeyHash

{- |

>>> let Just skh = Aeson.decode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
>>> stakeKeyHashToApi skh
"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
stakeKeyHashToApi :: GYStakeKeyHash -> Api.Hash Api.StakeKey
stakeKeyHashToApi = coerce

{- |

>>> stakeKeyHashFromApi "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
stakeKeyHashFromApi :: Api.Hash Api.StakeKey -> GYStakeKeyHash
stakeKeyHashFromApi = coerce

-- | Convert to corresponding ledger type.
stakeKeyHashToLedger :: GYStakeKeyHash -> Ledger.KeyHash Ledger.Staking Ledger.StandardCrypto
stakeKeyHashToLedger = stakeKeyHashToApi >>> Api.unStakeKeyHash

-- | Convert from corresponding ledger type.
stakeKeyHashFromLedger :: Ledger.KeyHash Ledger.Staking Ledger.StandardCrypto -> GYStakeKeyHash
stakeKeyHashFromLedger = Api.StakeKeyHash >>> stakeKeyHashFromApi

{- |

>>> let Just skh = Aeson.decode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
>>> LBS8.putStrLn $ Aeson.encode skh
"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
instance Aeson.ToJSON GYStakeKeyHash where
  toJSON = Aeson.toJSON . Api.serialiseToRawBytesHexText . stakeKeyHashToApi

{- |

>>> Aeson.eitherDecode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d\""
Right (GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d")

Invalid characters:

>>> Aeson.eitherDecode @GYStakeKeyHash "\"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab2340386323zzz\""
Left "Error in $: RawBytesHexErrorBase16DecodeFail \"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab2340386323zzz\" \"invalid character at offset: 53\""
-}
instance Aeson.FromJSON GYStakeKeyHash where
  parseJSON =
    Aeson.withText "GYStakeKeyHash" $
      either
        (fail . show)
        (return . GYStakeKeyHash)
        . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsStakeKey)
        . Text.encodeUtf8

{- |

>>> Printf.printf "%s\n" $ stakeKeyHashFromApi "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d
-}
instance Printf.PrintfArg GYStakeKeyHash where
  formatArg = Printf.formatArg . Api.serialiseToRawBytesHexText . stakeKeyHashToApi

{- |

>>> Csv.toField @GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
"7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
-}
instance Csv.ToField GYStakeKeyHash where
  toField = Api.serialiseToRawBytesHex . stakeKeyHashToApi

{- |

>>> Csv.runParser $ Csv.parseField @GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d"
Right (GYStakeKeyHash "7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d")

>>> Csv.runParser $ Csv.parseField @GYStakeKeyHash "not a pub stake key hash"
Left "RawBytesHexErrorBase16DecodeFail \"not a pub stake key hash\" \"invalid character at offset: 0\""
-}
instance Csv.FromField GYStakeKeyHash where
  parseField = either (fail . show) (return . stakeKeyHashFromApi) . Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsStakeKey)

instance Swagger.ToSchema GYStakeKeyHash where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYStakeKeyHash" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "The hash of a public stake key."
            & Swagger.example
          ?~ toJSON ("7a77d120b9e86addc7388dbbb1bd2350490b7d140ab234038632334d" :: Text)
            & Swagger.maxLength
          ?~ 56
            & Swagger.minLength
          ?~ 56
