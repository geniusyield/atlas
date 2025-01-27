{- |
Module      : GeniusYield.Types.TxOutRef
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.TxOutRef (
  GYTxOutRef,
  txOutRefToPlutus,
  txOutRefFromPlutus,
  txOutRefFromApi,
  txOutRefFromApiTxIdIx,
  wordToApiIx,
  txOutRefToApi,

  -- * Helpers
  showTxOutRef,
  txOutRefToTuple,
  txOutRefToTuple',
  txOutRefFromTuple,

  -- * CBOR format
  GYTxOutRefCbor (..),
) where

import Cardano.Api qualified as Api
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens ((?~))
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as Csv
import Data.Hashable (Hashable (..))
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Swagger.Lens qualified ()
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import PlutusLedgerApi.V1 qualified as Plutus (TxId (..), TxOutRef (..))
import PlutusTx.Builtins.Internal qualified as Plutus
import Text.Printf qualified as Printf
import Web.HttpApiData qualified as Web

import Data.Either.Combinators (mapLeft)
import GeniusYield.Imports
import GeniusYield.Types.Ledger
import GeniusYield.Types.Tx

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Data.Csv                   as Csv
>>> import qualified PlutusLedgerApi.V1         as Plutus
>>> import qualified Web.HttpApiData            as Web
-}

-------------------------------------------------------------------------------
-- GYTxOutRef
-------------------------------------------------------------------------------

-- | Type that represents a reference to a 'GYTxOut'.
newtype GYTxOutRef = GYTxOutRef Api.TxIn
  deriving (Show, Eq, Ord)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)

instance Hashable GYTxOutRef where
  hashWithSalt salt (GYTxOutRef (Api.TxIn x (Api.TxIx y))) =
    salt
      `hashWithSalt` Api.serialiseToRawBytes x
      `hashWithSalt` y

{- |

>>> txOutRefFromPlutus $ Plutus.TxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" 12
Right (GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 12)))

>>> txOutRefFromPlutus $ Plutus.TxOutRef "ae" 12
Left (UnknownPlutusToCardanoError {ptceTag = "txOutRefFromPlutus: invalid txOutRefId ae, error: SerialiseAsRawBytesError {unSerialiseAsRawBytesError = \"Unable to deserialise TxId\"}"})

>>> txOutRefFromPlutus $ Plutus.TxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (-2)
Left (UnknownPlutusToCardanoError {ptceTag = "txOutRefFromPlutus: negative txOutRefIdx -2"})

>>> txOutRefFromPlutus $ Plutus.TxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" 123456789012345678901
Left (UnknownPlutusToCardanoError {ptceTag = "txOutRefFromPlutus: txOutRefIdx 123456789012345678901 too large"})
-}
txOutRefFromPlutus :: Plutus.TxOutRef -> Either PlutusToCardanoError GYTxOutRef
txOutRefFromPlutus (Plutus.TxOutRef tid@(Plutus.TxId (Plutus.BuiltinByteString bs)) ix) = coerce . Api.TxIn <$> etid <*> eix
 where
  etid :: Either PlutusToCardanoError Api.TxId
  etid =
    mapLeft (\e -> UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: invalid txOutRefId " <> show tid <> ", error: " <> show e) $
      Api.deserialiseFromRawBytes Api.AsTxId bs

  eix :: Either PlutusToCardanoError Api.TxIx
  eix
    | ix < 0 = Left $ UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: negative txOutRefIdx " ++ show ix
    | ix > toInteger (maxBound @Word) = Left $ UnknownPlutusToCardanoError $ Text.pack $ "txOutRefFromPlutus: txOutRefIdx " ++ show ix ++ " too large"
    | otherwise = Right $ Api.TxIx $ fromInteger ix

{- |

>>> txOutRefToPlutus "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
TxOutRef {txOutRefId = 4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189, txOutRefIdx = 1}
-}
txOutRefToPlutus :: GYTxOutRef -> Plutus.TxOutRef
txOutRefToPlutus (GYTxOutRef (Api.TxIn tid (Api.TxIx ix))) =
  Plutus.TxOutRef
    (Plutus.TxId $ Plutus.BuiltinByteString $ Api.serialiseToRawBytes tid)
    (toInteger ix)

txOutRefFromApi :: Api.TxIn -> GYTxOutRef
txOutRefFromApi = coerce

txOutRefFromApiTxIdIx :: Api.TxId -> Api.TxIx -> GYTxOutRef
txOutRefFromApiTxIdIx txId ix = coerce (Api.TxIn txId ix)

wordToApiIx :: Word -> Api.TxIx
wordToApiIx = Api.TxIx

txOutRefToApi :: GYTxOutRef -> Api.TxIn
txOutRefToApi = coerce

txOutRefToTuple :: GYTxOutRef -> (GYTxId, Word)
txOutRefToTuple (GYTxOutRef (Api.TxIn x (Api.TxIx y))) = (txIdFromApi x, y)

txOutRefToTuple' :: GYTxOutRef -> (Text, Word)
txOutRefToTuple' (GYTxOutRef (Api.TxIn x (Api.TxIx y))) = (Api.serialiseToRawBytesHexText x, y)

txOutRefFromTuple :: (GYTxId, Word) -> GYTxOutRef
txOutRefFromTuple (txIdToApi -> x, y) = GYTxOutRef (Api.TxIn x (Api.TxIx y))

{- |

>>> Web.parseUrlPiece @GYTxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
Right (GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1)))
-}
instance Web.FromHttpApiData GYTxOutRef where
  -- copy parseTxIn from cardano-api
  parseUrlPiece tr = case Atto.parseOnly parser (TE.encodeUtf8 tr) of
    Left err -> Left (T.pack ("GYTxOutRef: " ++ err))
    Right x -> Right x
   where
    parser :: Atto.Parser GYTxOutRef
    parser = do
      tx <- Base16.decodeLenient <$> Atto.takeWhile1 isHexDigit
      _ <- Atto.char '#'
      ix <- Atto.decimal
      tx' <- either (\e -> fail $ "not txid bytes: " <> show tx <> " , error: " <> show e) pure $ Api.deserialiseFromRawBytes Api.AsTxId tx
      return (GYTxOutRef (Api.TxIn tx' (Api.TxIx ix)))

instance Web.ToHttpApiData GYTxOutRef where
  toUrlPiece = showTxOutRef

instance Printf.PrintfArg GYTxOutRef where
  formatArg oref = Printf.formatArg (showTxOutRef oref)

-- renderTxIn in cardano-api
showTxOutRef :: GYTxOutRef -> Text
showTxOutRef (GYTxOutRef (Api.TxIn txId (Api.TxIx ix))) =
  Api.serialiseToRawBytesHexText txId <> "#" <> T.pack (show ix)

{- |

>>> "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: GYTxOutRef
GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1))

>>> "not-a-tx-out-ref" :: GYTxOutRef
*** Exception: invalid GYTxOutRef: not-a-tx-out-ref
...
-}
instance IsString GYTxOutRef where
  fromString s = fromRight (error $ "invalid GYTxOutRef: " <> s) $ Web.parseUrlPiece $ T.pack s

{- |

>>> Csv.toField ("4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: GYTxOutRef)
"4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
-}
instance Csv.ToField GYTxOutRef where
  toField = encodeUtf8 . showTxOutRef

{- |

>>> Csv.runParser $ Csv.parseField @GYTxOutRef "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1"
Right (GYTxOutRef (TxIn "4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189" (TxIx 1)))

>>> Csv.runParser $ Csv.parseField @GYTxOutRef "not a tx-out ref"
Left "GYTxOutRef: Failed reading: takeWhile1"
-}
instance Csv.FromField GYTxOutRef where
  parseField = either (fail . T.unpack) return . Web.parseUrlPiece . decodeUtf8Lenient

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYTxOutRef where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "hex"
        & Swagger.pattern
      ?~ "[0-9a-fA-F]{64}#\"d+"

instance Swagger.ToSchema GYTxOutRef where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYTxOutRef" $
        Swagger.paramSchemaToSchema (Proxy @GYTxOutRef)
          & Swagger.example
          ?~ toJSON ("4293386fef391299c9886dc0ef3e8676cbdbc2c9f2773507f1f838e00043a189#1" :: Text)

-------------------------------------------------------------------------------
-- GYTxOutRefCbor
-------------------------------------------------------------------------------

newtype GYTxOutRefCbor = GYTxOutRefCbor {getTxOutRefHex :: GYTxOutRef}

{- |

>>> Web.parseUrlPiece @GYTxOutRefCbor "8282582004ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e420082583900e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f6161a004c4b40"
Right GYTxOutRef (TxIn "04ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e42" (TxIx 0))

>>> Web.parseUrlPiece @GYTxOutRefCbor "00"
Left "Invalid TxIn CBOR structure"
-}
instance Web.FromHttpApiData GYTxOutRefCbor where
  parseUrlPiece t = do
    bs <- first T.pack $ Base16.decode $ TE.encodeUtf8 t
    (rest, cbor) <- first (T.pack . show) $ CBOR.deserialiseFromBytes CBOR.decodeTerm $ LBS.fromStrict bs
    unless (LBS.null rest) $ Left "Left overs in input"
    case cbor of
      CBOR.TList [CBOR.TList [CBOR.TBytes tx, CBOR.TInt ix], _] -> do
        tx' <- mapLeft (\e -> T.pack $ "not txid bytes: " ++ show tx <> ", error: " <> show e) $ Api.deserialiseFromRawBytes Api.AsTxId tx
        unless (ix >= 0) $ Left "negative ix"
        return (GYTxOutRefCbor (GYTxOutRef (Api.TxIn tx' (Api.TxIx (fromIntegral ix)))))
      _ -> Left "Invalid TxIn CBOR structure"

instance Show GYTxOutRefCbor where
  show (GYTxOutRefCbor tx) = show tx

instance Printf.PrintfArg GYTxOutRefCbor where
  formatArg (GYTxOutRefCbor (GYTxOutRef txRef)) = Printf.formatArg (show txRef)

instance Aeson.FromJSON GYTxOutRefCbor where
  parseJSON v = do
    t <- Aeson.parseJSON v
    case Web.parseUrlPiece t of
      Left err -> fail $ T.unpack err
      Right ref -> return ref

instance Aeson.ToJSON GYTxOutRefCbor where
    toJSON (GYTxOutRefCbor gyTxOutRef) =
        let Api.TxIn txId (Api.TxIx txIx) = GeniusYield.Types.TxOutRef.txOutRefToApi gyTxOutRef
            someInt = 0 -- NOTE(jaredponn) January 27, 2025: I don't think the
                        -- value of this int matters, it just needs to be some int.
        in Aeson.String
            $ TE.decodeASCII
            $ Base16.encode
            $ CBOR.toStrictByteString
            $ CBOR.encodeTerm
            $ CBOR.TList
                [ CBOR.TList
                    [ CBOR.TBytes $ Api.serialiseToRawBytes txId
                    , CBOR.TInt $ fromIntegral txIx]
                , CBOR.TInt someInt
                ]

-------------------------------------------------------------------------------
-- swagger schema
-------------------------------------------------------------------------------

instance Swagger.ToParamSchema GYTxOutRefCbor where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.format
      ?~ "cbor hex"
        & Swagger.pattern
      ?~ "[0-9a-fA-F]+"

instance Swagger.ToSchema GYTxOutRefCbor where
  declareNamedSchema p =
    Swagger.plain $
      Swagger.paramSchemaToSchema p
        & Swagger.example
        ?~ toJSON ("8282582004ffecdf5f3ced5c5c788833415bcbef26e3e21290fcebcf8216327e21569e420082583900e1cbb80db89e292269aeb93ec15eb963dda5176b66949fe1c2a6a38d1b930e9f7add78a174a21000e989ff551366dcd127028cb2aa39f6161a004c4b40" :: Text)
