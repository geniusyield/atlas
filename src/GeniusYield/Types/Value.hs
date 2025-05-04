{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GeniusYield.Types.Value
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop
-}
module GeniusYield.Types.Value (
  -- * Value
  GYValue,
  valueMake,
  valueToPlutus,
  valueFromPlutus,
  valueToApi,
  valueFromApi,
  valueSingleton,
  valueFromList,
  valueToList,
  valueToMap,
  valueMap,
  valueTotalAssets,
  valueInsert,
  valueAdjust,
  valueAdjustOverDefault,
  valueAlter,
  valueFromLovelace,
  valueFromApiTxOutValue,
  valueToApiTxOutValue,
  valueAssets,
  valueWithoutAssets,
  valueRestrictAssets,
  parseValueKM,

  -- ** Arithmetic
  valueMinus,
  valueMonus,
  valueNegate,
  valuePositive,
  valueNonNegative,
  valueGreaterOrEqual,
  valueGreater,
  valueLessOrEqual,

  -- ** Unions & Intersections
  valueUnionWith,
  valueIntersection,
  valueIntersectionWith,

  -- ** Lookup
  valueAssetClass,
  valueAssetPresent,

  -- ** Splitting
  valueSplitAda,
  valueAda,
  valueNonAda,
  valueSplitSign,

  -- ** Predicates
  isEmptyValue,
  valueVerifyNonNegative,

  -- ** Debug
  valueValid,

  -- ** Conversion errors
  GYFromPlutusValueError (..),

  -- * Asset class
  GYAssetClass (..),
  assetClassToPlutus,
  assetClassFromPlutus,
  assetClassToApi,
  assetClassFromApi,
  parseAssetClassWithSep,
  parseAssetClassWithoutSep,
  parseAssetClassCore,

  -- * Token name
  GYTokenName (..),
  tokenNameToHex,
  tokenNameFromBS,
  tokenNameToPlutus,
  tokenNameFromPlutus,
  tokenNameToApi,
  tokenNameFromHex,
  unsafeTokenNameFromHex,
  makeAssetClass,
) where

import Cardano.Ledger.Coin qualified as Ledger
import Control.Lens ((?~))
import Data.Aeson (object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Csv qualified as Csv
import Data.List (intercalate)
import Data.Scientific qualified as SC
import GHC.IsList qualified (toList)
import GeniusYield.Imports
import PlutusTx.Builtins (fromBuiltin, toBuiltin)

import Cardano.Api qualified as Api

-- import qualified Cardano.Api.Value                as Api
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import PlutusLedgerApi.V1.Value qualified as Plutus
import Text.Printf qualified as Printf
import Web.HttpApiData qualified as Web

import Data.Either.Combinators (mapLeft)
import Data.Foldable (for_)
import Data.Hashable (Hashable (..))
import GHC.IsList (IsList (fromList))
import GeniusYield.Types.Ada qualified as Ada
import GeniusYield.Types.Era
import GeniusYield.Types.Script

{- $setup

>>> :set -XOverloadedStrings -XTypeApplications
>>> import qualified Cardano.Api                as Api
>>> import qualified Data.Aeson                 as Aeson
>>> import qualified Data.ByteString.Char8      as BS8
>>> import qualified Data.ByteString.Lazy.Char8 as LBS8
>>> import qualified Data.Csv                   as Csv
>>> import qualified Text.Printf                as Printf
>>> import qualified Web.HttpApiData            as Web
-}

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

-- | Errors raised during 'Plutus.Value' -> 'GYValue' conversion.
data GYFromPlutusValueError
  = -- | Length of the token name bytestring is more than 32.
    GYTokenNameTooBig !Plutus.TokenName
  | -- | PolicyId deserialization failure.
    GYInvalidPolicyId !Plutus.CurrencySymbol
  deriving (Show, Eq)

-- | Value: a (total) map from asset classes ('GYAssetClass') to amount ('Integer').
newtype GYValue = GYValue (Map.Map GYAssetClass Integer)
  deriving Eq
  deriving newtype Ord

{- | Check the 'GYValue' representation invariants.

Should always evaluate to 'True'
-}
valueValid :: GYValue -> Bool
valueValid (GYValue v) = 0 `notElem` v -- invariant: zero integers are not stored.

-- This normalizes the map.
valueMake :: Map.Map GYAssetClass Integer -> GYValue
valueMake m = GYValue (Map.filter (/= 0) m)

instance Show GYValue where
  showsPrec d v =
    showParen (d > 10) $
      showString "valueFromList "
        . showsPrec 11 (valueToList v)

instance Semigroup GYValue where
  GYValue x <> GYValue y = valueMake $ Map.unionWith (+) x y

instance Monoid GYValue where
  mempty = GYValue Map.empty

-- | Converts a 'GYValue' to a Plutus 'Plutus.Value'
valueToPlutus :: GYValue -> Plutus.Value
valueToPlutus (GYValue m) = foldMap f (Map.toList m)
 where
  f :: (GYAssetClass, Integer) -> Plutus.Value
  f (assetClassToPlutus -> Plutus.AssetClass (cs, tn), n) = Plutus.singleton cs tn n

{- | Converts a Plutus 'Plutus.Value' to a 'GYValue'.
  Returns Left 'GYFromPlutusValueError' if it fails.
-}
valueFromPlutus :: Plutus.Value -> Either GYFromPlutusValueError GYValue
valueFromPlutus v = fmap valueFromList $
  forM (Plutus.flattenValue v) $ \(cs, tn, n) -> do
    ac <- assetClassFromPlutus (Plutus.AssetClass (cs, tn))
    return (ac, n)

{- |

>>> valueFromLovelace 0
valueFromList []

>>> valueFromLovelace 100
valueFromList [(GYLovelace,100)]
-}
valueFromLovelace :: Integer -> GYValue
valueFromLovelace 0 = GYValue mempty
valueFromLovelace i = GYValue (Map.singleton GYLovelace i)

{- | Returns a 'GYValue' containing only the given 'GYAssetClass' with the given amount.

>>> valueSingleton (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD") 100
valueFromList [(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",100)]
-}
valueSingleton :: GYAssetClass -> Integer -> GYValue
valueSingleton ac n = valueMake $ Map.singleton ac n

-- | Convert a 'GYValue' to a Cardano Api 'Api.Value'
valueToApi :: GYValue -> Api.Value
valueToApi v =
  fromList
    [ (assetClassToApi ac, Api.Quantity n)
    | (ac, n) <- valueToList v
    ]

-- | Convert a Cardano Api 'Api.Value' to a 'GYValue'
valueFromApi :: Api.Value -> GYValue
valueFromApi v =
  valueFromList
    [ (assetClassFromApi ac, n)
    | (ac, Api.Quantity n) <- GHC.IsList.toList v
    ]

valueFromApiTxOutValue :: Api.TxOutValue era -> GYValue
valueFromApiTxOutValue (Api.TxOutValueByron (Ledger.Coin x)) = valueFromLovelace x
valueFromApiTxOutValue (Api.TxOutValueShelleyBased e v) =
  valueFromApi $ Api.fromLedgerValue e v

valueToApiTxOutValue :: GYValue -> Api.TxOutValue ApiEra
valueToApiTxOutValue v =
  Api.TxOutValueShelleyBased
    Api.ShelleyBasedEraConway
    (Api.toLedgerValue Api.MaryEraOnwardsConway $ valueToApi v)

{- | Create 'GYValue' from a list of asset class and amount.
Duplicates are merged.
-}
valueFromList :: [(GYAssetClass, Integer)] -> GYValue
valueFromList xs = valueMake $ Map.fromListWith (+) xs

-- | Returns a list of all 'GYAssetClass' and amount pairs contained in a given 'GYValue'.
valueToList :: GYValue -> [(GYAssetClass, Integer)]
valueToList (GYValue v) = Map.toList v

-- | Returns a map from 'GYAssetClass' to their amount contained in a given 'GYValue'.
valueToMap :: GYValue -> Map GYAssetClass Integer
valueToMap (GYValue m) = m

-- | Map operation over the 'GYValue's amounts.
valueMap :: (GYAssetClass -> Integer -> Integer) -> GYValue -> GYValue
valueMap f (GYValue m) = valueMake $ Map.mapWithKey f m

-- | Insert an asset with its quantity.
valueInsert :: GYAssetClass -> Integer -> GYValue -> GYValue
valueInsert asc 0 (GYValue m) = GYValue (Map.delete asc m)
valueInsert asc i (GYValue m) = GYValue (Map.insert asc i m)

{- | Adjust the amount of a given 'GYAssetClass' in the given 'GYValue'.
If the asset is not present in the value, original value is returned instead.

If the result of the adjustment is zero, the asset is removed from the value.
-}
valueAdjust :: (Integer -> Integer) -> GYAssetClass -> GYValue -> GYValue
valueAdjust f asc (GYValue m) = case Map.lookup asc m of
  Nothing -> GYValue m
  Just i -> let fi = f i in if fi == 0 then GYValue (Map.delete asc m) else GYValue (Map.insert asc fi m)

-- | Same as 'valueAdjust' but if the asset is not present in the value, we apply the function to 0 and insert the resultant (if non-zero) into the value.
valueAdjustOverDefault :: (Integer -> Integer) -> GYAssetClass -> GYValue -> GYValue
valueAdjustOverDefault f asc (GYValue m) = case Map.lookup asc m of
  Nothing -> let fi = f 0 in if fi == 0 then GYValue m else GYValue (Map.insert asc fi m)
  Just i -> let fi = f i in if fi == 0 then GYValue (Map.delete asc m) else GYValue (Map.insert asc fi m)

{- | The expression (@'valueAlter' f asc val@) alters the value @x@ at @asc@, or absence thereof.
'valueAlter' can be used to insert, delete, or update a value in a 'GYValue'.

>>> valueAlter (Just . maybe 1 (+ 1)) GYLovelace $ mempty
valueFromList [(GYLovelace,1)]

>>> valueAlter (Just . maybe 1 (+ 1)) GYLovelace $ valueFromList [(GYLovelace, 1)]
valueFromList [(GYLovelace,2)]
-}
valueAlter :: (Maybe Integer -> Maybe Integer) -> GYAssetClass -> GYValue -> GYValue
valueAlter f asc (GYValue m) = GYValue (Map.alter f asc m)

-- | Set of assets within a 'GYValue' in non-zero quantities.
valueAssets :: GYValue -> Set GYAssetClass
valueAssets (GYValue m) = Map.keysSet m

-- | Returns the total count of assets in a given 'GYValue'
valueTotalAssets :: GYValue -> Int
valueTotalAssets (GYValue v) = Map.size v

valueWithoutAssets :: GYValue -> Set GYAssetClass -> GYValue
valueWithoutAssets (GYValue m) acs = GYValue $ Map.withoutKeys m acs

valueRestrictAssets :: GYValue -> Set GYAssetClass -> GYValue
valueRestrictAssets (GYValue m) acs = GYValue $ Map.restrictKeys m acs

{- |

>>> Printf.printf "value = %s" (valueFromList [])
value =

>>> Printf.printf "value = %s" (valueFromList [(GYLovelace, 1000)])
value = 1000 lovelace
-}
instance Printf.PrintfArg GYValue where
  formatArg v = Printf.formatArg (showValue (valueToPlutus v))

showValue :: Plutus.Value -> String
showValue = intercalate " + " . map f . Plutus.flattenValue
 where
  f :: (Plutus.CurrencySymbol, Plutus.TokenName, Integer) -> String
  f (cs, tn, n) = show n ++ " " ++ showAssetClass (Plutus.AssetClass (cs, tn))

{- |

>>> LBS8.putStrLn . Aeson.encode . valueFromList $ [(GYLovelace,22),(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",101)]
{"lovelace":22,"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44":101}
-}
instance Aeson.ToJSON GYValue where
  toJSON = object . map (uncurry assetPairToKV) . valueToList
  toEncoding = Aeson.pairs . foldMap (uncurry assetPairToKV) . valueToList

instance Csv.ToField GYValue where
  toField = LBS.toStrict . Aeson.encode

instance Csv.FromField GYValue where
  parseField value =
    case Aeson.decode $ LBS.fromStrict value of
      Just v -> pure v
      Nothing -> fail $ "Error Parsing GYValue: " <> show value

assetPairToKV :: Aeson.KeyValue e kv => GYAssetClass -> Integer -> kv
assetPairToKV ac i = K.fromText (f ac) .= i
 where
  f GYLovelace = "lovelace"
  f (GYToken cs tk) = mintingPolicyIdToText cs <> T.cons '.' (tokenNameToHex tk)

{- |

>>> Aeson.decode @GYValue "{\"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44\":101,\"lovelace\":22}"
Just (valueFromList [(GYLovelace,22),(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",101)])
-}
instance Aeson.FromJSON GYValue where -- TODO: Do we need this? Can't this be derived from newtype?
  parseJSON = Aeson.withObject "GYValue" $ parseValueKM False

-- | Parse a 'GYValue' from a JSON object.
parseValueKM ::
  -- | Attempt to try parsing without separator in asset class when parsing fails with separator.
  Bool ->
  -- | JSON object.
  KM.KeyMap Aeson.Value ->
  Aeson.Parser GYValue
parseValueKM allowWithoutSep km =
  case KM.toList km of
    [] -> pure $ valueMake mempty
    xs -> valueFromList <$> traverse go xs
 where
  go :: (Aeson.Key, Aeson.Value) -> Aeson.Parser (GYAssetClass, Integer)
  go (k, v) = do
    let k' = K.toText k
        parseWithSep = parseAssetClassWithSep '.' k'
    ac <-
      either fail pure $
        either (\(Left -> e) -> if allowWithoutSep then parseAssetClassWithoutSep k' <> e else e) Right parseWithSep
    scN <- parseJSON v
    case SC.floatingOrInteger @Double scN of
      Left d -> fail $ "Expected amount to be an integer; amount: " <> show d
      Right i -> pure (ac, i)

instance Swagger.ToSchema GYValue where
  declareNamedSchema _ = do
    integerSchema <- Swagger.declareSchemaRef @Integer Proxy
    pure $
      Swagger.named "GYValue" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerObject
            & Swagger.example
          ?~ toJSON
            ( valueFromList
                [ (GYLovelace, 22)
                , (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD", 101)
                ]
            )
            & Swagger.description
          ?~ "A multi asset quantity, represented as map where each key represents an asset: policy ID and token name in hex concatenated by a dot."
            & Swagger.additionalProperties
          ?~ Swagger.AdditionalPropertiesSchema integerSchema

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------

{- | Subtracts the second 'GYValue' from the first one (asset class by asset class). Note that zero entries are filtered for.

>>> valueFromList [(GYLovelace, 100)] `valueMinus` valueFromList [(GYLovelace, 200)]
valueFromList [(GYLovelace,-100)]

>>> valueFromList [(GYLovelace, 100)] `valueMinus` valueFromList [(GYLovelace, 100)]
valueFromList []
-}
valueMinus :: GYValue -> GYValue -> GYValue
valueMinus x y = x <> valueNegate y

{- | Subtracts the second 'GYValue' from the first one (asset class by asset class). However, this differs from 'valueMinus' in that it filters out negative amounts (zero amounts are already filtered by 'valueMinus').

>>> valueFromList [(GYLovelace, 100)] `valueMonus` valueFromList [(GYLovelace, 200)]
valueFromList []

>>> valueFromList [(GYLovelace, 100)] `valueMonus` valueFromList [(GYLovelace, 50)]
valueFromList [(GYLovelace,50)]
-}
valueMonus :: GYValue -> GYValue -> GYValue
valueMonus x y = valueMinus x y & GYValue . Map.filter (> 0) . valueToMap

-- | Returns the given 'GYValue' with all amounts negated.
valueNegate :: GYValue -> GYValue
valueNegate (GYValue x) = GYValue (Map.map negate x)

-- | Checks if all amounts of the given 'GYValue' are not negative ( '>=' 0 ).
valueNonNegative :: GYValue -> Bool
valueNonNegative (GYValue x) = all (>= 0) x

-- | Checks if all amounts of the given 'GYValue' are positive ( '>' 0 ).
valuePositive :: GYValue -> Bool
valuePositive (GYValue x) = all (> 0) x

-- | Checks if all amounts of the first 'GYValue' are greater or equal to the second 'GYValue'.
valueGreaterOrEqual :: GYValue -> GYValue -> Bool
valueGreaterOrEqual v w = valueNonNegative $ v `valueMinus` w

-- | Checks if all amounts of the first 'GYValue' are greater than the second 'GYValue'.
valueGreater :: GYValue -> GYValue -> Bool
valueGreater v w = valuePositive $ v `valueMinus` w

-- | Checks if all amounts of the first 'GYValue' are less than the second 'GYValue'.
valueLessOrEqual :: GYValue -> GYValue -> Bool
valueLessOrEqual v w = valueGreaterOrEqual w v

{- | Splits a 'GYValue' into the lovelace amount and the rest of it's components.

>>> valueSplitAda $ valueFromLovelace 100
(100,valueFromList [])

>>> valueSplitAda $ valueFromList [(GYLovelace, 100), (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",101)]
(100,valueFromList [(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",101)])
-}
valueSplitAda :: GYValue -> (Integer, GYValue)
valueSplitAda (GYValue m) = (Map.findWithDefault 0 GYLovelace m, GYValue (Map.delete GYLovelace m))

-- | Get the lovelace amount from a 'GYValue'.
valueAda :: GYValue -> Integer
valueAda = fst . valueSplitAda

-- | Removes the lovelace amount from a 'GYValue'.
valueNonAda :: GYValue -> GYValue
valueNonAda = snd . valueSplitAda

-- | Returns the amount of a 'GYAssetClass' contained in the given 'GYValue'.
valueAssetClass :: GYValue -> GYAssetClass -> Integer
valueAssetClass (GYValue m) ac = Map.findWithDefault 0 ac m

valueAssetPresent :: GYValue -> GYAssetClass -> Bool
valueAssetPresent v ac = valueAssetClass v ac > 0

{- | Split a 'GYValue' into its positive and negative components. The first element of
  the pair is the positive components of the value. The second element is the negative component.

>>> valueSplitSign $ valueFromList [(GYLovelace,22),(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",-10)]
(valueFromList [(GYLovelace,22)],valueFromList [(GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD",10)])
-}
valueSplitSign :: GYValue -> (GYValue, GYValue)
valueSplitSign (GYValue m) = (GYValue positiveVal, GYValue $ negate <$> negativeVal)
 where
  (positiveVal, negativeVal) = Map.partition (> 0) m

-- | Verify the value only consists of positive amounts, returning a map containing naturals as a result.
valueVerifyNonNegative :: GYValue -> Maybe (Map GYAssetClass Natural)
valueVerifyNonNegative (GYValue m) = if all (>= 0) m then Just $ fromIntegral <$> m else Nothing

-------------------------------------------------------------------------------
-- Unions & Intersections
-------------------------------------------------------------------------------

-- | Combine two values with given function.
valueUnionWith :: (Integer -> Integer -> Integer) -> GYValue -> GYValue -> GYValue
valueUnionWith f (GYValue m1) (GYValue m2) = valueMake $ Map.unionWith f m1 m2

{- | Left biased intersection of two 'GYValue's.

Persist only the assets in the first value that also exist in the second.
-}
valueIntersection :: GYValue -> GYValue -> GYValue
valueIntersection (GYValue m1) (GYValue m2) = valueMake $ Map.intersection m1 m2

-- | Intersection of two 'GYValue's with a combining function.
valueIntersectionWith :: (Integer -> Integer -> Integer) -> GYValue -> GYValue -> GYValue
valueIntersectionWith f (GYValue m1) (GYValue m2) = valueMake $ Map.intersectionWith f m1 m2

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------

{- | Checks if the given 'GYValue' is empty

>>> isEmptyValue mempty
True

>>> isEmptyValue $ valueFromLovelace 100
False

>>> isEmptyValue $ valueMinus (valueFromLovelace 100) (valueFromLovelace 100)
True
-}
isEmptyValue :: GYValue -> Bool
isEmptyValue (GYValue m) = Map.null m

-------------------------------------------------------------------------------
-- Asset class
-------------------------------------------------------------------------------

-- | Asset class. Either lovelace or minted token.
data GYAssetClass = GYLovelace | GYToken GYMintingPolicyId GYTokenName
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable GYAssetClass where
  hashWithSalt salt ac = hashWithSalt salt $ Web.toUrlPiece ac

instance Aeson.ToJSONKey GYAssetClass where
  toJSONKey = Aeson.toJSONKeyText Web.toUrlPiece

instance Aeson.FromJSONKey GYAssetClass where
  fromJSONKey = Aeson.FromJSONKeyTextParser (either (fail . show) pure . Web.parseUrlPiece)

instance Swagger.ToParamSchema GYAssetClass where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString

instance Swagger.ToSchema GYAssetClass where
  declareNamedSchema p = do
    return $
      Swagger.named "GYAssetClass" $
        Swagger.paramSchemaToSchema p
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.description
          ?~ "This is an asset class, i.e. either \"lovelace\" or some other token with its minting policy and token name delimited by dot (.)."
            & Swagger.example
          ?~ toJSON ("ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44" :: Text)

-- | Converts a 'GYAssetClass' into a Plutus 'Plutus.AssetClass'.
assetClassToPlutus :: GYAssetClass -> Plutus.AssetClass
assetClassToPlutus GYLovelace = Plutus.AssetClass (Ada.adaSymbol, Ada.adaToken)
assetClassToPlutus (GYToken cs tn) = Plutus.AssetClass (mintingPolicyIdToCurrencySymbol cs, tokenNameToPlutus tn)

{- | Converts a Plutus 'Plutus.AssetClass' into a 'GYAssetClass'.
Returns Left 'GYFromPlutusValueError' if it fails.
-}
assetClassFromPlutus :: Plutus.AssetClass -> Either GYFromPlutusValueError GYAssetClass
assetClassFromPlutus (Plutus.AssetClass (cs, tn))
  | cs == Ada.adaSymbol, tn == Ada.adaToken = Right GYLovelace
  | otherwise = do
      tn' <- maybe (Left $ GYTokenNameTooBig tn) Right $ tokenNameFromPlutus tn
      cs' <- mapLeft (\_ -> GYInvalidPolicyId cs) . Api.deserialiseFromRawBytes Api.AsScriptHash $
        case cs of Plutus.CurrencySymbol bs -> fromBuiltin bs
      return (GYToken (mintingPolicyIdFromApi (Api.PolicyId cs')) tn')

-- | Converts a 'GYAssetClass' into a Cardano Api 'Api.AssetId'.
assetClassToApi :: GYAssetClass -> Api.AssetId
assetClassToApi GYLovelace = Api.AdaAssetId
assetClassToApi (GYToken cs tn) = Api.AssetId (mintingPolicyIdToApi cs) (tokenNameToApi tn)

-- | Converts a Cardano Api 'Api.AssetId' into a 'GYAssetClass'.
assetClassFromApi :: Api.AssetId -> GYAssetClass
assetClassFromApi Api.AdaAssetId = GYLovelace
assetClassFromApi (Api.AssetId cs tn) = GYToken (mintingPolicyIdFromApi cs) (tokenNameFromApi tn)

instance IsString GYAssetClass where
  fromString s = case Web.parseUrlPiece $ T.pack s of
    Left err -> error $ T.unpack err
    Right x -> x

{- |

>>> Printf.printf "ac = %s" GYLovelace
ac = lovelace
-}
instance Printf.PrintfArg GYAssetClass where
  formatArg ac = Printf.formatArg (showAssetClass (assetClassToPlutus ac))

showAssetClass :: Plutus.AssetClass -> String
showAssetClass (Plutus.AssetClass (cs, tn))
  | cs == Ada.adaSymbol && tn == Ada.adaToken = "lovelace"
  | otherwise = case tokenNameFromPlutus tn of
      Nothing -> error $ "invalid token name: " <> show tn
      Just tn' -> show cs <> "." <> T.unpack (tokenNameToHex tn')

{- |

>>> Web.toUrlPiece GYLovelace
"lovelace"

>>> Web.toUrlPiece (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "GOLD")
"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.474f4c44"

>>> let tn = unsafeTokenNameFromHex "0014df1043727970746f20556e69636f726e" in Web.toUrlPiece (GYToken "ecda51cf797535f5661a8ade59170d6f3ee7623be5789b58fac583f0" tn)
"ecda51cf797535f5661a8ade59170d6f3ee7623be5789b58fac583f0.0014df1043727970746f20556e69636f726e"
-}
instance Web.ToHttpApiData GYAssetClass where
  toUrlPiece = T.pack . showAssetClass . assetClassToPlutus

{- | Note: not used currently by API (tests only)

>>> Web.parseUrlPiece @GYAssetClass "lovelace"
Right GYLovelace

>>> Web.parseUrlPiece @GYAssetClass ""
Right GYLovelace

>>> Web.parseUrlPiece @GYAssetClass "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")
-}
instance Web.FromHttpApiData GYAssetClass where
  parseUrlPiece t = first T.pack (parseAssetClassWithSep '.' t)

{- |

>>> LBS8.putStrLn $ Aeson.encode GYLovelace
"lovelace"

>>> LBS8.putStrLn $ Aeson.encode $ GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold"
"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64"
-}
instance Aeson.ToJSON GYAssetClass where
  toJSON = Aeson.toJSON . showAssetClass . assetClassToPlutus

{- |

>>> Aeson.decode @GYAssetClass "\"lovelace\""
Just GYLovelace

>>> Aeson.decode @GYAssetClass "\"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64\""
Just (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")

>>> Aeson.decode @GYAssetClass "\"ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.0014df1043727970746f20556e69636f726e\""
Just (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "\NUL\DC4\223\DLECrypto Unicorn")
-}
instance Aeson.FromJSON GYAssetClass where
  parseJSON (Aeson.String t) = either fail return (parseAssetClassWithSep '.' t)
  parseJSON v = Aeson.typeMismatch "AssetClass" v

{- |

>>> BS8.putStrLn $ Csv.toField GYLovelace
lovelace

>>> BS8.putStrLn $ Csv.toField $ GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold"
ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64
-}
instance Csv.ToField GYAssetClass where
  toField = encodeUtf8 . T.pack . showAssetClass . assetClassToPlutus

{- |

>>> Csv.runParser @GYAssetClass $ Csv.parseField "lovelace"
Right GYLovelace

>>> Csv.runParser @GYAssetClass $ Csv.parseField ""
Right GYLovelace

>>> Csv.runParser @GYAssetClass $ Csv.parseField "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")

>>> Csv.runParser @GYAssetClass $ Csv.parseField "not an asset class"
Left "not enough input"
-}
instance Csv.FromField GYAssetClass where
  parseField = either fail return . parseAssetClassWithSep '.' . TE.decodeUtf8Lenient

{- |
Parse hex encoded currency symbol and hex encoded token name separated by the given separator.
>>> parseAssetClassWithSep '.' "lovelace"
Right GYLovelace

>>> parseAssetClassWithSep '.' ""
Right GYLovelace

>>> parseAssetClassWithSep '.' "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef.476f6c64"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")
>>> parseAssetClassWithSep '#' "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef#476f6c64"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")
>>> parseAssetClassWithSep '#' "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef#"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "")
-}
parseAssetClassWithSep :: Char -> Text -> Either String GYAssetClass
parseAssetClassWithSep = parseAssetClass . Just

{- |
Parse hex encoded currency symbol and hex encoded token name joined together without any separator.
>>> parseAssetClassWithoutSep "lovelace"
Right GYLovelace

>>> parseAssetClassWithoutSep ""
Right GYLovelace

>>> parseAssetClassWithoutSep "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef476f6c64"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "Gold")
>>> parseAssetClassWithoutSep "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef"
Right (GYToken "ff80aaaf03a273b8f5c558168dc0e2377eea810badbae6eceefc14ef" "")
-}
parseAssetClassWithoutSep :: Text -> Either String GYAssetClass
parseAssetClassWithoutSep = parseAssetClass Nothing

parseAssetClass :: Maybe Char -> Text -> Either String GYAssetClass
parseAssetClass msep =
  case msep of
    Just sep -> parseAssetClassCore sep tnParser
    Nothing -> parseAssetClassCore' Nothing tnParser
 where
  tnParser tn =
    case tokenNameFromHexBS tn of
      Left err -> fail $ T.unpack err
      Right x -> pure x

parseAssetClassCore :: Char -> (BS.ByteString -> Atto.Parser GYTokenName) -> Text -> Either String GYAssetClass
parseAssetClassCore = parseAssetClassCore' . Just

parseAssetClassCore' :: Maybe Char -> (BS.ByteString -> Atto.Parser GYTokenName) -> Text -> Either String GYAssetClass
parseAssetClassCore' _ _ "lovelace" = pure GYLovelace
parseAssetClassCore' _ _ "" = pure GYLovelace
parseAssetClassCore' msep tkParser t = Atto.parseOnly parser (TE.encodeUtf8 t)
 where
  parser :: Atto.Parser GYAssetClass
  parser = do
    cs <- Atto.take 56
    case Api.deserialiseFromRawBytesHex Api.AsPolicyId cs of
      Left x -> fail $ "Invalid currency symbol: " ++ show cs ++ "; Reason: " ++ show x
      Right cs' -> do
        for_ msep (void . Atto.char)
        tn <- Atto.takeWhile isAlphaNum
        GYToken (mintingPolicyIdFromApi cs') <$> tkParser tn

-------------------------------------------------------------------------------
-- TokenName
-------------------------------------------------------------------------------

{- | Token name is an arbitrary byte string up to 32 bytes long.

TODO: it's unclear whether it's an arbitrary byte string or UTF8 encoded text #32
(which encoded byte form is 32 byte long at most).
(https://github.com/geniusyield/atlas/issues/32)
/We treat it as an arbitrary string/.

>>> LBS8.putStrLn $ Aeson.encode ("Gold" :: GYTokenName)
"476f6c64"
-}
newtype GYTokenName = GYTokenName BS.ByteString
  deriving stock (Eq, Ord)

instance Show GYTokenName where
  showsPrec d (GYTokenName s) = showsPrec d s

-- | /Does NOT UTF8-encode/.
instance IsString GYTokenName where
  fromString s =
    fromMaybe
      (error $ "fromString @GYTokenName " ++ show s ++ ": token name too long")
      (tokenNameFromBS bs)
   where
    bs = fromString s -- TODO: utf8-encode #33 (https://github.com/geniusyield/atlas/issues/33)

instance Swagger.ToParamSchema GYTokenName where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.maxLength
      ?~ 64
        & Swagger.format
      ?~ "hex"
        & Swagger.pattern
      ?~ "[0-9a-fA-F]+"

instance Swagger.ToSchema GYTokenName where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYTokenName" $
        Swagger.paramSchemaToSchema (Proxy @GYTokenName)
          & Swagger.description
          ?~ "This is the name of a token."
            & Swagger.example
          ?~ toJSON ("476f6c64" :: Text)

{- |

>>> Aeson.eitherDecode @GYTokenName "\"476f6c64\""
Right "Gold"

>>> Aeson.eitherDecode @GYTokenName "\"0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021\""
Left "Error in $: parseJSON @GYTokenName: token name too long (0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021)"

>>> Aeson.eitherDecode @GYTokenName "\"gold\""
Left "Error in $: parseJSON @GYTokenName: not base16 encoded (gold)"

>>> Aeson.eitherDecode @GYTokenName "123"
Left "Error in $: parsing Text failed, expected String, but encountered Number"
-}
instance Aeson.FromJSON GYTokenName where
  parseJSON v = do
    t <- parseJSON v
    case Web.parseUrlPiece t of
      Right tn -> return tn
      Left err -> fail $ "parseJSON @GYTokenName: " <> T.unpack err

{- |

>>> Aeson.encode @GYTokenName "Gold"
"\"476f6c64\""
-}
instance Aeson.ToJSON GYTokenName where
  toJSON = toJSON . tokenNameToHex
  toEncoding = toEncoding . tokenNameToHex

{- |

>>> Web.parseUrlPiece @GYTokenName "476f6c64"
Right "Gold"

>>> Web.parseUrlPiece @GYTokenName "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021"
Left "token name too long (0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021)"

>>> Web.parseUrlPiece @GYTokenName "Gold"
Left "not base16 encoded (Gold)"
-}
instance Web.FromHttpApiData GYTokenName where
  parseUrlPiece t = case Base16.decode $ TE.encodeUtf8 t of
    Right bs -> maybe (Left $ "token name too long (" <> t <> ")") Right $ tokenNameFromBS bs
    Left _ -> Left $ "not base16 encoded (" <> t <> ")"

tokenNameToHex :: GYTokenName -> Text
tokenNameToHex (GYTokenName bs) = TE.decodeUtf8 $ Base16.encode bs

-- >>> tokenNameToPlutus "GOLD"
-- "GOLD"

tokenNameToPlutus :: GYTokenName -> Plutus.TokenName
tokenNameToPlutus (GYTokenName bs) = Plutus.TokenName (toBuiltin bs)

-- | Convert Plutus 'Plutus.TokenName' to 'GYTokenName'.
tokenNameFromPlutus :: HasCallStack => Plutus.TokenName -> Maybe GYTokenName
tokenNameFromPlutus (Plutus.TokenName bbs) = tokenNameFromBS (fromBuiltin bbs)

tokenNameFromBS :: BS.ByteString -> Maybe GYTokenName
tokenNameFromBS bs
  | BS.length bs > 32 = Nothing
  | otherwise = Just (GYTokenName bs)

tokenNameToApi :: GYTokenName -> Api.AssetName
tokenNameToApi = coerce

tokenNameFromApi :: Api.AssetName -> GYTokenName
tokenNameFromApi = coerce

tokenNameFromHex :: Text -> Either Text GYTokenName
tokenNameFromHex = Web.parseUrlPiece

tokenNameFromHexBS :: BS.ByteString -> Either Text GYTokenName
tokenNameFromHexBS = Web.parseUrlPiece . TE.decodeUtf8

unsafeTokenNameFromHex :: Text -> GYTokenName
unsafeTokenNameFromHex = either (error . T.unpack) id . tokenNameFromHex

makeAssetClass :: Text -> Text -> Either String GYAssetClass
makeAssetClass cs tn = parseAssetClassWithSep '.' $ if cs == "" then tn else cs <> T.cons '.' tn
