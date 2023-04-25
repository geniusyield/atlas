{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : GeniusYield.Imports
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Imports (
    module X,
    pattern TODO,
    findFirst,
    decodeUtf8Lenient,
    lazyDecodeUtf8Lenient,
    hush,
    hoistMaybe
) where

import           Control.Applicative        as X (liftA2)
import           Control.Exception          as X (Exception, catch, throwIO)
import           Control.Monad              as X (ap, foldM, forM, forM_, guard,
                                                  join, unless, when)
import           Data.Aeson                 as X (FromJSON (..), ToJSON (..))
import           Data.Bifunctor             as X (bimap, first, second)
import           Data.Char                  as X (isAlphaNum, isHexDigit)
import           Data.Coerce                as X (coerce)
import           Data.Either                as X (fromRight)
import           Data.Either.Combinators    as X (rightToMaybe)
import           Data.Foldable              as X (find, foldl', toList)
import           Data.Foldable.WithIndex    as X (ifor_, itoList)
import           Data.Function              as X (on)
import           Data.Functor               as X (void)
import           Data.Functor.Const         as X (Const (..))
import           Data.Functor.Contravariant as X (Contravariant (..))
import           Data.Functor.Identity      as X (Identity (..))
import           Data.Kind                  as X (Constraint, Type)
import           Data.List                  as X (maximumBy, minimumBy, sortBy)
import           Data.Map                   as X (Map)
import           Data.Maybe                 as X (fromMaybe, isJust)
import           Data.Proxy                 as X (Proxy (..))
import           Data.Set                   as X (Set)
import           Data.Some                  as X (Some (..), withSome)
import           Data.String                as X (IsString (..))
import           Data.Text                  as X (Text)
import           Data.Text.Encoding         as X (encodeUtf8)
import           Data.Type.Equality         as X ((:~:) (..))
import           Data.Void                  as X (Void, absurd)
import           GHC.Generics               as X (Generic)
import           GHC.Stack                  as X (CallStack, HasCallStack)
import           Numeric.Natural            as X (Natural)
import           Text.Printf                as X (PrintfArg (..), printf)
import           Witherable                 as X (catMaybes, iwither, mapMaybe,
                                                  wither)

-- Not re-exported.
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Monoid                (First (..))
import qualified Data.Text.Encoding         as TE
import           Data.Text.Encoding.Error   (lenientDecode)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LTE
import           GHC.TypeLits               (ErrorMessage (..), TypeError)

-- | Use 'TODO' instead of 'undefined's
pattern TODO :: () => HasCallStack => a
pattern TODO <- (todoMatch -> ())
  where TODO = error "TODO"

{-# DEPRECATED TODO "TODO left in the code" #-}

todoMatch :: a -> ()
todoMatch _ = ()

findFirst :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
findFirst f xs = getFirst (foldMap (coerce f) xs)

-- poisonous instances
-- (the orphan in plutus-ledger-api was removed in Feb 2022)
instance TypeError ('Text "Forbidden FromJSON ByteString instance") => FromJSON ByteString where
    parseJSON = error "FromJSON @ByteString"

instance TypeError ('Text "Forbidden ToJSON ByteString instance") => ToJSON ByteString where
    toJSON = error "ToJSON @ByteString"

-- | Decode a lazy 'ByteString' containing UTF-8 encoded text.
--
-- Any invalid input bytes will be replaced with the Unicode replacement
-- character U+FFFD.
lazyDecodeUtf8Lenient :: LBS.ByteString -> LT.Text
lazyDecodeUtf8Lenient = LTE.decodeUtf8With lenientDecode

-- | Decode a strict 'ByteString' containing UTF-8 encoded text.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With lenientDecode

-- | Convert a 'Either' into a 'Maybe', using the 'Right' as 'Just' and silencing the 'Left' val as 'Nothing'.
hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | Convert a 'Maybe' computation to 'MaybeT'.
--
-- __NOTE:__ This is also defined (& exported) in @transformers-0.6.0.0@, so should be removed once we upgrade to it.
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure
