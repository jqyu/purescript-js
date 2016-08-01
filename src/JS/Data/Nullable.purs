module JS.Data.Nullable
  ( Nullable
  , null
  , isNull
  , isNotNull
  , nullable
  , nullable'
  , fromNullable
  , fromNullable'
  , fromMaybe
  , toMaybe
  ) where

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply, (<*>))
import Control.Bind (class Bind)
import Control.Extend (class Extend)
import Control.Monad (class Monad)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)

import Data.Bounded (class Bounded, top)
import Data.Eq (class Eq, eq)
import Data.Function (const, id)
import Data.Functor (class Functor, (<$>))
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Maybe(Maybe(..), maybe)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord, compare)
import Data.Ordering (Ordering(..))
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)

-- | The `Nullable` type represents a native js value that can possibly be
-- | null or undefined. This offers a more performant version of `Maybe`,
-- | at the cost of no longer being able to pattern match.

-- | This module might as well be straight copypasta of purescript-maybe

foreign import data Nullable :: * -> *

foreign import null :: forall a. Nullable a
foreign import isNull :: forall a. Nullable a -> Boolean
foreign import isNotNull :: forall a. Nullable a -> Boolean
foreign import nullable :: forall a b. b -> (a -> b) -> Nullable a -> b
foreign import nullable_ :: forall a b. (Unit -> b) -> (a -> b) -> Nullable a -> b

nullable' :: forall a b. (Unit -> b) -> (a -> b) -> Nullable a -> b
nullable' = nullable_

fromNullable :: forall a. a -> Nullable a -> a
fromNullable a = nullable a id

fromNullable' :: forall a. (Unit -> a) -> Nullable a -> a
fromNullable' a = nullable' a id

fromMaybe :: forall a. Maybe a -> Nullable a
fromMaybe = maybe null _pure

toMaybe :: forall a. Nullable a -> Maybe a
toMaybe = nullable Nothing Just

-- | Functor (map, <$>)

foreign import _map :: forall a b. (a -> b) -> Nullable a -> Nullable b

instance functorNullable :: Functor Nullable where
  map = _map

-- | Apply (<*>)

foreign import _apply :: forall a b. Nullable (a -> b) -> Nullable a -> Nullable b

instance applyNullable :: Apply Nullable where
  apply = _apply

-- | Applicative (pure)

foreign import _pure :: forall a. a -> Nullable a

instance applicativeNullable :: Applicative Nullable where
  pure = _pure

-- | Alt (<|>)

foreign import _alt :: forall a. Nullable a -> Nullable a -> Nullable a

instance altNullable :: Alt Nullable where
  alt = _alt

-- | Plus (empty)

instance plusNullable :: Plus Nullable where
  empty = null

-- | Alternative (Applicative and Plus)

instance alternativeNullable :: Alternative Nullable

-- | Bind (>>=)

foreign import _bind :: forall a b. Nullable a -> (a -> Nullable b) -> Nullable b

instance bindNullable :: Bind Nullable where
  bind = _bind

-- | Monad (do notation)

instance monadNullable :: Monad Nullable

instance monadZeroNullable :: MonadZero Nullable

-- | Extend (<<=)

foreign import _extend :: forall a b. (Nullable a -> b) -> Nullable a -> Nullable b

instance extendNullable :: Extend Nullable where
  extend = _extend

instance invariantNullable :: Invariant Nullable where
  imap = imapF

-- | Semigroup (<>)

instance semigroupNullable :: Semigroup a => Semigroup (Nullable a) where
  append x y = append <$> x <*> y

instance monoidNullable :: Semigroup a => Monoid (Nullable a) where
  mempty = null

-- | Eq (==)

foreign import _eq :: forall a. (a -> a -> Boolean) -> Nullable a -> Nullable a -> Boolean

instance eqNullable :: Eq a => Eq (Nullable a) where
  eq = _eq eq

-- | Ord (>, >=, <, <=)
-- | where forall a: null < a

foreign import _compare :: forall a. (a -> a -> Ordering) -> Ordering -> Ordering -> Ordering
                        -> Nullable a -> Nullable a -> Ordering

instance ordNullable :: Ord a => Ord (Nullable a) where
  compare = _compare compare EQ LT GT

instance boundedNullable :: Bounded a => Bounded (Nullable a) where
  top = _pure top
  bottom = null

-- | Show

instance showNullable :: Show a => Show (Nullable a) where
  show = nullable "Null" \x -> "(Nullable " <> show x <> ")"
