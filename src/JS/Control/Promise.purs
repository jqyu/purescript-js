module JS.Control.Promise
  ( Promise
  , PROMISE
  , resolve
  , reject
  , all
  , race
  , delay
  , runPromise
  ) where

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (class Apply, lift2)
import Control.Applicative (class Applicative)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff, Canceler, nonCanceler)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadPlus (class MonadZero, class MonadPlus)
import Control.Parallel.Class (class MonadRace, class MonadPar)
import Control.Plus (class Plus)

import Data.Either (Either, isLeft)
import Data.Functor (class Functor)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.Unit (Unit)

-- | The `Promise` type constructor is used to represent native promises for pure values.
-- For async effects, refer to the Contextual Promise monad.

foreign import data Promise :: * -> *
foreign import data PROMISE :: !

foreign import resolve :: forall a. a -> Promise a
foreign import reject :: forall a b. b -> Promise a
foreign import all :: forall a. Array (Promise a) -> Promise (Array a)
foreign import race :: forall a. Array (Promise a) -> Promise a
foreign import delay :: Number -> Promise Unit

foreign import _runPromise :: forall e a. Canceler e -> Promise a -> Aff ( promise :: PROMISE | e ) a

runPromise :: forall e a. Promise a -> Aff ( promise :: PROMISE | e ) a
runPromise = _runPromise nonCanceler


-- | Semigroup (<>)

instance semigroupPromise :: (Semigroup a) => Semigroup (Promise a) where
  append = lift2 append

-- | Monoid (mempty)

instance monoidPromise :: (Monoid a) => Monoid (Promise a) where
  mempty = resolve mempty

-- | Functor (map, <$>)

foreign import _map :: forall a b. (a -> b) -> Promise a -> Promise b

instance functorPromise :: Functor Promise where
  map = _map

-- | Apply (<*>)

foreign import _apply :: forall a b. Promise (a -> b) -> Promise a -> Promise b

instance applyPromise :: Apply Promise where
  apply = _apply

-- | Applicative (pure)

instance applicativePromise :: Applicative Promise where
  pure = resolve

-- | Alt (<|>)

foreign import _alt :: forall a. Promise a -> Promise a -> Promise a

instance altPromise :: Alt Promise where
  alt = _alt

-- | Plus (Alt and empty)

instance plusPromise :: Plus Promise where
  empty = reject "empty promise"

-- | Alternative (Applicative and Plus)

instance alternativePromise :: Alternative Promise

-- | Bind (>>=)

foreign import _bind :: forall a b. Promise a -> (a -> Promise b) -> Promise b

instance bindPromise :: Bind Promise where
  bind = _bind

-- | Monad ( do notation )

instance monadPromise :: Monad Promise

-- | MonadPlus (distributivity of <|> over >>=)

instance monadPlusPromise :: MonadPlus Promise
instance monadZeroPromise :: MonadZero Promise

-- | MonadRec

foreign import _tailRecM :: forall a b. (Either a b -> Boolean) -> (a -> Promise (Either a b)) -> a -> Promise b

instance monadRecPromise :: MonadRec Promise where
  tailRecM = _tailRecM isLeft

-- | Allows users to catch and throw errors

foreign import _catch :: forall err a. Promise a -> (err -> Promise a) -> Promise a

instance monadErrorPromise :: MonadError err Promise where
  throwError e = reject e
  catchError = _catch

-- | Allows to call with a continuation

foreign import _callCC :: forall a b. ((a -> Promise b) -> Promise a) -> Promise a

instance monadContPromise :: MonadCont Promise where
  callCC = _callCC

-- | Monad Parallel

instance monadParPromise :: MonadPar Promise where
  par = lift2

foreign import _stall :: forall a. Promise a

instance monadRacePromise :: MonadRace Promise where
  stall = _stall
  race a b = race [a, b]
