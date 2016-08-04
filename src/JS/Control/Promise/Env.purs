module JS.Control.Promise.Env
  ( PromiseEnv
  , lift
  , resolve
  , reject
  , all
  , race
  , delay
  , run
  , getEnv
  ) where

import Prelude (($), (<<<), (>>>))

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (class Apply, lift2, (<*>))
import Control.Applicative (class Applicative)
import Control.Bind (class Bind, bind)
import Control.Monad (class Monad)
import Control.Monad.Aff (Aff)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.MonadPlus (class MonadZero, class MonadPlus)
import Control.Parallel.Class (class MonadRace, class MonadPar, stall)

import Control.Plus (class Plus)

import Data.Either (Either, isLeft)
import Data.Functor (class Functor, map, (<$>))
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.Unit (Unit)

import JS.Control.Promise (Promise, PROMISE)
import JS.Control.Promise as Promise

-- | An asynchronous computation with effects `e` and environment `env`
-- | This is the moral equivalent of StateT env Promise,
-- | Except we also add in a notion of effects, since we're dealing with lazy values

newtype PromiseEnv (e :: # !) env a = PromiseEnv (env -> Promise a)

unwrap :: forall e env a. PromiseEnv e env a -> (env -> Promise a)
unwrap (PromiseEnv p) = p

applyEnv :: forall e env a. env -> PromiseEnv e env a -> Promise a
applyEnv env = unwrap >>> (_ $ env)

lift :: forall e env a. Promise a -> PromiseEnv e env a
lift a = PromiseEnv \_ -> a

resolve :: forall e env a. a -> PromiseEnv e env a
resolve a = PromiseEnv \_ -> Promise.resolve a

reject :: forall e env a. Error -> PromiseEnv e env a
reject e = PromiseEnv \_ -> Promise.reject e

all :: forall e env a. Array (PromiseEnv e env a) -> PromiseEnv e env (Array a)
all ps = PromiseEnv \env -> Promise.all (map (applyEnv env) ps)

race :: forall e env a. Array (PromiseEnv e env a) -> PromiseEnv e env a
race ps = PromiseEnv \env -> Promise.race (map (applyEnv env) ps)

delay :: forall e env. Number -> PromiseEnv e env Unit
delay = lift <<< Promise.delay

run :: forall e env a. env -> PromiseEnv e env a -> Aff ( promise :: PROMISE | e ) a
run env (PromiseEnv p) = Promise.run $ p env

getEnv :: forall e env. PromiseEnv e env env
getEnv = PromiseEnv \env -> Promise.resolve env

-- | Type class instances

-- | Semigroup (<>)

instance semigroupPromiseEnv :: (Semigroup a) => Semigroup (PromiseEnv e env a) where
  append = lift2 append

-- | Monoid (mempty)

instance monoidPromiseEnv :: (Monoid a) => Monoid (PromiseEnv e env a) where
  mempty = resolve mempty

-- | Functor (map, <$>)

instance functorPromiseEnv :: Functor (PromiseEnv e env) where
  map f (PromiseEnv p) = PromiseEnv \env -> f <$> p env

-- | Apply (<*>)

instance applyPromiseEnv :: Apply (PromiseEnv e env) where
  apply (PromiseEnv f) (PromiseEnv x) = PromiseEnv \env -> f env <*> x env

-- | Applicative (pure)

instance applicativePromiseEnv :: Applicative (PromiseEnv e env) where
  pure = resolve

-- | Alt (<|>)

instance altPromiseEnv :: Alt (PromiseEnv e env) where
  alt (PromiseEnv x) (PromiseEnv y) = PromiseEnv \env -> x env <|> y env

-- | Plus (Alt and empty)

instance plusPromiseEnv :: Plus (PromiseEnv e env) where
  empty = reject (error "empty promise env")

-- | Alternative (Applicative and Plus)

instance alternative :: Alternative (PromiseEnv e env)

-- | Bind (>>=)

instance bindPromiseEnv :: Bind (PromiseEnv e env) where
  bind x k = PromiseEnv \env -> do
    x' <- applyEnv env x
    applyEnv env (k x')

-- | Monad (do notation)

instance monadPromiseEnv :: Monad (PromiseEnv e env)

-- | MonadEff (liftEff)

foreign import _liftEff :: forall e env a. Eff e a -> PromiseEnv e env a

instance monadEffPromiseEnv :: MonadEff e (PromiseEnv e env) where
  liftEff = _liftEff

-- | MonadPlus (distributivity of <|> over >>=)

instance monadPlusPromiseEnv :: MonadPlus (PromiseEnv e env)
instance monadZeroPromiseEnv :: MonadZero (PromiseEnv e env)

-- | MonadRec

instance monadRecPromiseEnv :: MonadRec (PromiseEnv e env) where
  tailRecM f a = PromiseEnv \env -> tailRecM (f >>> applyEnv env) a

-- | Allows users to catch and throw errors

foreign import _catch :: forall e env a. PromiseEnv e env a
                      -> (Error -> PromiseEnv e env a)
                      -> PromiseEnv e env a

instance monadErrorPromiseEnv :: MonadError Error (PromiseEnv e env) where
  throwError = reject
  catchError = _catch

-- | Allows a call with continuation

foreign import _callCC :: forall e env a b
                        . ((a -> PromiseEnv e env b) -> PromiseEnv e env a)
                       -> PromiseEnv e env a

instance monadContPromiseEnv :: MonadCont (PromiseEnv e env) where
  callCC = _callCC

-- | Monad Parallel

-- less bindings than the lift2 implementation,
-- although they're morally the same
foreign import _par :: forall e env a b c
                     . (a -> b -> c)
                    -> PromiseEnv e env a
                    -> PromiseEnv e env b
                    -> PromiseEnv e env c

instance monadParPromiseEnv :: MonadPar (PromiseEnv e env) where
  par = _par

instance monadRacePromiseEnv :: MonadRace (PromiseEnv e env) where
  stall = PromiseEnv \_ -> stall
  race a b = race [a, b]
