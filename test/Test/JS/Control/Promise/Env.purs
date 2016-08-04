module Test.JS.Control.Promise.Env where

import Prelude
import Control.Monad.Cont
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Data.Either

import Test.Unit
import Test.Unit.Assert as Assert

import JS.Control.Promise.Env (PromiseEnv)
import JS.Control.Promise.Env as PromiseEnv

promiseEnvSuite = suite "JS.Control.Promise.Env" do

  test "env gets read" do
    n <- PromiseEnv.run { x: 5, y: 12 } do
      { x, y } <- PromiseEnv.getEnv
      pure (x + y)
    Assert.equal n 17

  suite "Monad.Rec" do
    let go :: _ -> PromiseEnv _ Unit (Either _ Number)
        go { acc, n } | n <= 0.0  = PromiseEnv.resolve (Right acc)
                      | otherwise = PromiseEnv.resolve (Left { acc: acc + n, n: n - 1.0 })
        rec n = tailRecM go { acc: 0.0, n }
    test "call stack not exceeded" do
      n <- PromiseEnv.run unit $ rec 100000.0
      Assert.equal n 5000050000.0

  suite "Monad.Error" do
    test "it works" do
      let try :: PromiseEnv _ Unit String
          try = throwError $ error "This is an error!"
          catch :: Error -> PromiseEnv _ Unit String
          catch _ = PromiseEnv.resolve "Ting"
      msg <- PromiseEnv.run unit $ catchError try catch
      Assert.equal msg "Ting"


  suite "Monad.Cont" do
    let validate "" exit = exit "You forgot to tell me your name!"
        validate name exit = pure unit
        welcome name = callCC \exit -> do
          validate name exit
          pure $ "Welcome, " <> name <> "!"
    test "it exits when necessary" do
      msg <- PromiseEnv.run unit $ welcome ""
      Assert.equal msg "You forgot to tell me your name!"
    test "it avoids exiting when necessary" do
      msg <- PromiseEnv.run unit $ welcome "James"
      Assert.equal msg "Welcome, James!"
