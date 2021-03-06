module Test.JS.Control.Promise where

import Prelude
import Control.Monad.Cont
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Data.Either
import Test.Unit
import Test.Unit.Assert as Assert

import JS.Control.Promise (Promise)
import JS.Control.Promise (resolve, run) as Promise

promiseSuite = suite "JS.Control.Promise" do

  suite "Monad.Rec" do
    let go :: _ -> Promise (Either _ Number)
        go { acc, n } | n <= 0.0  = Promise.resolve (Right acc)
                      | otherwise = Promise.resolve (Left { acc: acc + n, n: n - 1.0 })
        rec n = tailRecM go { acc: 0.0, n }
    test "call stack not exceeded" do
      n <- Promise.run $ rec 100000.0
      Assert.equal n 5000050000.0

  suite "Monad.Error" do
    test "it works" do
      let try :: Promise String
          try = throwError $ error "This is an error!"
          catch :: Error -> Promise String
          catch _ = Promise.resolve "Ting"
      msg <- Promise.run $ catchError try catch
      Assert.equal msg "Ting"

  suite "Monad.Cont" do
    let validate "" exit = exit "You forgot to tell me your name!"
        validate name exit = pure unit
        welcome name = callCC \exit -> do
          validate name exit
          pure $ "Welcome, " <> name <> "!"
    test "it exits when necessary" do
      msg <- Promise.run $ welcome ""
      Assert.equal msg "You forgot to tell me your name!"
    test "it avoids exiting when necessary" do
      msg <- Promise.run $ welcome "James"
      Assert.equal msg "Welcome, James!"
