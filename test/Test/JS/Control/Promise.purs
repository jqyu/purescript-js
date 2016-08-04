module Test.JS.Control.Promise where

import Prelude
import Data.Either
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Test.Unit
import Test.Unit.Assert as Assert

import JS.Control.Promise (Promise, resolve, runPromise)

promiseSuite = suite "JS.Control.Promise" do

  suite "Monad.Rec" do

    let go :: _ -> Promise (Either _ Number)
        go { acc, n } | n <= 0.0  = resolve (Right acc)
                      | otherwise = resolve (Left { acc: acc + n, n: n - 1.0 })

        rec n = tailRecM go { acc: 0.0, n }

    test "call stack not exceeded" do
      n <- runPromise $ rec 100000.0
      Assert.equal n 5000050000.0
