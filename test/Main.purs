module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit.Main

import Test.JS.Control.Promise (promiseSuite)
import Test.JS.Data.Nullable (nullableSuite)

main = runTest do
  promiseSuite
  nullableSuite
