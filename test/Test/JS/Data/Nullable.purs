module Test.JS.Data.Nullable where

import Prelude
import Data.Maybe
import Test.Unit
import Test.Unit.Assert as Assert

import JS.Data.Nullable hiding (fromMaybe, toMaybe)
import JS.Data.Nullable (fromMaybe, toMaybe) as Nullable

nullableSuite = suite "JS.Data.Nullable" do

  suite "Data.Maybe interop" do
    let n1 = pure 5  :: Nullable Int
        n2 = null    :: Nullable Int
        m1 = Just 5  :: Maybe Int
        m2 = Nothing :: Maybe Int
    test "fromMaybe" do
      Assert.equal (Nullable.fromMaybe m1) n1
      Assert.equal (Nullable.fromMaybe m2) n2
    test "toMaybe" do
      Assert.equal (Nullable.toMaybe n1) m1
      Assert.equal (Nullable.toMaybe n2) m2

    -- TODO:
