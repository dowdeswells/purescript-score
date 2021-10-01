module Test.Main where

import GameState
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "GameState" do
      test "create new game" do
        Assert.equal true true
