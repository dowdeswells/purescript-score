module Test.Main where

import Prelude

import Data.List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import GameState

main :: Effect Unit
main = do
  runTest do
    suite "GameState" do
      test "create new game" do
        let actual = len g
              where
              g = newGame $ ["Simon","Lara","Sam"]
              len (GameScore l) = foldl (\acc _ -> acc + 1) 0 l
              len Initial = 0
        Assert.equal 3 actual

      test "get a Team from a game by team name" do
        let name = getName team
              where 
              team = lookupTeam "Simon" (newGame $ ["Simon","Lara","Sam"]) 
        Assert.equal "Simon" name

      test "get a Team from a game by index" do
        let name = getName team
              where 
              team = lookupTeamByIndex 0 (newGame $ ["Simon","Lara","Sam"]) 
        Assert.equal "Simon" name
      
      test "replace a team name" do
        Assert.equal ["Mum","Lara","Sam"] (replaceListElement 0 "Mum" ["Simon","Lara","Sam"]) 
 

            
      
getName :: Maybe TeamScore -> String
getName t = case t of
            Just (TeamScore tn _) -> tn
            Nothing -> ""   