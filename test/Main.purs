module Test.Main where

import Data.List
import Data.Tuple
import GameState
import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Debug
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "GameState" do
      test "create new game" do
        let actual = len g
              where
              g = newGame $ ["Simon","Lara","Sam"]
        Assert.equal 3 actual
      
      test "add a score to team 0" do
        let Tuple teamScores newTeamIndex = case newGameState of
                    Initial -> Tuple ((TeamScore "" Nil):Nil) 0
                    GameScore ts i -> Tuple ts i
                    where 
                    newGameState = addScore (Score 44) initialGame
        Assert.assert "gamestate is advanced for team at index 0" $ (newTeamIndex == 1) && (head teamScores) == Just (TeamScore "Simon" ((Score 44):Nil))

      test "add a score to team 0 and team 1" do
        let Tuple teamScores newTeamIndex = case newGameState of
                    Initial -> Tuple ((TeamScore "" Nil):Nil) 0
                    GameScore ts i -> Tuple ts i
                    where 
                    newGameState = addScore (Score 55) $ addScore (Score 44) initialGame
        Assert.assert "gamestate is advanced for team at index 1" $
                          (newTeamIndex == 2) 
                          && (head teamScores) == Just (TeamScore "Simon" ((Score 44):Nil))
                          && (head $ (drop 1) teamScores) == Just (TeamScore "Lara" ((Score 55):Nil))

      test "get a Team from a game by team name" do
        let name = getName team
              where 
              team = lookupTeam "Simon" initialGame 
        Assert.equal "Simon" name

      test "get a Team from a game by index" do
        let name = getName team
              where 
              team = lookupTeamByIndex 0 initialGame 
        Assert.equal "Simon" name
      
      test "replace a team name" do
        Assert.equal ["Mum","Lara","Sam"] (replaceListElement 0 "Mum" ["Simon","Lara","Sam"]) 
 

initialGame :: GameScore
initialGame = newGame ["Simon","Lara","Sam"]

len :: GameScore -> Int
len (GameScore l _) = foldl (\acc _ -> acc + 1) 0 l
len Initial = 0            
      
getName :: Maybe TeamScore -> String
getName t = case t of
            Just (TeamScore tn _) -> tn
            Nothing -> ""   