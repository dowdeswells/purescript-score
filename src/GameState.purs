module GameState where

import Prelude
import Control.Plus (empty)
import Data.Foldable (lookup)
import Data.List (List(..), filter, head, foldr)
import Data.Maybe (Maybe)

newtype Score
  = Score Int

derive newtype instance semiringScore :: Semiring Score
derive newtype instance eqScore :: Eq Score
derive newtype instance showScore :: Show Score

data TeamScore = TeamScore String (List Score)

instance showTeamScore :: Show TeamScore where
  show (TeamScore n l) = "Team: " <> n <> " = " <> (show l)

data GameScore = GameScore (List TeamScore)

instance showGameScore :: Show GameScore where
  show (GameScore l) = "Game: " <> (show l)

newGame :: List String -> GameScore
newGame names = GameScore $ foldr (\tn gs -> Cons (newTeam tn) gs) Nil names
  where newTeam tn = TeamScore tn Nil

addScore :: Score -> TeamScore -> TeamScore
addScore s (TeamScore n scores) = TeamScore n (Cons s scores)

-- lookupTeam :: String -> GameScores -> Maybe TeamScores
-- lookupTeam teamName = head $ filter (\e -> (_.teamName e) == teamName
