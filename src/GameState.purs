module GameState where

import Prelude

import Data.Array (mapWithIndex)
import Data.List (List(..), filter, head, foldr, index)
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


getTeamName :: TeamScore -> String
getTeamName (TeamScore tn _) = tn

getTeamScore :: TeamScore -> List Score
getTeamScore (TeamScore _ l) = l

lookupTeam :: String -> GameScore -> Maybe TeamScore
lookupTeam teamName (GameScore l) = head $ filter (\e -> (getTeamName e) == teamName) l

lookupTeamByIndex :: Int -> GameScore -> Maybe TeamScore
lookupTeamByIndex i (GameScore l) = index l i

newGame :: List String -> GameScore
newGame names = GameScore $ foldr (\tn gs -> Cons (newTeam tn) gs) Nil names
  where newTeam tn = TeamScore tn Nil

addScore :: Score -> TeamScore -> TeamScore
addScore s (TeamScore n scores) = TeamScore n (Cons s scores)

replaceListElement :: Int -> String -> Array String -> Array String
replaceListElement i s = mapWithIndex (\li e -> rep li i s e)
    where rep li i s e = case li == i of
                            true -> s
                            false -> e