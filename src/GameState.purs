module GameState where

import Prelude

import Data.Array (mapWithIndex) as DA
import Data.List (List(..), filter, head, foldr, index, length, mapWithIndex)
import Data.Maybe (Maybe(..))

newtype Score
  = Score Int

data TeamScore = TeamScore String (List Score)

data GameScore = 
    Initial
    | GameScore (List TeamScore) Int

derive newtype instance semiringScore :: Semiring Score
derive newtype instance eqScore :: Eq Score
derive newtype instance showScore :: Show Score

instance showTeamScore :: Show TeamScore where
  show (TeamScore n l) = "{Team: " <> n <> " = " <> (show l) <> "}"

instance equalTeamScore :: Eq TeamScore where
  eq (TeamScore n1 l1) (TeamScore n2 l2) = (n1 == n2) && (l1 == l2)

instance showGameScore :: Show GameScore where
  show (GameScore l i) = "Game: " <> (show l) <> " current turn:" <> (show i)
  show Initial = "Not playing"


getTeamName :: TeamScore -> String
getTeamName (TeamScore tn _) = tn

getTeamScore :: TeamScore -> List Score
getTeamScore (TeamScore _ l) = l

lookupTeam :: String -> GameScore -> Maybe TeamScore
lookupTeam teamName (GameScore l _) = head $ filter (\e -> (getTeamName e) == teamName) l
lookupTeam _ Initial = Nothing

lookupTeamByIndex :: Int -> GameScore -> Maybe TeamScore
lookupTeamByIndex i (GameScore l _) = index l i
lookupTeamByIndex _ Initial = Nothing

newGame :: Array String -> GameScore
newGame names = GameScore teams 0 
  where 
  teams = foldr (\tn gs -> Cons (newTeam tn) gs) Nil names
    where
      newTeam tn = TeamScore tn Nil

addScore :: Score -> GameScore -> GameScore
addScore _ Initial = Initial
addScore score (GameScore teams teamIndex) = GameScore newTeams newIndex
  where
    newIndex = (teamIndex + 1) `mod` (length teams)
    newTeams = mapWithIndex (\li ts@(TeamScore teamName scores)-> case li == teamIndex of 
                                                true -> TeamScore teamName (Cons score scores)
                                                false -> ts) teams

replaceListElement :: Int -> String -> Array String -> Array String
replaceListElement i s = DA.mapWithIndex (\li e -> rep li i s e)
    where rep li i s e = case li == i of
                            true -> s
                            false -> e