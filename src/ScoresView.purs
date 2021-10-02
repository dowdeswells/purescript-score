module ScoresView where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int (fromString, toStringAs, decimal)
import Data.List (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import GameState (GameScore(..), TeamScore(..), Score(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React
import Web.HTML.Event.EventTypes (offline)

type Props
  = { gameState :: GameScore
    }

scoreView :: Props -> React.JSX
scoreView = unsafePerformEffect mkScoreView

mkScoreView :: Component Props
mkScoreView = do
  component "ScoreView" \props -> React.do
    -- text /\ setText <- useState { text: "", value: 0 }
    pure
      $ R.div_
          [ 
            case props.gameState of
                Initial -> R.text "Game Not Started"
                GameScore teamScores teamIndex ->
                    R.div
                        { children:
                            Array.fromFoldable $ mapWithIndex
                                ( \i (TeamScore teamName scores) ->
                                    R.div_
                                    [
                                        R.div {
                                            children: [ R.text teamName]
                                        }
                                        ,R.div {
                                            children: 
                                                Array.fromFoldable $ mapWithIndex 
                                                    (\scoreIndex (Score s) -> R.text (toStringAs decimal s))
                                                    scores
                                        }
                                    ]
                                )
                                teamScores
                        }
          ]


