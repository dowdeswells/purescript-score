module ScoresView where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Int (fromString, toStringAs, decimal)
import Data.List (List(..))
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
    pure
      $ R.div_
          [ case props.gameState of
              Initial -> R.text "Game Not Started"
              GameScore teamScores teamIndex ->
                R.div
                  { children:
                      Array.fromFoldable
                        $ map
                            ( \(TeamScore teamName scores total) ->
                                R.div
                                  { children:
                                      [ R.div
                                          { children: [ R.text teamName ]
                                          }
                                      , R.div
                                          { className: "flex flex-wrap flex-row gap-6"
                                          , children:
                                              Array.fromFoldable
                                                $ Cons (scoreDiv total)
                                                $ map (\s -> scoreDiv s) scores
                                          }
                                      ]
                                  }
                            )
                            teamScores
                  }
          ]

scoreDiv :: Score -> React.JSX
scoreDiv (Score s) =
  R.div
    { className: "flex-shrink border-2 border-blue-800 bg-blue-500"
    , children: [ R.text (toStringAs decimal s) ]
    }
