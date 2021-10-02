module EnterTeams where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Props
  = { onOk :: Array String -> Effect Unit
    }

enterTeams :: Props -> React.JSX
enterTeams = unsafePerformEffect mkEnterTeams

mkEnterTeams :: Component Props
mkEnterTeams = do
  component "EnterTeams" \props -> React.do
    teams /\ setTeams <- useState [ "" ]
    pure
      $ R.div_
          [ R.button
              { type: "button"
              , className: "rounded bg-blue-500 text-white p-2"
              , onClick: handler_ (setTeams \_ -> (addBlankTeam teams))
              , children: [ R.text "Add Team" ]
              }
            , R.button
              { type: "button"
              , className: "rounded bg-blue-500 text-white p-2"
              , onClick: handler_ (props.onOk teams)
              , children: [ R.text "Ok" ]
              }              
          , R.div
              { children:
                  mapWithIndex
                    ( \i teamName ->
                        R.input
                          { type: "text"
                          , className: "border-gray-500 border-2 rounded p-2"
                          , placeholder: "team name"
                          , value: teamName
                          , onChange: handler targetValue $ traverse_ \str -> setTeams \_ -> (replaceTeam i str teams)
                          }
                    )
                    teams
              }
          , R.div
              { children: teams <#> \n -> R.text n
              }
          ]

replaceTeam :: Int -> String -> Array String -> Array String
replaceTeam i s = mapWithIndex (\li e -> case li == i of
                                                true -> s
                                                false -> e)

addBlankTeam :: Array String -> Array String
addBlankTeam = Array.cons ""
