module EnterTeams where

import Prelude

import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
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
      $ R.div
          { className:"p-6 bg-blue-100 rounded"
            ,children:
              [ 
                R.h1 { className:"text-xl text-blue-800", children:[R.text "Configure teams for the game"]}
                ,R.div
                  { className: "flex py-4"
                  , children:
                      [ R.button
                          { type: "button"
                          , className: "flex-none rounded bg-blue-500 text-white p-2"
                          , onClick: handler_ (setTeams \_ -> (addBlankTeam teams))
                          , children: [ R.text "Add Team" ]
                          }
                      , R.div { className: "flex-grow" }
                      , R.button
                          { type: "button"
                          , className: "flex-none rounded bg-blue-500 text-white p-2"
                          , onClick: handler_ (props.onOk teams)
                          , children: [ R.text "Ok" ]
                          }
                      ]
                  }
              , R.div
                  { className: "grid grid-cols-1"
                  , children:
                      mapWithIndex
                        ( \i teamName ->
                            let
                              handleValue mv = setTeams \_ -> (replaceTeam i (fromMaybe "" mv) teams)
                            in
                              buildInput teamName handleValue
                        )
                        teams
                  }
              ]
          }

buildInput :: String -> (Maybe String -> Effect Unit) -> JSX
buildInput teamName handleValue =
  R.input
    { type: "text"
    , className: "border-gray-500 border-2 rounded p-2"
    , placeholder: "team name"
    , value: teamName
    , onChange: handler targetValue handleValue
    }

replaceTeam :: Int -> String -> Array String -> Array String
replaceTeam i s =
  mapWithIndex
    ( \li e -> case li == i of
        true -> s
        false -> e
    )

addBlankTeam :: Array String -> Array String
addBlankTeam = Array.cons ""
