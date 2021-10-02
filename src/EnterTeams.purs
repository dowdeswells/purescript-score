module EnterTeams where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.String (fromCodePointArray, toCodePointArray)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (SyntheticEvent, handler, handler_)
import React.Basic.Hooks (Component, JSX, component, mkReducer, useReducer, useState, (/\))
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

type Props
  = { onOk :: Array String -> Effect Unit
    }

enterTeams :: {} -> React.JSX
enterTeams = unsafePerformEffect mkEnterTeams

mkEnterTeams :: Component {}
mkEnterTeams = do
  component "EnterTeams" \_ -> React.do
    teams /\ setTeams <- useState [""]
    pure
      $ R.div_
          [ R.div
              { children:
                  mapWithIndex
                    ( \i teamName ->
                        R.input
                          { type: "text"
                          , placeholder: "team name"
                          , value: teamName
                          , onChange: handler targetValue $ traverse_ \str -> setTeams \_ -> (replaceTeam i str teams)
                          }
                    )
                    teams
              }
          , R.p {}
          , R.button
              { type: "button"
              , onClick: handler_ (setTeams \_ -> (addBlankTeam teams))
              , children: [ R.text "Add Team" ]
              }
          , R.div
              { children: teams <#> \n -> R.text n
              }
          ]

replaceTeam :: Int -> String -> Array String -> Array String
replaceTeam i s = mapWithIndex (\li e -> rep li i s e)
  where
  rep li i s e = case li == i of
    true -> s
    false -> e

addBlankTeam :: Array String -> Array String
addBlankTeam = Array.cons ""
