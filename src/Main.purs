module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCodePointArray, toCodePointArray)
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Exception (throw)
import EnterScore as ES
import EnterTeams as ET
import GameState as GS
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState, (/\), mkReducer, useReducer)
import React.Basic.Hooks as React
import ScoresView (scoreView)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  body <- body =<< document =<< window
  case body of
    Nothing -> throw "Could not find body."
    Just b -> do
      app <- mkApp
      render (app {}) (toElement b)

mkApp :: Component {}
mkApp = do
  component "app" \_ -> React.do
    gameState /\ setGameState <- useState GS.Initial
    let
      onClickOk teams = setGameState \_ -> GS.newGame teams --handler_ (logShow "here")
      onAddScore i = do
          setGameState \_ -> GS.addScore (GS.Score i) gameState
          logShow gameState
    pure
      $ case gameState of
          GS.Initial ->
            R.div_
              [ ET.enterTeams
                  { onOk: onClickOk
                  }
              ]
          gameScore ->
            R.div_
              [ R.text "Game has started"
              , R.div_
                  [ ES.enterScore { onOk: onAddScore }
                  ]
              , R.div_ [
                scoreView {gameState:gameScore}
              ]
              ]

reverse :: String -> String
reverse = fromCodePointArray <<< Array.reverse <<< toCodePointArray
