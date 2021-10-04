module EnterScore where

import Data.Maybe (Maybe(..), fromMaybe)
import Prelude

import Data.Int (fromString)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type Props
  = { onOk :: Int -> Effect Unit
    }

type InputState
  = { text :: String, value :: Int
    }

enterScore :: Props -> React.JSX
enterScore = unsafePerformEffect mkEnterScore

mkEnterScore :: Component Props
mkEnterScore = do
  component "EnterScore" \props -> React.do
    text /\ setText <- useState { text: "", value: 0 }
    pure
      $ R.div_
          [ R.input
              { type: "text"
              , className: "border-gray-500 border-2 rounded p-2"
              , placeholder: "score"
              , value: text.text
              , onChange:
                  let
                    handleValue mv = setText (\_ -> convertInput mv)
                  in              
                  handler targetValue handleValue
              }
          , R.button
              { type: "button"
              , className: "rounded bg-blue-500 text-white p-2"
              , onClick:
                  let
                    handleValue _ = do
                      props.onOk text.value
                      setText \_ -> { text: "", value: 0 }
                  in              
                  handler targetValue handleValue
                    
                    
              , children: [ R.text "Ok" ]
              }
          ]


convertInput :: Maybe String -> InputState
convertInput s = 
  let 
    cs = fromMaybe "" s
  in
    { text: cs, value: case fromString cs of 
                    Nothing -> 0
                    Just v -> v } 
