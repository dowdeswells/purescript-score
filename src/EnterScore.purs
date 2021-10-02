module EnterScore where

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
    text /\ setText <- useState { text:"", value: 0 }
    pure
      $ R.div_
          [ R.button
              { type: "button"
              , className: "rounded bg-blue-500 text-white p-2"
              , onClick: handler_ (props.onOk text.value)
              , children: [ R.text "Ok" ]
              }
          , R.div_
              [ R.input
                  { type: "text"
                  , className: "border-gray-500 border-2 rounded p-2"
                  , placeholder: "score"
                  , value: text.text
                  , onChange: handler targetValue $ traverse_ \str -> setText (\_ -> {text:str, value:0})
                  }
              ]

          ]
