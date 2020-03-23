module App.Driver
  ( driverClass
  , Props
  ) where

import Lib.WebSocket as WS
import Lib.WebSocket (WebSocket)
import React (ReactClass, getProps, getState, modifyState, component)
import Effect.Console (error)
import Data.Traversable (sequence)
import Prelude hiding (div)
import React.DOM (text, div)
import Api.Push (decodePush, Push(Pong))
import Data.Either (Either(Left, Right))

type Props =
  { ws :: WebSocket
  }

type State =
  { answer :: String
  }

driverClass :: ReactClass Props
driverClass = component "Driver" \this -> do
  pure
    { state:
      { answer: "press the button"
      }
    , render: render this
    , componentDidMount: do
        props <- getProps this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right { val: Pong } -> modifyState this _{ answer = "got answer" }
        ) (sequence <<< map error)
    }
  where
  render this = ado
    props <- getProps this
    state <- getState this
    in div []
      [ text state.answer
      ]
