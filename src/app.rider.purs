module App.Rider
  ( riderClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import Effect.Console (error)
import React (ReactClass, getProps, getState, modifyState, component)
import React.DOM (text, div)

import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api.Push (decodePush, Push(Pong))

type Props =
  { ws :: WebSocket
  }

type State = {}

riderClass :: ReactClass Props
riderClass = component "Rider" \this -> do
  props <- getProps this
  pure
    { state: {}
    , render: render this
    , componentDidMount: do
        props <- getProps this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right _ -> pure unit
        ) (sequence <<< map error)
    }
  where
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div [] [ text "rider" ]
