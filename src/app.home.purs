module App.Home
  ( homeClass
  , Props
  ) where

import Prelude hiding (div)

import React (ReactClass, getProps, getState, component)
import React.DOM (text, div)

import Lib.WebSocket (WebSocket)

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  }

type State = {}

homeClass :: ReactClass Props
homeClass = component "Home" \this -> do
  props <- getProps this
  pure
    { state: {}
    , render: render this
    }
  where  
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div []
      [ div [] [ text "Description here" ]
      , div [] [ text "Login form here" ]
      ]
