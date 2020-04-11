module App.Home
  ( homeClass
  , Props
  ) where

import Prelude hiding (div)

import React (ReactClass, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (text, div, ul, li, a)
import React.DOM.Props (href, onClick)
import React.SyntheticEvent (preventDefault) as R

import Lib.React(cn)
import Lib.WebSocket (WebSocket)

import App.Driver (driverClass)
import App.Rider (riderClass)

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
