module App.View
  ( viewClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Maybe (Maybe)
import React (ReactClass, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (a, div, li, text, ul)
import React.DOM.Props (href, onClick)
import React.SyntheticEvent (preventDefault) as R

import Api.Push (UserData)
import App.View.Drivers (driversClass)
import App.View.Riders (ridersClass)
import Lib.React(cn)
import Lib.WebSocket (WebSocket)

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { tab :: Tab
  }

type This = ReactThis Props State 

data Tab = ViewD | ViewP
derive instance eqTab :: Eq Tab
tabKey :: Tab -> String
tabKey ViewD = "key.drivers"
tabKey ViewP = "key.passengers"

viewClass :: ReactClass Props
viewClass = component "View" \this -> do
  pure
    { state: { tab: ViewD }
    , render: do
        props <- getProps this
        state <- getState this
        pure $ div []
          [ ul [ cn "nav nav-pills nav-pills-primary justify-content-center mb-3" ] $
            map (\t ->
              li []
              [ a [ cn $ "nav-link" <> if t == state.tab then " active" else ""
                  , href "#"
                  , onClick \e -> (R.preventDefault e) >>= \_ -> modifyState this _{ tab = t }
                  ]
                [ text $ props.keyText $ tabKey t ]
              ]
            ) [ViewD, ViewP]
          , case state.tab of
              ViewD -> createLeafElement driversClass props
              ViewP -> createLeafElement ridersClass props
          ]
    }
