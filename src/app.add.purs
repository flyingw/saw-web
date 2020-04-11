module App.Add
  ( addClass
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

type State = 
  { tab :: Tab
  }

data Tab = AddD | AddR

derive instance eqTab :: Eq Tab
tabKey :: Tab -> String
tabKey AddD = "key.driver"
tabKey AddR = "key.passenger"

addClass :: ReactClass Props
addClass = component "Add" \this -> do
  props <- getProps this
  pure
    { state:
      { tab: AddD
      }
    , render: render this
    }
  where  
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div []
      [ ul [ cn "nav nav-tabs" ] $
        map (\t ->
          li [ cn "nav-item" ]
            [ a [ cn $ "nav-link" <> if t == state.tab then " active" else ""
                , href "#"
                , onClick \e -> (R.preventDefault e) >>= \_ -> modifyState this _{ tab = t }
                ]
              [ text $ props.keyText $ tabKey t ]
            ]
        ) [AddD, AddR]
      , case state.tab of
          AddD -> createLeafElement driverClass props
          AddR -> createLeafElement riderClass props
      ]
