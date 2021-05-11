module App.Add.Menu
  ( addMenuClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Maybe (Maybe)
import React (ReactClass, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (a, div, li, text, ul)
import React.DOM.Props (href, onClick)
import React.SyntheticEvent (preventDefault) as R

import Lib.React(cn)
import Lib.WebSocket (Ws)
import App.Add.Driver (addDriverClass)
import App.Add.Rider (addRiderClass)
import Api.Push (UserData)

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { tab :: Tab
  }

data Tab = AddDriver | AddRider

derive instance eqTab :: Eq Tab
tabKey :: Tab -> String
tabKey AddDriver = "key.driver"
tabKey AddRider = "key.passenger"

addMenuClass :: ReactClass Props
addMenuClass = component "AddMenu" \this -> do
  props <- getProps this
  pure
    { state:
      { tab: AddDriver
      }
    , render: render this
    }
  where
  render this = do
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
        ) [ AddDriver, AddRider ]
      , case state.tab of
          AddDriver -> createLeafElement addDriverClass props
          AddRider  -> createLeafElement addRiderClass props
      ]
