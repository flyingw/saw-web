module App.View
  ( viewClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (catMaybes, head)
import Data.JSDate (parse, now, getTime, toISOString, fromTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (text, div, ul, li, a, label, input, button, form, span, table, thead, tbody, th, tr, td, small, iframe)
import React.DOM.Props (href, onClick, htmlFor, _type, _id, value, key, height, frameBorder, width, src)
import React.SyntheticEvent (preventDefault) as R
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Data.String (take)

import Api.Pull (encodePull, Pull(GetFreeDrivers))
import Api.Push (decodePush, Push(LoginOk, FreeDrivers), UserData, DriverInfo)
import Lib.React(cn)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS
import Format (todayDateISO, formatTime)
import App.View.Drivers (driversClass)
import App.View.Riders (ridersClass)

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
  date  <- todayDateISO
  props <- getProps this
  pure
    { state:
      { tab: ViewD
      }
    , render: render this
    }
  where
  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div []
      [ ul [ cn "nav nav-pills nav-pills-primary justify-content-center" ] $
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
