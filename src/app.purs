module App where

import Prelude (Unit, apply, bind, discard, map, pure, void, ($), (<>), (>>=))

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState)
import React.DOM (button, div, text)
import React.DOM.Props (_type, onClick)
import ReactDOM (render)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, alert)

import Lib.React (cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api.Pull (encodePull, Pull(Ping))

import App.Driver (driverClass)
import App.Rider (riderClass)

type Props =
  { ws :: WebSocket
  }

type State = {}

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: {}
    , render: render this
    }
  where
  render :: ReactThis Props State -> Effect ReactElement
  render this = ado
    props <- getProps this
    let ws = props.ws
    state <- getState this
    in div []
      [ button  [ _type "button"
                , cn "btn btn-primary"
                , onClick \_ -> WS.send ws $ encodePull Ping
                ] [ text "Send" ]
      , createLeafElement driverClass { ws }
      , createLeafElement riderClass { ws }
      ]

view :: Effect ({ first_name :: String } -> Effect Unit)
view = do
  container <- byId "container"
  ws <- WS.create "127.0.0.1:8001"
  WS.onOpen ws \_ -> WS.setBinary ws
  let element = createLeafElement appClass { ws }
  void $ render element container
  pure \user -> window >>= alert user.first_name
  where
  byId :: String -> Effect Element
  byId id = do
    doc <- window >>= document
    let d = toNonElementParentNode doc
    e :: Maybe Element <- getElementById id d
    case e of
      Just e' -> pure e'
      Nothing -> throw $ "not found="<>id

