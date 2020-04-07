module App where

import Prelude (Unit, apply, show, bind, discard, map, pure, void, unit, mempty, (<<<), ($), (>>=))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (button, div, text)
import React.DOM.Props (_type, onClick)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Lib.React (cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api.Pull (encodePull, Pull(Ping, LoginAttempt), LoginAttempt)
import Api.Push (decodePush, Push(LoginOk))

import App.Driver (driverClass)
import App.Rider (riderClass)

type Props =
  { ws :: WebSocket
  }

type State =
  { sessionid :: Maybe String
  }

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: mempty :: State
    , render: render this
    , componentDidMount: do
        props <- getProps this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right { val: LoginOk {sessionid}} -> modifyState this _{ sessionid = Just sessionid }
          Right _ -> pure unit
        ) (sequence <<< map error)
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

view :: Effect (LoginAttempt -> Effect Unit)
view = do
  doc <- window >>= document
  let doc' = toNonElementParentNode doc
  elem <- getElementById "container" doc'
  container <- maybe (throw "container not found") pure elem
  ws <- WS.create "ec2-54-93-193-191.eu-central-1.compute.amazonaws.com:8443"
  WS.onOpen ws \_ -> WS.setBinary ws
  let element = createLeafElement appClass { ws }
  void $ render element container
  pure \user -> WS.send ws $ encodePull $ LoginAttempt user
