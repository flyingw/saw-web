module App where

import Prelude (Unit, apply, show, bind, discard, map, pure, void, unit, mempty, (<<<), ($), (>>=), (/=), (<>))

import Control.Monad.Except (runExcept)
import Data.Array (filter, sort, mapMaybe, catMaybes)
import Data.Either (Either(..))
import Data.Functor ((<#>))
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup (append)
import Data.String.Common (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Foreign (Foreign, F, readNull, readNumber, readString, unsafeFromForeign)
import Foreign.Keys (keys) as F
import Foreign.Index ((!)) as F
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

import Api.Pull (encodePull, Pull(Ping, LoginAttempt))
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

view :: Effect (Foreign -> Effect Unit)
view = do
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  ws <- WS.create "ec2-54-93-193-191.eu-central-1.compute.amazonaws.com:8443"
  WS.onOpen ws \_ -> WS.setBinary ws
  let element = createLeafElement appClass { ws }
  void $ render element container
  pure \user -> do
    hash <- case runExcept $ user F.! "hash" >>= readNull >>= traverse readString of
      Right (Just x) -> pure x
      _ -> throw "no hash"
    auth_date <- case runExcept $ user F.! "auth_date" >>= readNull >>= traverse readNumber of
      Right (Just x) -> pure x
      _ -> throw "no auth_date"
    let (res :: Either _ String) = runExcept $ do
          keys <- F.keys user
          xs <- sequence $ map (\k -> user F.! k >>= readNull >>= traverse unsafeFromForeign <#> map (append k)) $ sort $ filter (_ /= "hash") keys
          pure $ joinWith "\n" $ catMaybes xs
    data_check_string <- case res of
      Right x -> pure x
      Left x -> throw $ show x
    WS.send ws $ encodePull $ LoginAttempt { data_check_string, hash, auth_date }

--todo: one 'do' for all F _