module App where

import Prelude (Unit, apply, show, bind, discard, map, pure, void, unit, mempty, (<<<), ($), (>>=), (/=))

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array (filter, sort, catMaybes)
import Data.Either (Either(..))
import Data.Functor ((<#>))
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString) as Number
import Data.Semigroup (append)
import Data.String.Common (joinWith)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Foreign (Foreign, F, ForeignError(..), readNull, readNumber, readString, readNullOrUndefined)
import Foreign (fail) as F
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
  , name :: Maybe String
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
          Right { val: LoginOk { sessionid, name }} -> modifyState this _{ sessionid=Just sessionid, name=name }
          Right _ -> pure unit
        ) (sequence <<< map error)
    }
  where
  render :: ReactThis Props State -> Effect ReactElement
  render this = ado
    props <- getProps this
    let ws = props.ws
    state <- getState this
    let name = state.name
    in div []
      [ button  [ _type "button"
                , cn "btn btn-primary"
                , onClick \_ -> WS.send ws $ encodePull Ping
                ] [ text "Send" ]
      , createLeafElement driverClass { ws, name }
      , createLeafElement riderClass { ws, name }
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
  pure \user -> case runExcept $ f user of
    Left x -> throw $ show x
    Right msg -> do
      WS.send ws $ encodePull msg
  where
  f :: Foreign -> F Pull
  f x = do
    hash' <- x F.! "hash" >>= readNullOrUndefined >>= traverse readString
    hash <- maybe (F.fail $ ForeignError "no hash") pure hash'
    auth_date' <- x F.! "auth_date" >>= readNullOrUndefined >>= traverse readNumber
    auth_date <- maybe (F.fail $ ForeignError "no auth_date") pure auth_date'
    keys' <- F.keys x
    let keys = sort $ filter (_ /= "hash") keys'
    xs <- sequence $ map (\k -> x F.! k >>= readNull >>= traverse readStringLike <#> map (append k)) keys
    let data_check_string = joinWith "\n" $ catMaybes xs
    name <- x F.! "first_name" >>= readNullOrUndefined >>= traverse readString
    pure $ LoginAttempt { data_check_string, hash, auth_date, name }
    where
    readStringLike :: Foreign -> F String
    readStringLike y = readString y <|> (map Number.toString $ readNumber y)
