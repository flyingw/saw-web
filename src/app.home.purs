module App.Home
  ( homeClass
  , Props
  ) where

import Prelude hiding (div)

import Control.Monad.Except (runExcept)
import Data.Array (take, drop, mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe, isJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Foreign (Foreign, F, readNumber, readString, typeOf)
import Foreign.Index ((!)) as F
import Foreign.Keys (keys) as F
import React (ReactClass, getProps, getState, component, ReactThis)
import React.DOM (text, div, h6, span)
import React.DOM.Props (_id, _type, onClick, name, checked, href, src, height, async)

import Api.Pull (encodePull, Pull(TelegramLogin), TelegramData(TelegramString, TelegramNum))
import Api.Push (UserData)
import Lib.React (cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = {}

type This = ReactThis Props State

homeClass :: ReactClass Props
homeClass = component "Home" \this -> do
  props <- getProps this
  pure
    { state: {}
    , render: render this
    , componentDidMount: do
        p <- getProps this
        case p.user of
          Nothing -> telegramLoginWidget "login-widget" (login this)
          Just u -> pure unit
    }
  where  
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div [ cn "m-2" ]
      [ div [ cn "d-flex justify-content-center mb-3" ] 
        [ h6 [] [ text $ props.keyText "key.home.head" ]
        ]
      , div [ cn "d-flex justify-content-center mb-3" ] 
        [ span [] [ text $ props.keyText "key.home.text" ]
        ]
      , div [ cn $ if (isJust props.user) then "d-none" else "" ] 
        [ div [ cn $ "d-flex justify-content-center mb-3", _id "login-widget"] []
        , div [ cn "d-flex justify-content-center mb-3" ] 
          [ div [ cn "alert alert-info" ] 
            [ text $ props.keyText "key.home.login.hint"
            ]
          ]
        ]
      ]

foreign import telegramLoginWidget :: String -> (Foreign -> Effect Unit) -> Effect Unit

login :: This -> Foreign -> Effect Unit
login this user = do
  props <- getProps this
  case runExcept $ f user of
    Left x -> throw $ show x
    Right msg -> WS.send props.ws $ encodePull msg
  where
  f :: Foreign -> F Pull
  f x = do
    keys'  <- F.keys x
    xs     <- sequence $ map (\k -> x F.! k <#> \v -> Tuple k v) keys'
    tds    <- sequence $ mapMaybe (\(Tuple k v) ->
                case typeOf v of
                  "string" -> Just $ readString v <#> \s -> TelegramString { key: k, value: s }
                  "number" -> Just $ readNumber v <#> \n -> TelegramNum { key: k, value: n }
                  _        -> Nothing
              ) xs
    pure $ TelegramLogin { d: tds }
