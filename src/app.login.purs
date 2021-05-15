module App.Login
  ( loginClass
  , Props
  ) where

import Prelude hiding (div)

import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Console (error)
import Foreign (Foreign, F, readNumber, readString, typeOf)
import Foreign.Index ((!)) as F
import Foreign.Keys (keys) as F
import React (ReactClass, getProps, getState, component, ReactThis, modifyState)
import React.DOM (text, div, h6, span, input, button)
import React.DOM.Props (_id, onClick, _type, value, placeholder)
import Proto.Uint8Array (wrap, unwrap)
import Lib.React (cn, onChangeValue)
import Lib.Ajax (postEff)

import Api.Pull (encodePull, Pull(TelegramLogin, SimpleLogin), TelegramData(TelegramString, TelegramNum))
import Api.Push (decodePush, Push(LoginOk, LoginErr), UserData)

type Props =
  { await :: Effect Unit
  , ok :: Effect Unit
  , err :: Effect Unit
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { err :: Boolean
  , simpleLogin :: Boolean
  , f1 :: String
  , f2 :: String
  }

type This = ReactThis Props State

loginClass :: ReactClass Props
loginClass = component "Home" \this -> do
  props <- getProps this
  pure
    { state: 
      { err: false 
      , simpleLogin: false
      , f1: ""
      , f2: ""
      }
    , render: render this
    , componentDidMount: do
        p <- getProps this
        case p.user of
          Nothing  -> telegramLoginWidget "login-widget" (login 
                        (modifyState this _{ err = false } >>= \_ -> p.await) 
                        (modifyState this _{ err = true } >>= \_ -> p.err)
                        (modifyState this _{ err = false } >>= \_ -> p.ok)
                      )
          Just u -> pure unit
    }
  where  
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div []
      [ div [ cn "d-flex justify-content-center mb-3" ] 
        [ h6 [] [ text $ props.keyText "key.home.head" ]
        ]
      , div [ cn "d-flex justify-content-center mb-3" ] 
        [ span [] [ text $ props.keyText "key.home.text" ]
        ]
      , div [ cn $ if (isJust props.user) then "d-none" else "" ] 
        [ if state.err
            then 
              div [ cn "d-flex justify-content-center" ]
              [ div [ cn "alert alert-danger d-inline-block" ] 
                [ text $ props.keyText "key.err" ]
              ]
            else mempty
        , div [ cn $ "d-flex justify-content-center mb-3", _id "login-widget"] []
        , div [ cn "d-flex justify-content-center mb-3" ] 
          [ div [ cn "alert alert-info" ] 
            [ text $ props.keyText "key.home.login.hint"
            , span [ onClick \_ -> modifyState this _{ simpleLogin = true }
                   ] 
              [ text "." ]
            ]
          ]
        ]
      , if state.simpleLogin then
          div [ cn "" ] 
          [ div [ cn "d-flex justify-content-center mb-3" ] 
            [ div [ cn "col-md-6 col-lg-4 mb-3" ]
              [ input [ _type "text", cn "form-control", _id "f1"
                      , value state.f1
                      , onChangeValue \v -> modifyState this _{ f1 = v }
                      , placeholder "1"
                      ]
              ]
            ]
          , div [ cn "d-flex justify-content-center mb-3" ]
            [ div [ cn "col-md-6 col-lg-4 mb-3" ]
              [ input [ _type "text", cn "form-control", _id "f2"
                      , value state.f2
                      , onChangeValue \v -> modifyState this _{ f2 = v }
                      , placeholder "2"
                      ]
              ]
            ]
          , div [ cn "d-flex justify-content-center mb-3" ]
            [ button [ cn "btn btn-primary mb-3", _type "button"
                      , onClick \_ ->
                          postEff "//ridehub.city/login2" (unwrap $ encodePull $ SimpleLogin { f1: state.f1, f2: state.f2 }) (
                            \_ -> props.err
                          ) (
                            \_ -> props.ok
                          )
                      ]
              [ text "ok" ]
            ]
          ]
        else
          mempty
      ]

foreign import telegramLoginWidget :: String -> (Foreign -> Effect Unit) -> Effect Unit

login :: Effect Unit -> Effect Unit -> Effect Unit -> Foreign -> Effect Unit
login await err ok user = do
  _ <- await
  case runExcept $ f user of
    Left e -> error (show e) >>= \_ -> err
    Right msg ->
      postEff "//ridehub.city/login" (unwrap $ encodePull msg) (
        \_ -> err
      ) (
        \x -> case decodePush $ wrap x of
          Left e -> error (show e) >>= \_ -> err
          Right { val: LoginOk } -> ok
          Right { val: LoginErr } -> err
          Right _ -> err
      )
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
