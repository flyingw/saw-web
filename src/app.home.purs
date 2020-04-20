module App.Home
  ( homeClass
  , Props
  ) where

import Prelude hiding (div)

import Ajax (postEff)
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
import React.DOM (text, div, h6, span)
import React.DOM.Props (_id)

import Api.Pull (encodePull, Pull(TelegramLogin), TelegramData(TelegramString, TelegramNum))
import Api.Push (decodePush, Push(LoginOk, LoginErr), UserData)
import Lib.React (cn)

type Props =
  { await :: Effect Unit
  , ok :: Effect Unit
  , err :: Effect Unit
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { err :: Boolean
  }

type This = ReactThis Props State

homeClass :: ReactClass Props
homeClass = component "Home" \this -> do
  props <- getProps this
  pure
    { state: { err: false }
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
            ]
          ]
        ]
      ]

foreign import telegramLoginWidget :: String -> (Foreign -> Effect Unit) -> Effect Unit

login :: Effect Unit -> Effect Unit -> Effect Unit -> Foreign -> Effect Unit
login await err ok user = do
  _ <- await
  case runExcept $ f user of
    Left e -> error (show e) >>= \_ -> err
    Right msg ->
      postEff "/login" (encodePull msg) (
        \_ -> err
      ) (
        \x -> case decodePush x of
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
