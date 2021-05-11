module App.Home
  ( homeClass
  , Props
  ) where

import Prelude hiding (div)

import Prelude hiding (div, min, max)

import Data.Array (fromFoldable, elem, delete, (:))
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, ul, li)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked)

import Api (PassengerType(..))
import Api.Pull (Pull(AddDriver, Logout), Address)
import Api.Push (Push(AddRouteOk, AddRouteErr), UserData)
import Keys (keyPassengerType)
import Lib.Datepicker (datepickerClass)
import Lib.JS (encodeURI)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Proto.BigInt (fromNumber)

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  , logout :: Effect Unit
  }

type State = 
  { unsub :: Effect Unit
  }

type This = ReactThis Props State

homeClass :: ReactClass Props
homeClass = component "Home" \this -> do
  props <- getProps this
  pure
    { state: 
      { unsub: pure unit
      }
    , render: render this
    , componentDidMount: do
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
    }
  where  
  render this = do
    props <- getProps this
    state <- getState this
    pure $ 
      div []
      [ div [ cn "d-flex justify-content-center" ]
        [ button [ cn "btn btn-primary mb-3"
                 , _type "button" 
                 , onClick \_ -> props.logout
                 ]
          [ text $ props.keyText "key.logout" ]
        ]
      ]

  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this _ = pure unit