module App.SelectPoint
  ( selectPointClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (fromFoldable, elem, delete, (:))
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, datalist, option, select, form, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, list, selected, autoComplete)
import Effect.Class.Console (infoShow)

import Api.Pull (Pull(GetUserData), Address)
import Api.Push (Push(UserDataOk), UserData)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

type Props =
  { ws :: Ws
  , keyText :: String -> String
  , lang :: String
  , done :: Effect Unit
  }

type State =
  { unsub :: Effect Unit
  }

type This = ReactThis Props State

selectPointClass :: ReactClass Props
selectPointClass = component "SelectPoint" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { unsub: pure unit
      } :: State
    , render: render this
    , componentDidMount: do
        WS.sub props.ws (onMsg this) >>= \unsub -> modifyState this _{ unsub = unsub }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this _ = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div [] []