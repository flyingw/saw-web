module App.View
  ( viewClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component)
import React.DOM (text, div, ul, li, a)
import React.DOM.Props (href, onClick)
import React.SyntheticEvent (preventDefault) as R
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)

import Api.Pull (encodePull)
import Api.Push (decodePush, Push(LoginOk), UserData)
import Lib.React(cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { tab :: Tab
  , drivers :: Array Unit
  }

type This = ReactThis Props State 

data Tab = ViewD | ViewP

derive instance eqTab :: Eq Tab
tabKey :: Tab -> String
tabKey ViewD = "key.drivers"
tabKey ViewP = "key.passengers"

viewClass :: ReactClass Props
viewClass = component "View" \this -> do
  props <- getProps this
  pure
    { state:
      { tab: ViewD
      , drivers: []
      }
    , render: render this
    , componentDidMount: do
        p  <- getProps this
        s  <- getState this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right msg -> handleMsg this msg.val
        ) (sequence <<< map error)

    }
  where

  handleMsg :: This -> Push -> Effect Unit
  handleMsg this msg = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ div []
      [ ul [ cn "nav nav-tabs" ] $
        map (\t ->
          li [ cn "nav-item" ]
            [ a [ cn $ "nav-link" <> if t == state.tab then " active" else ""
                , href "#"
                , onClick \e -> (R.preventDefault e) >>= \_ -> modifyState this _{ tab = t }
                ]
              [ text $ props.keyText $ tabKey t ]
            ]
        ) [ViewD, ViewP]
      , case state.tab of
          ViewD -> text "1"
          ViewP -> text "2"
      ]
