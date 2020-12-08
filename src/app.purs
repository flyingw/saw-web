module App where

import Prelude hiding (div)

import Control.Alt ((<|>))
import Data.Array (take, drop)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String.Common (joinWith, split, toLower)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Exception (throw)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (a, button, div, img, li, nav, option, select, span, text, ul)
import React.DOM.Props (_type, height, href, onClick, src, style, value)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Ajax (getEff)

import Api.Push (Push(SessionData), UserData)

import Lib.Datepicker (datepickerLoad)
import Lib.React (cn, onChangeValue)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

import App.Add (addClass)
import App.Home (homeClass)
import App.View (viewClass)

type Props =
  { ws :: Ws
  }

type State =
  { user :: Maybe UserData
  , lang :: String
  , keyText :: String -> String
  , menuItem :: MenuItem
  , expandMenu :: Boolean
  }

type This = ReactThis Props State

data MenuItem = HomeItem | ViewItem | AddItem
instance showMenuItem :: Show MenuItem where
  show :: MenuItem -> String
  show HomeItem = "key.menu.home"
  show ViewItem = "key.menu.view"
  show AddItem  = "key.menu.add"
derive instance eqMenuItem :: Eq MenuItem
derive instance ordMenuItem :: Ord MenuItem

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { user: Nothing
      , lang: "uk"
      , keyText: \key -> key
      , menuItem: HomeItem
      , expandMenu: false
      }:: State
    , render: render this
    , componentDidMount: do
        props <- getProps this
        _     <- setLang this "uk"
        void $ WS.sub props.ws $ onMsg this
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (SessionData r)) = modifyState this _{ user = Just r.user }
  onMsg this _                      = pure unit

  setLang :: This -> String -> Effect Unit
  setLang this lang = do
    getEff ("langs/" <> lang <> ".js") \v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, keyText = \key -> fromMaybe key $ Map.lookup (toLower key) keys }

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    u     <- userIcon this
    pure $ div []
      [ nav [ cn "navbar navbar-expand-lg navbar-dark bg-primary" ]
        [ a [ cn "navbar-brand", href "#", onClick \_ -> modifyState this _{ menuItem = HomeItem } ] [ text "Ridehub" ]
        , ul [ cn "navbar-nav ml-auto nav-flex-icons d-lg-none" ] [ u ]
        , button [ cn "navbar-toggler collapsed", _type "button" 
                 , onClick \_ -> modifyState this \s -> s { expandMenu = not s.expandMenu }
                 ] 
          [ span [ cn "navbar-toggler-icon" ] [] 
          ]
        , div [ cn $ "navbar-collapse" <> if state.expandMenu then "" else " collapse" ]
          [ ul [ cn "navbar-nav mr-auto" ] $
            map (\v ->
              li [ cn $ "nav-item" <> if v == state.menuItem then " active" else "" ]
              [ a [ cn "nav-link", href "#"
                  , onClick \_ -> modifyState this _{ menuItem = v, expandMenu = false }
                  ] 
                [ text $ state.keyText $ show v ]
              ]
            ) [ HomeItem, AddItem, ViewItem ]
          , select  [ cn "custom-select bg-primary text-white-50", style { width: "9.5rem" }
                    , onChangeValue \v -> setLang this v
                    , value state.lang
                    ] $
            map (\v -> option [ value v ] [ text $ state.keyText $ "key." <> v ]) [ "uk", "ru" ]
          ]
        , ul [ cn "navbar-nav ml-auto nav-flex-icons d-none d-lg-inline" ] [ u ]
        ]
      , div [ cn "m-3"] $ case state.menuItem of
          HomeItem -> [ createLeafElement homeClass { await: pure unit
                                                    , ok: WS.reconnect props.ws
                                                    , err: pure unit
                                                    , keyText: state.keyText
                                                    , user: state.user
                                                    }
                      ]
          ViewItem -> [ createLeafElement viewClass { ws: props.ws, lang: state.lang, keyText: state.keyText, user: state.user }
                      ]
          AddItem  -> [ createLeafElement addClass { ws: props.ws, lang: state.lang, keyText: state.keyText, user: state.user }
                      ]
      ]

  userIcon :: This -> Effect ReactElement
  userIcon this = do
    state <- getState this
    pure $ case state.user of
      Just user @ { photo: Just photo } ->
        li [ cn "avatar" ]
        [ span [ cn "nav-link p-0" ]
          [ img [ src photo, cn "rounded-circle z-depth-0 mr-1", height "35" ]
          , text $ fromMaybe "" $ user.firstName <|> user.lastName 
          ]
        ]
      Just user ->
        li [ cn "nav-item" ]
        [ span [ cn "nav-link" ]
          [ text $ fromMaybe "" $ user.firstName <|> user.lastName ]
        ]
      Nothing ->
        li [ cn "nav-item" ]
        [ a [ cn "nav-link", href "#", onClick \_ -> modifyState this _{ menuItem = HomeItem } ]
          [ text $ state.keyText "key.login" ]
        ]

view :: Effect Unit
view = do
  _         <- datepickerLoad
  doc       <- window >>= document
  elem      <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  ws        <- endpoint >>= WS.new
  let element = createLeafElement appClass { ws }
  void $ render element container

foreign import endpoint :: Effect String
