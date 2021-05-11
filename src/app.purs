module App where

import Prelude hiding (div)

import Data.Array (take, drop)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Foldable (find)
import Data.String.Common (joinWith, split, toLower, trim, null)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Class.Console (infoShow)
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (a, button, div, img, li, nav, option, select, span, text, ul)
import React.DOM.Props (_type, height, href, onClick, src, style, value, _id)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, location)
import Web.HTML.Location (protocol)

import Api.Push (Push(SessionData), UserData, UserStatus(Active, Guest))
import Api.Pull (Pull(Logout))
import Lib.Maps(loadMap)
import Lib.Datepicker (datepickerLoad)
import Lib.React (cn, onChangeValue)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Lib.Ajax (getEff)

import App.Add (addClass)

import App.Add.Menu (addMenuClass)
import App.Login (loginClass)
import App.Home (homeClass)
import App.View (viewClass)
import App.ConfirmRegistration (confirmRegistrationClass)

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

data MenuItem = Loading | LoginItem | HomeItem | ViewItem | AddItem | AddItem2
instance showMenuItem :: Show MenuItem where
  show :: MenuItem -> String
  show Loading    = ""
  show LoginItem  = "key.menu.login"
  show HomeItem   = "key.menu.home"
  show ViewItem   = "key.menu.view"
  show AddItem    = "key.menu.add"
  show AddItem2    = "key.menu.add"
derive instance eqMenuItem :: Eq MenuItem
derive instance ordMenuItem :: Ord MenuItem

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { user: Nothing
      , lang: "uk"
      , keyText: \key -> key
      , menuItem: LoginItem
      , expandMenu: false
      }:: State
    , render: render this
    , componentDidMount: do
        props <- getProps this
        _     <- setLang this "uk"
        void $ WS.sub props.ws $ onMsg this
        -- void $ loadMap "div-map"
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (SessionData { status: Active r })) = do
    s <- getState this
    case s.menuItem of
      LoginItem -> modifyState this _{ menuItem = HomeItem, user = Just r.user }
      _         -> modifyState this _{ user = Just r.user }
  onMsg this (Just (SessionData { status: Guest }))    =
    modifyState this _{ menuItem = LoginItem, user = Nothing }
  onMsg this _                                         = pure unit

  setLang :: This -> String -> Effect Unit
  setLang this lang = do
    getEff ("langs/" <> lang <> ".js") \v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> split (Pattern "=") <#> \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, keyText = \key -> fromMaybe key $ Map.lookup (toLower key) keys }

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ 
      div []
      [ nav [ cn "navbar navbar-expand-lg navbar-dark bg-primary" ]
        [ a [ cn "navbar-brand", href "#", onClick \_ -> modifyState this _{ menuItem = HomeItem } ] [ text "Ridehub" ]
        , ul [ cn "navbar-nav ml-auto nav-flex-icons d-lg-none" ] [ userIcon state.user ]
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
            ) (menuItems state.user)
          , select  [ cn "custom-select bg-primary text-white-50", style { width: "9.5rem" }
                    , onChangeValue \v -> setLang this v
                    , value state.lang
                    ] $
            map (\v -> option [ value v ] [ text $ state.keyText $ "key." <> v ]) [ "uk", "ru" ]
          ]
        , ul [ cn "navbar-nav ml-auto nav-flex-icons d-none d-lg-inline" ] [ userIcon state.user ]
        ]
      , div [ cn "m-3"] $ case state.menuItem of
          Loading    -> [ div [] [] ]
            -- [ div [ cn "d-flex justify-content-center form-row" ]
            --   [ div [ cn "col-md-10 col-lg-6 mb-3" ]
            --     [ div [ _id "div-map", style { height: "300px" } ] []
            --     ]
            --   ]
            -- ]
          LoginItem  -> [ createLeafElement loginClass { await: pure unit
                                                       , ok: WS.reconnect props.ws
                                                       , err: pure unit
                                                       , keyText: state.keyText
                                                       , user: state.user
                                                       }
                        ]
          HomeItem   -> [ createLeafElement homeClass { ws: props.ws
                                                      , lang: state.lang
                                                      , keyText: state.keyText
                                                      , user: state.user
                                                      , logout: do
                                                           WS.snd props.ws Logout
                                                           modifyState this _{ menuItem = LoginItem, user = Nothing }
                                                      }
                        ]
          ViewItem   -> [ createLeafElement viewClass { ws: props.ws, lang: state.lang, keyText: state.keyText, user: state.user }
                        ]
          AddItem    -> [ createLeafElement addMenuClass { ws: props.ws, lang: state.lang, keyText: state.keyText, user: state.user }
                        ]
          AddItem2   -> [ createLeafElement addClass { ws: props.ws, lang: state.lang, keyText: state.keyText, user: state.user }
                        ]
      ]

  menuItems :: Maybe UserData -> Array MenuItem
  menuItems (Just _) = [ HomeItem, AddItem, AddItem2, ViewItem ]
  menuItems Nothing  = [ LoginItem ]

  userIcon :: Maybe UserData -> ReactElement
  userIcon (Just userData @ { photo: Just photo }) =
    li []
    [ span [ cn "nav-link p-0" ]
      [ img [ src photo, cn "rounded-circle z-depth-0 mr-1", height "35" ]
      , text $ showUserName userData
      ]
    ]
  userIcon (Just userData) =
    li [ cn "nav-item" ]
    [ span [ cn "nav-link" ]
      [ text $ showUserName userData
      ]
    ]
  userIcon Nothing =
    li [ cn "nav-item" ] []

showUserName :: UserData -> String
showUserName userData = do
  let items = [ trim $ fromMaybe "" userData.username
              , trim $ (fromMaybe "" userData.firstName) <> " " <> (fromMaybe "" userData.lastName)
              ]
  fromMaybe "Anonymous" $ find (trim >>> null >>> not) items

view :: Effect Unit
view = do
  _ <- datepickerLoad
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  protocol' <- window >>= location >>= protocol
  let port = if protocol' == "https:" then "" else ":8001"
  ws <- WS.new $ "ridehub.city"<>port<>"/ws"
  let element = createLeafElement appClass { ws }
  void $ render element container
