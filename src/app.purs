module App where

import Prelude hiding (div)

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array (filter, sort, catMaybes, take, drop)
import Data.Either (Either(..))
import Data.Maybe (maybe, fromMaybe)
import Data.Number.Format (toString) as Number
import Data.String.Common (joinWith, split, toLower)
import Data.String.Pattern (Pattern(Pattern))
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Foreign (Foreign, F, readNull, readNumber, readString, readNullOrUndefined)
import Foreign.Keys (keys) as F
import Foreign.Index ((!)) as F
import React (ReactClass, ReactElement, ReactThis, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (div, text, nav, ul, li, a, label, input)
import React.DOM.Props (_type, onClick, name, checked, href)
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Ajax (getEff)
import Lib.React (cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api.Pull (encodePull, Pull(LoginAttempt))
import Api.Push (decodePush)

import App.Home (homeClass)
import App.Add (addClass)

type Props =
  { ws :: WebSocket
  }

type State =
  { lang :: String
  , keyText :: String -> String
  , menuItem :: MenuItem
  }

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
      { lang: "uk"
      , keyText: \key -> key
      , menuItem: HomeItem
      }:: State
    , render: render this
    , componentDidMount: do
        props  <- getProps this
        state  <- getState this
        _      <- setLang this state.lang
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right _ -> pure unit
        ) (sequence <<< map error)
    }
  where
  setLang :: ReactThis Props State -> String -> Effect Unit
  setLang this lang = do
    getEff ("/langs/" <> lang <> ".js") (\err -> pure unit)(\v -> do
      let keys = Map.fromFoldable $ split (Pattern "\n") v <#> 
                                    split (Pattern "=") <#> 
                                    \kv -> Tuple (joinWith "" $ take 1 kv) (joinWith "" $ drop 1 kv)
      modifyState this _ { lang = lang, keyText = \key -> fromMaybe key $ Map.lookup (toLower key) keys }
    )

  render :: ReactThis Props State -> Effect ReactElement
  render this = do
    props <- getProps this
    let ws = props.ws
    state <- getState this
    pure $ div []
      [ nav [ cn "navbar navbar-expand-lg navbar-light bg-light" ]
        [ ul [ cn "navbar-nav mr-auto mt-2 mt-lg-0" ] $
            map (\v ->
              li [ cn $ "nav-item" <> if v == state.menuItem then " active" else "" ]
              [ a [ cn "nav-link", href "#"
                  , onClick \_ -> modifyState this _{ menuItem = v }
                  ] 
                [ text $ state.keyText $ show v ]
              ]
            ) [ HomeItem, ViewItem, AddItem ]
        , div [ cn "btn-group btn-group-sm btn-group-toggle" ] $
            map (\v ->
              label [ cn $ "btn btn-secondary" <> if state.lang == v then " active" else "" ]
              [ input [ _type "radio", name "options", checked $ state.lang == v
                      , onClick \_ -> setLang this v
                      ]
              , text $ state.keyText $ "key." <> v
              ]
            ) [ "uk", "ru" ]
        ]
      , div [ cn "m-2"] $
          case state.menuItem of
            HomeItem -> [ createLeafElement homeClass {ws: ws, lang: state.lang, keyText: state.keyText} ]
            ViewItem -> [
              
                        ]
            AddItem  -> [ createLeafElement addClass {ws: ws, lang: state.lang, keyText: state.keyText}
                        ]
      ]

view :: Effect (Foreign -> Effect Unit)
view = do
  doc <- window >>= document
  elem <- getElementById "container" $ toNonElementParentNode doc
  container <- maybe (throw "container not found") pure elem
  ws <- WS.create "ridehub.city/ws"
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
    hash              <- x F.! "hash" >>= readString
    auth_date         <- x F.! "auth_date" >>= readNumber
    keys'             <- F.keys x
    keys              <- pure $ sort $ filter (_ /= "hash") keys'
    xs                <- sequence $ map (\k -> x F.! k >>= readNull >>= traverse readStringLike <#> map (append k <<< append "=")) keys
    data_check_string <- pure $ joinWith "\n" $ catMaybes xs
    name              <- x F.! "first_name" >>= readNullOrUndefined >>= traverse readString
    id                <- x F.! "id" >>= readStringLike
    pure $ LoginAttempt { data_check_string, hash, auth_date, name, id }
    where
    readStringLike :: Foreign -> F String
    readStringLike y = readString y <|> (map Number.toString $ readNumber y)
