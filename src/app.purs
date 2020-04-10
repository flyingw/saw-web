module App where

import Prelude (Unit, show, bind, discard, map, pure, void, unit, mempty, (<<<), ($), (>>=), (/=), (<$>), (<#>), (<>))

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array (filter, sort, catMaybes, take, drop)
import Data.Either (Either(..))
import Data.Functor ((<#>))
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString) as Number
import Data.Semigroup (append)
import Data.Maybe (fromMaybe)
import Data.String.Common (joinWith, split, toLower)
import Data.String.Pattern (Pattern(Pattern))
import Data.Map as Map
import Data.Tuple (Tuple(Tuple))
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

import Ajax (getEff)
import Lib.React (cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api.Pull (encodePull, Pull(Ping, LoginAttempt))
import Api.Push (decodePush, Push(LoginOk))

import App.Add (addClass)

type Props =
  { ws :: WebSocket
  }

type State =
  { sessionid :: Maybe String
  , lang :: String
  , keyText :: String -> String
  }

appClass :: ReactClass Props
appClass = component "App" \this -> do
  pure
    { state: 
      { sessionid: mempty
      , lang: "uk"
      , keyText: \key -> key
      }:: State
    , render: render this
    , componentDidMount: do
        props  <- getProps this
        state  <- getState this
        _      <- setLang this state.lang
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right { val: LoginOk {sessionid}} -> modifyState this _{ sessionid=Just sessionid }
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
      [ button  [ _type "button"
                , cn "btn btn-primary"
                , onClick \_ -> WS.send ws $ encodePull Ping
                ] [ text "Send" ]
      , createLeafElement addClass {ws: ws, lang: state.lang, keyText: state.keyText}
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
    hash' <- x F.! "hash" >>= readNullOrUndefined >>= traverse readString
    hash <- maybe (F.fail $ ForeignError "no hash") pure hash'
    auth_date' <- x F.! "auth_date" >>= readNullOrUndefined >>= traverse readNumber
    auth_date <- maybe (F.fail $ ForeignError "no auth_date") pure auth_date'
    keys' <- F.keys x
    let keys = sort $ filter (_ /= "hash") keys'
    xs <- sequence $ map (\k -> x F.! k >>= readNull >>= traverse readStringLike <#> map (append k <<< append "=")) keys
    let data_check_string = joinWith "\n" $ catMaybes xs
    name <- x F.! "first_name" >>= readNullOrUndefined >>= traverse readString
    pure $ LoginAttempt { data_check_string, hash, auth_date, name }
    where
    readStringLike :: Foreign -> F String
    readStringLike y = readString y <|> (map Number.toString $ readNumber y)
