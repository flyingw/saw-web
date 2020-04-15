module App.View
  ( viewClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (catMaybes, head)
import Data.JSDate (parse, now, getTime, toISOString, fromTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component)
import React.DOM (text, div, ul, li, a, label, input, button, form, span, table, thead, tbody, th, tr, td, small)
import React.DOM.Props (href, onClick, htmlFor, _type, _id, value)
import React.SyntheticEvent (preventDefault) as R
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (throw)
import Data.String (take)

import Api.Pull (encodePull, Pull(GetFreeDrivers))
import Api.Push (decodePush, Push(LoginOk, FreeDrivers), UserData, DriverInfo)
import Lib.React(cn)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS
import Format (todayDateISO, formatTime)

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { tab :: Tab
  , date :: String
  , drivers :: Array DriverInfo
  }

type This = ReactThis Props State 

data Tab = ViewD | ViewP

derive instance eqTab :: Eq Tab
tabKey :: Tab -> String
tabKey ViewD = "key.drivers"
tabKey ViewP = "key.passengers"

viewClass :: ReactClass Props
viewClass = component "View" \this -> do
  date  <- todayDateISO
  props <- getProps this
  pure
    { state:
      { tab: ViewD
      , date: date
      , drivers: []
      }
    , render: render this
    , componentDidMount: do
        p  <- getProps this
        _  <- fetchDrivers this
        WS.onMsg p.ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right msg -> handleMsg this msg.val
        ) (sequence <<< map error)
    }
  where

  fetchDrivers :: This -> Effect Unit
  fetchDrivers this = do
    p  <- getProps this
    s  <- getState this
    d  <- parse s.date
    WS.send p.ws $ encodePull $ GetFreeDrivers { date: getTime d }

  handleMsg :: This -> Push -> Effect Unit
  handleMsg this (FreeDrivers r) = modifyState this _{ drivers = r.freeDrivers }
  handleMsg this _ = pure unit

  driversList :: Array DriverInfo -> Effect (Array ReactElement)
  driversList drivers =
    map catMaybes $ sequence $ map (\di -> do
      t <- formatTime $ fromTime di.date
      pure $ head di.routes <#> (\route ->
        tr []
        [ td [] 
          [ small [ cn "d-block"] [ text $ route.fromAddress ]
          , small [ cn "d-block"] [ text $ route.toAddress ]
          ]
        , td []
          [ text t
          ]
        ]
      )
    ) drivers


  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    dl    <- driversList state.drivers
    pure $ div []
      [ ul [ cn "nav nav-pills nav-pills-primary justify-content-center" ] $
        map (\t ->
          li []
          [ a [ cn $ "nav-link" <> if t == state.tab then " active" else ""
              , href "#"
              , onClick \e -> (R.preventDefault e) >>= \_ -> modifyState this _{ tab = t }
              ]
            [ text $ props.keyText $ tabKey t ]
          ]
        ) [ViewD, ViewP]
      , case state.tab of
          ViewD ->
            div [ cn "m-2" ]
            [ div [ cn "row" ]
              [ div [ cn "input-group mb-3 col-md-6" ]
                [ div [ cn "input-group-prepend" ]
                  [ span [ cn "input-group-text" ] 
                    [ text $ props.keyText "key.date" 
                    ]
                  ]
                , input [ _type "date", cn "form-control", _id "date"
                        , value state.date
                        , onChangeValue \v -> modifyState this _{ date=v }
                        ]
                , div [ cn "input-group-append" ]
                  [ button [ cn "btn btn-outline-secondary", _type "button"
                            , onClick \_ -> fetchDrivers this 
                            ] 
                      [ text $ props.keyText "key.search"
                      ]
                  ]
                ]
              ]
            , div [ cn "row" ]
              [ table [ cn "table table-borderless table-hover col-md-6" ]
                [ thead [] 
                  [ tr []
                    [ th [] [ text $ props.keyText "key.address" ]
                    , th [] [ text $ props.keyText "key.time" ]
                    ]
                  ]
                , tbody [] dl
                ]
              ]
            ]
          ViewP -> text "2"
      ]
