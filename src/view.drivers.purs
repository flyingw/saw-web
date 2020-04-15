module App.View.Drivers
  ( driversClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (catMaybes, head)
import Data.JSDate (parse, now, getTime, toISOString, fromTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence, traverse)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component)
import React.DOM (text, div, ul, li, a, label, input, button, form, span, table, thead, tbody, th, tr, td, small, iframe)
import React.DOM.Props (href, onClick, htmlFor, _type, _id, value, key, height, frameBorder, width, src)
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
  { date :: String
  , drivers :: Array DriverInfo
  , showItem :: String
  }

type This = ReactThis Props State 

driversClass :: ReactClass Props
driversClass = component "View.Drivers" \this -> do
  date  <- todayDateISO
  props <- getProps this
  pure
    { state:
      { date: date
      , drivers: []
      , showItem: ""
      }
    , render: render this
    , componentDidMount: do
        p <- getProps this
        _ <- fetchDrivers this
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

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    dl    <- driversList this
    pure $
      div [ cn "m-2" ]
      [ div [ cn "d-flex justify-content-center row mb-3" ]
      
        [ input [ _type "date", cn "form-control col-5 col-sm-4 col-md-3 col-lg-2 mr-2", _id "date"
                , value state.date
                , onChangeValue \v -> modifyState this _{ date=v }
                ]
        , button [ cn "btn btn-outline-secondary", _type "button" 
                 , onClick \_ -> fetchDrivers this 
                 ]
          [ text $ props.keyText "key.search" ]
        ]
      , dl
      ]

  driversList :: This -> Effect ReactElement
  driversList this = do
    state <- getState this
    props <- getProps this
    map (div [ cn "list-group d-flex flex-column justify-content-center" ]) $ map catMaybes $ sequence $ map (\di -> do
      t <- formatTime $ fromTime di.date
      pure $ head di.routes <#> (\route ->
        div [ cn "list-group-item", key di.id ]
        [ div [ cn "d-flex flex-row mb-2" ]
          [ div [ cn "mr-3" ]  
            [ small [ cn "d-block" ] [ text $ route.fromAddress ]
            , small [ cn "d-block" ] [ text $ route.toAddress ]
            ]
          , div []
            [ small [ cn "d-block" ] [ text t ]
            , small [ cn "d-block" ] 
              [ a [ href "#" 
                    , onClick (\_ -> modifyState this _{ showItem = di.id })
                  ]
                [ text $ props.keyText "key.show_map" ]
              ]
            ]
          ]  
        , if state.showItem == di.id 
          then do
            let host = "https://www.google.com/maps/embed/v1/directions"
            let origin = route.fromAddress
            let destination = route.toAddress
            let key = "AIzaSyAuq2lMfK8JPYK4-zYYw9Bl8SeTQrKJJeY"
            let q = host <> "?origin=" <> origin <> "&destination=" <> destination <> "&key=" <> key
            iframe [ width "100%", height "400", frameBorder "0", src q ] []
          else
            mempty
        ]
      )
    ) state.drivers
