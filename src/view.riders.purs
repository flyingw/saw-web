module App.View.Riders
  ( ridersClass
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

import Api.Pull (encodePull, Pull(GetFreePassengers))
import Api.Push (decodePush, Push(FreePassengers), UserData, PassengerInfo)
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
  , passengers :: Array PassengerInfo
  , showItem :: String
  }

type This = ReactThis Props State 

ridersClass :: ReactClass Props
ridersClass = component "View.Passengers" \this -> do
  date  <- todayDateISO
  props <- getProps this
  pure
    { state:
      { date: date
      , passengers: []
      , showItem: ""
      }
    , render: render this
    , componentDidMount: do
        p <- getProps this
        _ <- fetchPassengers this
        WS.onMsg p.ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right msg -> handleMsg this msg.val
        ) (sequence <<< map error)
    }
  where
  fetchPassengers :: This -> Effect Unit
  fetchPassengers this = do
    p  <- getProps this
    s  <- getState this
    d  <- parse s.date
    WS.send p.ws $ encodePull $ GetFreePassengers { date: getTime d }

  handleMsg :: This -> Push -> Effect Unit
  handleMsg this (FreePassengers r) = modifyState this _{ passengers = r.freePassengers }
  handleMsg this _ = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    dl    <- passengersList this
    pure $
      div []
      [ div [ cn "d-flex justify-content-center mb-3" ]
        [ input [ _type "date", cn "form-control col-6 col-sm-5 col-md-4 col-lg-3 mr-2", _id "date"
                , value state.date
                , onChangeValue \v -> modifyState this _{ date=v }
                ]
        , button [ cn "btn btn-outline-secondary", _type "button" 
                 , onClick \_ -> fetchPassengers this 
                 ]
          [ text $ props.keyText "key.search" ]
        ]
      , dl
      ]

  passengersList :: This -> Effect ReactElement
  passengersList this = do
    state <- getState this
    props <- getProps this
    map (div [ cn "list-group d-flex flex-column justify-content-center" ]) $ sequence $ map (\pi -> do
      t <- formatTime $ fromTime pi.date
      pure $
        div [ cn "list-group-item", key pi.id ]
        [ div [ cn "d-flex flex-row" ]
          [ div [ cn "mr-3" ]  
            [ small [ cn "d-block" ] [ text $ pi.fromAddress ]
            , small [ cn "d-block" ] [ text $ pi.toAddress ]
            ]
          , div []
            [ small [ cn "d-block" ] [ text t ]
            , small [ cn "d-block" ] 
              [ a [ href "#" 
                    , onClick (\_ -> modifyState this _{ showItem = pi.id })
                  ]
                [ text $ props.keyText "key.show_map" ]
              ]
            ]
          ]  
        , if state.showItem == pi.id 
          then do
            let host = "https://www.google.com/maps/embed/v1/directions"
            let origin = pi.fromAddress
            let destination = pi.toAddress
            let key = "AIzaSyAuq2lMfK8JPYK4-zYYw9Bl8SeTQrKJJeY"
            let q = host <> "?origin=" <> origin <> "&destination=" <> destination <> "&key=" <> key
            iframe [ width "100%", height "400", frameBorder "0", src q ] []
          else
            mempty
        ]
    ) state.passengers
