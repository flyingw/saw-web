module App.View.Riders
  ( ridersClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Either (Either(..))
import Data.JSDate (JSDate, now, getTime, fromTime)
import Data.Maybe (Maybe)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (error)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (a, button, div, iframe, small, text)
import React.DOM.Props (_type, frameBorder, height, href, key, onClick, src, width)

import Api.Pull (encodePull, Pull(GetFreePassengers))
import Api.Push (decodePush, Push(FreePassengers), UserData, PassengerInfo)
import Datepicker (datepickerClass, toLocaleTimeString)
import Lib.React(cn)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { date :: JSDate
  , passengers :: Array PassengerInfo
  , showItem :: String
  }

type This = ReactThis Props State 

ridersClass :: ReactClass Props
ridersClass = component "View.Passengers" \this -> do
  date  <- now
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
          Right { val: FreePassengers r } -> modifyState this _{ passengers = r.freePassengers }
          Right msg -> pure unit
        ) (sequence <<< map error)
    }
  where
  fetchPassengers :: This -> Effect Unit
  fetchPassengers this = do
    p  <- getProps this
    s  <- getState this
    WS.send p.ws $ encodePull $ GetFreePassengers { date: getTime s.date }

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    dl    <- passengersList this
    pure $
      div []
      [ div [ cn "d-flex justify-content-center mb-3" ]
        [ createLeafElement datepickerClass { onChange: \d -> modifyState this _{ date = d }
                                            , lang: props.lang
                                            , showTime: false
                                            , className: "form-control"
                                            , wrapperClassName: "form-control col-6 col-sm-5 col-md-4 col-lg-3 mr-2"
                                            , _id: "date"
                                            }
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
      t <- toLocaleTimeString $ fromTime pi.date
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
