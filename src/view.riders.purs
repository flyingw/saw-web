module App.View.Riders
  ( ridersClass
  , Props
  ) where

import Prelude hiding (div)

import Data.JSDate (JSDate, now, getTime, fromTime)
import Data.Maybe (Maybe(Just))
import Data.Traversable (sequence)
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (a, button, div, iframe, small, text)
import React.DOM.Props (_type, frameBorder, height, href, key, onClick, src, width)

import Api.Pull (Pull(GetFreePassengers))
import Api.Push (Push(FreePassengers), UserData, PassengerInfo)
import Datepicker (datepickerClass, toLocaleTimeString)
import Lib.React(cn)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State = 
  { date :: JSDate
  , passengers :: Array PassengerInfo
  , showItem :: String
  , unsub :: Effect Unit
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
      , unsub: pure unit
      }
    , render: render this
    , componentDidMount: do
        _      <- fetchPassengers this
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (FreePassengers r)) = modifyState this _{ passengers = r.freePassengers }
  onMsg this _                  = pure unit

  fetchPassengers :: This -> Effect Unit
  fetchPassengers this = do
    p  <- getProps this
    s  <- getState this
    WS.snd p.ws $ GetFreePassengers { date: getTime s.date }

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
