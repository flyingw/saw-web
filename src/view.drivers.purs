module App.View.Drivers
  ( driversClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (catMaybes, head)
import Data.JSDate (JSDate, fromTime, getTime, now)
import Data.Maybe (Maybe(Just))
import Data.Traversable (sequence)
import Effect (Effect)
import React (ReactClass, ReactElement, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (a, button, div, iframe, small, text)
import React.DOM.Props (_type, frameBorder, height, href, key, onClick, src, width)

import Api.Pull (Pull(GetFreeDrivers))
import Api.Push (Push(FreeDrivers), UserData, DriverInfo)

import Lib.Datepicker (datepickerClass, toLocaleTimeString)
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
  , drivers :: Array DriverInfo
  , showItem :: String
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

driversClass :: ReactClass Props
driversClass = component "View.Drivers" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { date: date
      , drivers: []
      , showItem: ""
      , unsub: pure unit
      }
    , render: render this
    , componentDidMount: do
        _      <- fetchDrivers this
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (FreeDrivers r)) = modifyState this _{ drivers = r.freeDrivers }
  onMsg this _                      = pure unit

  fetchDrivers :: This -> Effect Unit
  fetchDrivers this = do
    p  <- getProps this
    s  <- getState this
    WS.snd p.ws $ GetFreeDrivers { date: getTime s.date }

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    dl    <- driversList this
    pure $
      div []
      [ div [ cn "d-flex justify-content-center row mb-3" ]
        [ createLeafElement datepickerClass { onChange: \d -> modifyState this _{ date = d }
                                            , lang: props.lang
                                            , showTime: false
                                            , className: "form-control"
                                            , wrapperClassName: "form-control col-6 col-sm-5 col-md-4 col-lg-3 mr-2"
                                            , _id: "date"
                                            , keyText: props.keyText
                                            }
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
      t <- toLocaleTimeString $ fromTime di.date
      pure $ head di.routes <#> (\route ->
        div [ cn "list-group-item", key di.id ]
        [ div [ cn "d-flex flex-row" ]
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
