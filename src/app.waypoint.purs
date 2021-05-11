module App.Waypoint
  ( waypointClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.String (length)
import Data.Array (fromFoldable, elem, delete, (:), null)
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (a, i, text, div, label, input, button, h6, small, iframe, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, href, onFocus, onBlur, placeholder)
import Effect.Console (logShow)

import Api (PassengerType(..), Coordinates)
import Api.Pull (Pull(AddDriver, GetAutocomplete), Address)
import Api.Push (Push(AutocompleteOk, AddRouteErr), WaypointType(..), UserData, Waypoint)
import Proto.BigInt (fromNumber)

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Lib.JS (encodeURI)
import Lib.Maps (getLocation)

import Effect.Timer (setTimeout, clearTimeout, TimeoutId)

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  , icon :: String
  , location :: Maybe Coordinates
  , set :: Waypoint -> Effect Unit
  , removeActive :: Boolean
  , remove :: Effect Unit
  }

type State =
  { description :: String
  , coordinates :: Maybe Coordinates
  , timer :: Maybe TimeoutId
  , predictions :: Array Waypoint
  , focus :: Boolean
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

waypointClass :: ReactClass Props
waypointClass = component "Waypoint" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { description: ""
      , coordinates: Nothing
      , timer: Nothing
      , predictions: []
      , focus: false
      , unsub: pure unit
      } :: State
    , render: render this
    , componentDidMount: do
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (AutocompleteOk r)) = do
    s <- getState this
    if s.focus then modifyState this _{ predictions = r.predictions } else pure unit
  onMsg this _ = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div [] 
      [ div [ cn "d-flex justify-content-center" ] 
        [ div [ cn "col-md-8 col-lg-6 mb-3 input-group" ]
          [ div [ cn "input-group-prepend align-self-center" ] 
            [ i [ cn props.icon ] []
            ]
          , input [ _type "text", cn "form-control ml-2", _id "description", required true 
                  , value state.description
                  , onChangeValue \v -> fetchAutocomplete this v
                  , onFocus \_ -> modifyState this _{ focus = true }
                  , onBlur \_ -> modifyState this _{ focus = false }
                  , placeholder $ props.keyText "key.waypoint.input.placeholder"
                  ]
          , if props.removeActive then
              div [ cn "input-group-prepend align-self-center ml-2" ] 
              [ a [ cn "far fa-trash-alt fa-lg"
                  , href "#"
                  , onClick \_ -> props.remove
                  ] []
              ]
            else
              mempty
          , div [ cn $ "dropdown-menu ml-5" <> if not null state.predictions then " show" else "" ] $
              map (\prediction ->
                a [ cn "dropdown-item", href "#", onClick \_ -> modifyState this _{ description = prediction.description, predictions = [] } ]
                [ i [ cn $ waypointIcon prediction.tpe ] []
                , span [ cn "ml-1" ] [ text $ prediction.description ]
                ]
              ) state.predictions
          ]
        ]
      ]

  waypointIcon :: WaypointType -> String
  waypointIcon SubwayWaypoint        = "fas fa-subway fa-lg"
  waypointIcon BusStationWaypoint    = "fas fa-bus-alt fa-lg"
  waypointIcon AirportWaypoint       = "fas fa-plane fa-lg"
  waypointIcon RouteWaypoint         = ""
  waypointIcon StreetAddressWaypoint = "fas fa-building fa-lg"
  waypointIcon LocalityWaypoint      = "fas fa-city fa-lg"
  waypointIcon ShoppingMallWaypoint  = "fas fa-shopping-bag fa-lg"
  waypointIcon PointOfInterest       = "fas fa-map-marker-alt fa-lg"
  waypointIcon UnknownWaypoint       = ""

  fetchAutocomplete :: This -> String -> Effect Unit
  fetchAutocomplete this v = do
    s  <- getState this
    p  <- getProps this
    _  <- modifyState this _{ description=v }
    if length v > 3 
    then do
      _ <- fromMaybe (pure unit) $ map clearTimeout s.timer
      t <- setTimeout 1000 $ WS.snd p.ws $ GetAutocomplete { text: v, lang: p.lang, location: p.location }
      modifyState this _{ timer = Just t }
    else do
      pure unit
  fetchAutocomplete _ _ = pure unit
