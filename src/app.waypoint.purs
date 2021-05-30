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
import React.DOM.Props as DProps 
import React.DOM (a, i, text, div, label, input, button, h6, small, iframe, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, href, onFocus, onBlur, placeholder, style)
import Effect.Console (logShow)

import Api (PassengerType(..), Location, WaypointType(..), Waypoint)
import Api.Pull (Pull(AddDriver, GetAutocomplete), Address)
import Api.Push (Push(GetAutocompleteOk), UserData)
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
  , location :: Maybe Location
  , waypoint :: Waypoint
  , set :: Waypoint -> Boolean -> Effect Unit
  , done :: Boolean
  , removeActive :: Boolean
  , remove :: Effect Unit
  }

type State =
  { timer :: Maybe TimeoutId
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
      { timer: Nothing
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
  onMsg this (Just (GetAutocompleteOk r)) = do
    s <- getState this
    if s.focus then modifyState this _{ predictions = r.predictions } else pure unit
  onMsg this _ = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div [ cn "input-group" ] 
      [ div [ cn "input-group-prepend align-self-center" ] 
        [ i [ cn props.icon ] []
        ]
      , input [ _type "text", cn "form-control ml-2", required true 
              , value props.waypoint.description
              , onChangeValue \v -> do
                  props.set props.waypoint{ description = v } false
                  fetchAutocomplete this v
              , onFocus \_ -> modifyState this _{ focus = true }
              , onBlur \_ -> modifyState this _{ focus = false }
              , placeholder $ props.keyText "key.waypoint.input.placeholder"
              , outlineStyle props.done
              ]
      , if props.removeActive then
          div [ cn "input-group-prepend align-self-center ml-2" ] 
          [ a [ cn "far fa-trash-alt fa-lg text-secondary"
              , href "#"
              , onClick \_ -> props.remove
              ] []
          ]
        else
          mempty
      , div [ cn $ "dropdown-menu ml-4" <> if not null state.predictions then " show" else "" ] $
          map (\prediction ->
            a [ cn "dropdown-item", href "#"
              , onClick \_ -> do
                  modifyState this _{ predictions = [] }
                  props.set prediction true
              ]
            [ i [ cn $ waypointIcon prediction.tpe ] []
            , span [ cn "ml-1" ] [ text $ prediction.description ]
            ]
          ) state.predictions
      ]

  outlineStyle :: Boolean -> DProps.Props
  outlineStyle true = style
    { outlineColor: "#28a745"
    , outlineStyle: "solid"
    , outlineWidth: "thin"
    }
  outlineStyle false = style
    { outlineColor: "#dc3545"
    , outlineStyle: "solid"
    , outlineWidth: "thin"
    }

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
  fetchAutocomplete this v | length v > 3 = do
    s  <- getState this
    p  <- getProps this
    _ <- fromMaybe (pure unit) $ map clearTimeout s.timer
    t <- setTimeout 500 $ WS.snd p.ws $ GetAutocomplete { text: v, lang: p.lang, location: p.location }
    modifyState this _{ timer = Just t }
  fetchAutocomplete _ _ = pure unit
