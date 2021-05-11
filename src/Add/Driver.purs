module App.Add.Driver
  ( addDriverClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.Array (fromFoldable, elem, delete, (:), null, length, snoc, deleteAt)
import Data.FunctorWithIndex (mapWithIndex)
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (a, text, div, label, input, button, h6, small, iframe, i, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, href)
import Effect.Console (logShow)

import Api (PassengerType(..), Coordinates)
import Api.Pull (Pull(AddDriver, GetAutocomplete), Address)
import Api.Push (Push(AutocompleteOk, AddRouteErr), UserData)
import Proto.BigInt (fromNumber)

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Lib.JS (encodeURI)
import App.Waypoint (waypointClass)
import Lib.Maps (getLocation)

import Effect.Timer (setTimeout, clearTimeout, TimeoutId)

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type Waypoint =
  { description :: String
  , coordinates :: Maybe Coordinates
  }

type State =
  { waypoints :: Array Waypoint
  , location :: Maybe Coordinates
  , mapUrl :: Maybe String
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

addDriverClass :: ReactClass Props
addDriverClass = component "AddDriver" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { waypoints:
        [ { description: "", coordinates: Nothing }
        , { description: "", coordinates: Nothing }
        ]
      , location: Nothing
      , mapUrl: Nothing
      , unsub: pure unit
      } :: State
    , render: render this
    , componentDidMount: do
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
        getLocation (\lat lng -> do
          logShow $ lat
          modifyState this _{ location = Just { lat: lat, lng: lng } }
        ) (pure unit)
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this _ = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div []
      [ h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.waypoints.title" ]
      , div [] $ mapWithIndex (\i a ->
          createLeafElement waypointClass { key: show i
                                          , ws: props.ws
                                          , lang: props.lang
                                          , keyText: props.keyText
                                          , user: props.user
                                          , icon: icon i (length state.waypoints - 1)
                                          , location: state.location
                                          , set: \_ -> pure unit
                                          , removeActive: length state.waypoints > 2
                                          , remove: modifyState this \s -> s{ waypoints = fromMaybe s.waypoints $ deleteAt i s.waypoints }
                                          }
        ) state.waypoints
      , div [ cn "d-flex justify-content-center" ]
        [ a [ cn "btn btn-outline-secondary fas fa-plus fa-lg" 
            , href "#"
            , onClick \_ -> modifyState this \s -> s{ waypoints = snoc s.waypoints { description: "", coordinates: Nothing } }
            ]
          []
        ]
      , case state.mapUrl of
          Just url -> 
            div [ cn "form-row" ]
            [ div [ cn "col-md-12 mb-3" ]
              [ iframe [ width "100%", height "400", frameBorder "0", src url ]
                []
              ]
            ]
          Nothing -> mempty
      ]
  
  icon :: Int -> Int -> String
  icon 0 _          = "far fa-dot-circle fa-lg"
  icon a b | a == b = "far fa-flag-checkered fa-lg"
  icon _ _          = "far fa-circle fa-lg"
    