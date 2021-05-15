module App.Add.Driver
  ( addDriverClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.Array (fromFoldable, elem, delete, (:), null, length, snoc, deleteAt, modifyAt, filter, zip, take)
import Data.Traversable (sequence, maximum)
import Data.Tuple (Tuple(Tuple))
import Data.Ord (compare)
import Data.FunctorWithIndex (mapWithIndex)
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class.Console (infoShow)
import React (ReactClass, ReactThis, getProps, getState, modifyState, modifyStateWithCallback, component, createLeafElement, ReactElement)
import React.DOM (a, text, div, label, input, button, h6, small, iframe, i, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, href, style)
import Effect.Console (logShow)

import Api (PassengerType(..), Location, WaypointType(..), Waypoint)
import Api.Pull (Pull(AddDriver, GetDirections), Address)
import Api.Push (Push(GetDirectionsOk), UserData)
import Proto.BigInt (fromNumber)

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Lib.JS (encodeURI)
import App.Waypoint (waypointClass)
import Lib.Maps (getLocation, createMap, GoogleMap, GooglePolyline, decodeGoogle, createPolyline, attachPolyline, detachPolyline, setCenter)

import Effect.Timer (setTimeout, clearTimeout, TimeoutId)

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type WaypointItem =
  { waypoint :: Waypoint
  , done :: Boolean
  , n :: Int
  }

type State =
  { waypoints :: Array WaypointItem
  , date :: JSDate
  , dateDevaiation :: Int
  , seats :: Int
  , location :: Maybe Location
  , map :: Maybe GoogleMap
  , routes :: Array GooglePolyline
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
        [ { waypoint: defaultWaypoint, done: false, n: 0 }
        , { waypoint: defaultWaypoint, done: false, n: 1 }
        ]
      , date: date
      , dateDevaiation: 5
      , seats: 2
      , location: Nothing
      , map: Nothing
      , routes: []
      , unsub: pure unit
      } :: State
    , render: render this
    , componentDidMount: do
        unsub <- WS.sub props.ws $ onMsg this
        map   <- createMap "div-map" { center: { lat: 50.4500, lng: 30.5235 }, zoom: 14 }
        modifyState this _{ unsub = unsub, map = Just map }
        (getLocation 
          (\lat lng -> do
            let location = { lat: lat, lng: lng }
            s <- getState this
            modifyState this _{ location = Just location }
            case s.map of
              Just m  -> setCenter m location
              Nothing -> pure unit
          )
          (pure unit)
        )

    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (GetDirectionsOk r)) = do
    infoShow $ "GetDirectionsOk"
    infoShow $ r
    let withColor = zip r.routes [ "#FF0000", "#00FF00", "#0000FF", "#FFFF00" ]
    state  <- getState this
    void $ sequence $ map detachPolyline state.routes
    routes <- sequence $ map (\(Tuple route color) -> do
                polyline <- createPolyline { path: decodeGoogle route, strokeColor: color, strokeOpacity: 1.0, strokeWeight: 2 }
                fromMaybe (pure unit) $ map (attachPolyline polyline) state.map
                pure polyline
              ) withColor
    modifyState this _{ routes = routes }
  onMsg this _                          = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div []
      [ h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_data" ]
      , div [ cn "d-flex justify-content-center" ]
        [ div [ cn "col-9 col-md-6 col-lg-5 mb-3" ] 
          [ label [ htmlFor "date" ] [ text $ props.keyText "key.date" ]
          , createLeafElement datepickerClass 
              { onChange: \d -> modifyState this _{ date = d }
              , lang: props.lang
              , showTime: true
              , className: "form-control"
              , wrapperClassName: "form-control"
              , _id: "date"
              , keyText: props.keyText
              }
          ]
        , div [ cn "col-3 col-md-2 col-lg-1 mb-3" ] 
          [ label [ htmlFor "date" ] [ text $ props.keyText "key.date.deviation" ]
          , input [ _type "number", cn "form-control", _id "seats", min "0", max "30", value "1", required true
                  , value $ show state.dateDevaiation
                  , onChangeValueInt \v -> modifyState this _{ dateDevaiation=v }
                  ]
          ]
        ]
      , div [ cn "d-flex justify-content-center" ]
        [ div [ cn "col-3 col-md-2 col-lg-1 mb-3" ]
          [ label [ htmlFor "seats" ] [ text $ props.keyText "key.seats" ]
          , input [ _type "number", cn "form-control", _id "seats", min "1", max "5", value "1", required true
                  , value $ show state.seats
                  , onChangeValueInt \v -> modifyState this _{ seats=v }
                  ]
          ]
        , div [ cn "col-9 col-md-6 col-lg-6 mb-3" ]
          []
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.waypoints.title" ]
      , div [] $ mapWithIndex (\i a ->
          div [ cn "d-flex justify-content-center" ]
          [ div [ cn "col-md-8 col-lg-6 mb-3" ]
            [ createLeafElement waypointClass 
                { key: show a.n
                , ws: props.ws
                , lang: props.lang
                , keyText: props.keyText
                , user: props.user
                , icon: icon i (length state.waypoints - 1)
                , location: state.location
                , waypoint: a.waypoint
                , set: \v done ->
                    modifyStateWithCallback this 
                      (\s -> s{ waypoints = fromMaybe s.waypoints $ modifyAt i _{ waypoint = v, done = done } s.waypoints })
                      if done then
                        (fetchRoute this)
                      else
                        pure unit
                , removeActive: length state.waypoints > 2
                , remove: 
                    modifyStateWithCallback this 
                      (\s -> s{ waypoints = fromMaybe s.waypoints $ deleteAt i s.waypoints })
                      (fetchRoute this)
                }
            ]
          ]
        ) state.waypoints
      , div [ cn "d-flex justify-content-center mb-3" ]
        [ button [ cn "btn btn-outline-secondary" 
                 , href "#"
                 , onClick \_ -> 
                      modifyState this \s -> do
                        let max = fromMaybe 0 $ maximum $ map _.n s.waypoints
                        s{ waypoints = snoc s.waypoints { waypoint: defaultWaypoint, done: false, n: max + 1 } }
                 ]
          [ text $ props.keyText "key.waypoints.add.button"
          ]
        , button [ cn "btn btn-outline-primary ml-3"
                 , href "#"
                 , onClick \_ -> pure unit
                 ]
          [ text $ props.keyText "key.waypoints.done.button"
          ]  
        ]
      , div [ _id "div-map", style { height: "300px" } ] []
      ]

  defaultWaypoint :: Waypoint
  defaultWaypoint = { description: "", tpe: UnknownWaypoint, placeId: Nothing }

  fetchRoute :: This -> Effect Unit
  fetchRoute this = do
    s <- getState this
    p <- getProps this
    let points  = filter _.done s.waypoints
    if length points >= 2 then do
      let w = map _.waypoint points
      WS.snd p.ws $ GetDirections { waypoints: w, departure: fromNumber $ getTime s.date, lang: p.lang }
    else do
      pure unit

  icon :: Int -> Int -> String
  icon 0 _          = "far fa-dot-circle fa-lg"
  icon a b | a == b = "fas fa-flag-checkered fa-lg"
  icon _ _          = "far fa-circle fa-lg"
    