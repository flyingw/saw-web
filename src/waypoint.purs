module App.Waypoint
  ( waypointClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (fromFoldable, elem, delete, (:))
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, datalist, option, select)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, list, selected, autoComplete)
import Effect.Class.Console (infoShow)

import Api.Pull (Pull(GetCitiesList), Address)
import Api.Push (Push(CitiesList), UserData)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State =
  { addrs :: Address
  , countries :: Array String
  , cities :: Array String
  , country :: String
  , city :: String
  , street :: String
  , building :: String
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

waypointClass :: ReactClass Props
waypointClass = component "Waypoints" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { addrs: { country: "Украина", city: "Киев", street: "Льва Толстого", building: "1" }
      , countries: [ "Украина" ]
      , cities: [ "default", "list" ]
      , country: ""
      , city: ""
      , street: ""
      , building: ""
      , unsub: pure unit
      } :: State
    , render: render this
    , componentDidMount: do
        WS.sub props.ws (onMsg this) >>= \unsub -> modifyState this _{ unsub = unsub }
        WS.snd props.ws $ GetCitiesList { country: "UA", lang: props.lang }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (CitiesList r)) = do
      infoShow "get cities"
      modifyState this _{ cities = r.cities }
  onMsg this _                     = pure unit

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ 
      div [ cn "d-flex justify-content-center form-row" ]
      [ div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
        [ label [ htmlFor "country" ] [ text $ props.keyText "key.country" ]
        , select [ cn "form-control custom-select", _id "country"
                 , value state.country
                 , onChangeValue \v -> do
                     WS.snd props.ws $ GetCitiesList { country: v, lang: props.lang }
                     modifyState this _{ country = v }
                 ] $
          map (\v ->
            option [ value v, selected $ v == state.country ] [ text $ props.keyText $ "key.country." <> v ]
          ) [ "UA" ]
        ]
      , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
        [ label [ htmlFor "city" ] [ text $ props.keyText "key.city3" ]
        , input [ _type "text", cn "form-control", _id "city", autoComplete "off", list "cities"
                , value state.city
                , onChangeValue \v -> modifyState this _{ city = v }
                ]
        , datalist [ _id "cities" ] $
          map (\city ->
              option [ value city ] []
          ) state.cities
        ]
      , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
        [ label [ htmlFor "streetFrom" ] [ text $ props.keyText "key.street" ]
        , input [ _type "text", cn "form-control", _id "streetFrom", required true
                , value state.street
                , onChangeValue \v -> modifyState this _{ street=v }
                ]
        ]
      , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
        [ label [ htmlFor "buildingFrom" ] [ text $ props.keyText "key.building" ]
        , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                , value state.building
                , onChangeValue \v -> modifyState this _{ building=v }
                ]
        ]
      ]