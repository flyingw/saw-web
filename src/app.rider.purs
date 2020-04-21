module App.Rider
  ( riderClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.JSDate (JSDate, now, getTime)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Global (encodeURI)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, select, option)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, value, src, width, height, frameBorder, onClick, disabled)

import Api (PassengerType(..))
import Api.Pull (Pull(AddPassenger), Address)
import Api.Push (UserData, Push(AddRouteOk))

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

type Props =
  { ws :: Ws
  , lang :: String
  , keyText :: String -> String
  , user :: Maybe UserData
  }

type State =
  { mapQ :: Maybe String
  , routeId :: Maybe String
  , firstName :: String
  , lastName :: String
  , phone :: String
  , date :: JSDate
  , tpe :: PassengerType
  , from :: Address
  , to :: Address
  , await :: Boolean
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

riderClass :: ReactClass Props
riderClass = component "Rider" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { mapQ: Nothing
      , routeId: Nothing
      , firstName: fromMaybe "" $ props.user >>= _.firstName
      , lastName: fromMaybe "" $ props.user >>= _.lastName
      , phone: fromMaybe "" $ props.user >>= _.phone
      , date: date
      , tpe: fromMaybe Medical $ props.user >>= _.tpe
      , from: { country: "Украина", city: "Киев", street: "Спортивная", building: "1" }
      , to: { country: "Украина", city: "Киев", street: "Льва Толстого", building: "1" }
      , await: false
      , unsub: pure unit
      }
    , render: render this
    , componentDidMount: do
        unsub  <- WS.sub props.ws $ onMsg this
        modifyState this _{ unsub = unsub }
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (AddRouteOk r)) = modifyState this _{ routeId = Just r.id }
  onMsg this _                     = pure unit

  types :: Array PassengerType
  types = [ Medical, Police, Firefighter, Army, Farmacy, Cashier, Regular ]
  
  typesMap :: Map String PassengerType
  typesMap = fromFoldable $ map (\v -> Tuple (keyPassengerType v) v) types

  sendPassenger :: This -> Effect Unit
  sendPassenger this = do
    s <- getState this
    p <- getProps this
    _ <- modifyState this _{ await = true }
    let driver = AddPassenger { 
        firstName: s.firstName
      , lastName: s.lastName
      , phone: s.phone
      , date: getTime s.date
      , tpe: s.tpe
      , from: s.from
      , to: s.to
      }
    WS.snd p.ws driver
  
  updateMap :: This -> Effect Unit
  updateMap this = do
    s <- getState this
    let host = "https://www.google.com/maps/embed/v1/directions"
    let origin = fromMaybe "" $ encodeURI $ s.from.street <> "+" <> s.from.building <> "+" <> s.from.city
    let destination = fromMaybe "" $ encodeURI $ s.to.street <> "+" <> s.to.building <> "+" <> s.to.city
    let key = "AIzaSyAuq2lMfK8JPYK4-zYYw9Bl8SeTQrKJJeY"
    let q = host <> "?origin=" <> origin <> "&destination=" <> destination <> "&key=" <> key
    modifyState this \state -> state{ mapQ = Just q }

  render this = do
    props <- getProps this
    state <- getState this
    pure $ 
      div []
      [ h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.passenger_data" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "firstName" ] [ text $ props.keyText "key.first_name" ]
          , input [ _type "text", cn "form-control", _id "firstName", required true 
                  , value state.firstName
                  , onChangeValue \v -> modifyState this _{ firstName=v }
                  , value state.firstName
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "lastName" ] [ text $ props.keyText "key.last_name" ]
          , input [ _type "text", cn "form-control", _id "lastName", required true 
                  , value state.lastName
                  , onChangeValue \v -> modifyState this _{ lastName=v }
                  , value state.lastName
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.last_name.hint" ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "phone" ] [ text $ props.keyText "key.phone" ]
          , input [ _type "text", cn "form-control", _id "phone", autoComplete "phone", required true 
                  , value state.phone
                  , onChangeValue \v -> modifyState this _{ phone=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.phone.hint" ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "specialization" ] [ text $ props.keyText "key.specialization" ]
          , select [ cn "custom-select"
                    , _id "type"
                    , value $ keyPassengerType state.tpe
                    , onChangeValue \v -> modifyState this _{ tpe = fromMaybe Regular $ lookup v typesMap }
                    ] $
            map (\v -> option [ value $ keyPassengerType v ] [ text $ props.keyText $ keyPassengerType v ]) types
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_data" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "date" ] [ text $ props.keyText "key.date" ]
          , createLeafElement datepickerClass { onChange: \d -> modifyState this _{ date = d }
                                              , lang: props.lang
                                              , showTime: true
                                              , className: "form-control"
                                              , wrapperClassName: "form-control"
                                              , _id: "date"
                                              , keyText: props.keyText
                                              }
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_start" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "countryFrom" ] [ text $ props.keyText "key.country" ]
          , input [ _type "text", cn "form-control", _id "countryFrom", required true
                  , value state.from.country
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ country=v } }
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "cityFrom" ] [ text $ props.keyText "key.city" ]
          , input [ _type "text", cn "form-control", _id "cityFrom", required true
                  , value state.from.city
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ city=v } }
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "streetFrom" ] [ text $ props.keyText "key.street" ]
          , input [ _type "text", cn "form-control", _id "streetFrom", required true
                  , value state.from.street
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ street=v } }
                  ]
          ] 
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "buildingFrom" ] [ text $ props.keyText "key.building" ]
          , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                  , value state.from.building
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ building=v } }
                  ]
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_end" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "countryTo" ] [ text $ props.keyText "key.country" ]
          , input [ _type "text", cn "form-control", _id "countryTo", required true
                  , value state.to.country
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ country=v } }
                  ]
          ] 
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "cityTo" ] [ text $ props.keyText "key.city" ]
          , input [ _type "text", cn "form-control", _id "cityTo", required true
                  , value state.to.city
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ city=v } } 
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "streetTo" ] [ text $ props.keyText "key.street" ]
          , input [ _type "text", cn "form-control", _id "streetTo", required true
                  , value state.to.street
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ street=v } } 
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "houseTo" ] [ text $ props.keyText "key.building" ]
          , input [ _type "text", cn "form-control", _id "houseTo", required true
                  , value state.to.building
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ building=v } } 
                  ]
          ]
        ]
      , button [ cn "btn btn-outline-secondary mb-3", _type "button"
               , onClick \_ -> updateMap this ] [ text $ props.keyText "key.overview_route"
               ]
      , case state.mapQ of
          Just q -> 
            div [ cn "form-row" ]
            [ div [ cn "col-md-12 mb-3" ]
              [ iframe [ width "100%", height "400", frameBorder "0", src q ]
                []
              ]
            ]
          Nothing -> mempty
      , div [ cn "form-group" ]
        [ div [ cn "form-check" ]
          [ input [ _type "checkbox", cn "form-check-input", _id "agree_terms" ]
          , label [ htmlFor "agree_terms", cn "form-check-label" ] [ text $ props.keyText "key.agree_terms" ]
          ]
        , div [ cn "form-check" ]
          [ input [ _type "checkbox", cn "form-check-input", _id "agree_rules" ]
          , label [ htmlFor "agree_rules", cn "form-check-label" ] [ text $ props.keyText "key.agree_rules" ]
          ]
        ]
      , div [ cn "alert alert-info col-md-12" ] [ text $ props.keyText "key.add.hint" ]
      , case state.routeId of
          Just id -> div [ cn "alert alert-success" ] [ text $ props.keyText "key.add.success" <> " " <> id ]
          Nothing ->
            button [ cn "btn btn-primary mb-3", _type "button"
                  --  , disabled $ isNothing state.mapQ
                    , disabled state.await
                    , onClick \_ -> sendPassenger this 
                    ] 
            [ text $ props.keyText "key.add"
            , if state.await then div [ cn "spinner-border text-light spinner-border-sm ml-1" ] [] else mempty
            ]
      ]
