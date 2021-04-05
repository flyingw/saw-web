module App.Driver
  ( driverClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.Array (fromFoldable, elem, delete, (:))
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement)
import React.DOM (text, div, label, input, button, h6, small, iframe)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked)

import Api (PassengerType(..))
import Api.Pull (Pull(AddDriver), Address)
import Api.Push (Push(AddRouteOk, AddRouteErr), UserData)

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS
import Lib.JS (encodeURI)

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
  , carPlate :: String
  , date :: JSDate
  , deviationDistance :: Int
  , deviationTime :: Int
  , seats :: Int
  , from :: Address
  , to :: Address
  , types :: Array PassengerType
  , await :: Boolean
  , error :: Maybe String
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

driverClass :: ReactClass Props
driverClass = component "Driver" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { mapQ: Nothing
      , routeId: Nothing
      , firstName: fromMaybe "" $ props.user >>= _.firstName
      , lastName: fromMaybe "" $ props.user >>= _.lastName
      , phone: fromMaybe "" $ props.user >>= _.phone
      , carPlate: fromMaybe "" $ props.user >>= _.carPlate
      , date: date
      , deviationDistance: 2
      , deviationTime: 30
      , seats: 1  
      , from: { country: "Украина", city: "Киев", street: "Спортивная", building: "1" }
      , to: { country: "Украина", city: "Киев", street: "Льва Толстого", building: "1" }
      , types: fromFoldable [ Medical, Police, Firefighter, Army, Farmacy, Cashier, Regular ]
      , await: false
      , error: Nothing
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
  onMsg this (Just (AddRouteOk r))  = modifyState this _{ routeId = Just r.id, error = Nothing }
  onMsg this (Just (AddRouteErr r)) = modifyState this _{ error = Just $ fromMaybe "system" $ r.err, await = false }
  onMsg this _ = pure unit

  sendDriver :: This -> Effect Unit
  sendDriver this = do
    s <- getState this
    p <- getProps this
    _ <- modifyState this _{ await = true }
    let driver = AddDriver { 
        firstName: s.firstName
      , lastName: s.lastName
      , phone: s.phone
      , carPlate: s.carPlate
      , date: getTime s.date
      , deviationDistance: s.deviationDistance
      , deviationTime: s.deviationTime
      , seats: s.seats
      , from: s.from
      , to: s.to
      , types: s.types
      , lang: p.lang
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
      [ h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.driver_data" ]
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
          [ label [ htmlFor "carPlate" ] [ text $ props.keyText "key.car_plate"]
          , input [ _type "text", cn "form-control", _id "carPlate", autoComplete "carPlate", required true 
                  , value state.carPlate
                  , onChangeValue \v -> modifyState this _{ carPlate=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.car_plate.hint" ]
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_data" ]
      , div [ cn "d-flex justify-content-center form-row" ] 
        [ div [ cn "col-md-1 d-lg-none mb-md-3" ] []
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
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
        , div [ cn "col-md-2 col-lg-2 mb-3" ]
          [ label [ htmlFor "deviationDistance" ] [ text $ props.keyText "key.deviation_distance" ]
          , input [ _type "number", cn "form-control", _id "deviationDistance", min "2", max "20", required true 
                  , value $ show state.deviationDistance
                  , onChangeValueInt \v -> modifyState this _{ deviationDistance=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.deviation_distance.hint" ]
          ]
        , div [ cn "col-md-2 col-lg-2 mb-3" ]
          [ label [ htmlFor "deviationTime" ] [ text $ props.keyText "key.deviation_time" ]
          , input [ _type "number", cn "form-control", _id "deviationTime", min "10", max "120", required true 
                  , value $ show state.deviationTime
                  , onChangeValueInt \v -> modifyState this _{ deviationTime=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.deviation_time.hint" ]
          ]
        , div [ cn "col-md-1 d-lg-none mb-md-3" ] []  
        , div [ cn "col-md-4 col-lg-1 mb-3" ]
          [ label [ htmlFor "seats" ] [ text $ props.keyText "key.seats" ]
          , input [ _type "number", cn "form-control", _id "seats", min "1", max "5", value "1", disabled true, required true
                  , value $ show state.seats
                  , onChangeValueInt \v -> modifyState this _{ seats=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.seats.hint" ]
          ]
        , div [ cn "col-md-4 col-lg-4 mb-3 pl-md-2" ]
          [ div [ cn "pl-md-2" ] [ label [] [ text $ props.keyText "key.passenger" ] ]
          , div [ cn "pl-md-2" ] $
              map (\v ->
                div [ cn "form-check" ]
                [ input [ cn "form-check-input", _type "checkbox",  value "", _id $ keyPassengerType v, checked $ elem v state.types 
                        , onChange \_ -> modifyState this _ { types = if elem v state.types then delete v state.types else v : state.types }
                        ]
                , label  [ cn "form-check-label", htmlFor $ keyPassengerType v ] [ text $ props.keyText $ keyPassengerType v ]
                ]
              ) [ Medical, Police, Firefighter, Army, Farmacy, Cashier, Regular ]
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_start" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "countryFrom" ] [ text $ props.keyText "key.country" ]
          , input [ _type "text", cn "form-control", _id "countryFrom", required true
                  , value state.from.country
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ country=v } }
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "cityFrom" ] [ text $ props.keyText "key.city" ]
          , input [ _type "text", cn "form-control", _id "cityFrom", required true
                  , value state.from.city
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ city=v } }
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "streetFrom" ] [ text $ props.keyText "key.street" ]
          , input [ _type "text", cn "form-control", _id "streetFrom", required true
                  , value state.from.street
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ street=v } }
                  ]
          ] 
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "buildingFrom" ] [ text $ props.keyText "key.building" ]
          , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                  , value state.from.building
                  , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ building=v } }
                  ]
          ]
        ]
      , h6 [ cn "d-flex justify-content-center" ] [ text $ props.keyText "key.route_end" ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "countryTo" ] [ text $ props.keyText "key.country" ]
          , input [ _type "text", cn "form-control", _id "countryTo", required true
                  , value state.to.country
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ country=v } }
                  ]
          ] 
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "cityTo" ] [ text $ props.keyText "key.city" ]
          , input [ _type "text", cn "form-control", _id "cityTo", required true
                  , value state.to.city
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ city=v } } 
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
          [ label [ htmlFor "streetTo" ] [ text $ props.keyText "key.street" ]
          , input [ _type "text", cn "form-control", _id "streetTo", required true
                  , value state.to.street
                  , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ street=v } } 
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 mb-3" ]
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
      , case state.error of
          Just err -> div [ cn "alert alert-danger" ] [ text $ props.keyText $ "key.err." <> err ]
          Nothing -> mempty
      , case state.routeId of
          Just id -> div [ cn "alert alert-success" ] [ text $ props.keyText "key.add.success" <> " " <> id ]
          Nothing ->
            button [ cn "btn btn-primary mb-3", _type "button"
                  --  , disabled $ isNothing state.mapQ
                    , disabled state.await
                    , onClick \_ -> sendDriver this 
                    ] 
            [ text $ props.keyText "key.add"
            , if state.await then div [ cn "spinner-border text-light spinner-border-sm ml-1" ] [] else mempty
            ]
      ]
