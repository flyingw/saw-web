module App.Rider
  ( riderClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Either (Either(Left, Right))
import Data.Int (ceil)
import Data.JSDate (parse, now, getTime, toISOString)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
import Data.Monoid (mempty)
import Data.String (take)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (error)
import Global (encodeURI)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component)
import React.DOM (text, div, form, label, input, button, h6, small, iframe, select, option)
import React.DOM.Props (htmlFor, placeholder, _id, _type, noValidate, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, value, disabled)

import Lib.React(cn, targetValue, onChangeValue, onChangeValueInt)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api (Address)
import Api.Push (decodePush, Push(Pong))
import Api.Pull (PassengerType(..), Pull(AddPassenger), encodePull)

type Props =
  { ws :: WebSocket
  }

type State =
  { mapQ :: Maybe String
  , name :: String
  , phone :: String
  , date :: String
  , tpe :: PassengerType
  , from :: Address
  , to :: Address
  }

riderClass :: ReactClass Props
riderClass = component "Rider" \this -> do
  date <- today
  props <- getProps this
  pure
    { state:
      { mapQ: Nothing
      , name: ""
      , phone: ""
      , date: date
      , tpe: Medical
      , from: { city: "Киев", street: "Спортивная", building: "1" }
      , to: { city: "Киев", street: "Льва Толстого", building: "1" }
      }
    , render: render this
    , componentDidMount: do
        props <- getProps this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right _ -> pure unit
        ) (sequence <<< map error)
    }
  where

  today :: Effect String 
  today = now >>= toISOString <#> (take 19)

  sendPassenger :: ReactThis Props State -> Effect Unit
  sendPassenger this = do
    s <- getState this
    p <- getProps this
    d <- parse s.date
    let driver = AddPassenger { 
        name: s.name
      , phone: s.phone
      , date: getTime d
      , tpe: s.tpe
      , from: s.from
      , to: s.to
      }
    WS.send p.ws $ encodePull driver
  
  updateMap :: ReactThis Props State -> Effect Unit
  updateMap this = do
    s <- getState this
    let host = "https://www.google.com/maps/embed/v1/directions"
    let origin = fromMaybe "" $ encodeURI $ s.from.street <> "+" <> s.from.building <> "+" <> s.from.city
    let destination = fromMaybe "" $ encodeURI $ s.to.street <> "+" <> s.to.building <> "+" <> s.to.city
    let key = "AIzaSyAuq2lMfK8JPYK4-zYYw9Bl8SeTQrKJJeY"
    let q = host <> "?origin=" <> origin <> "&destination=" <> destination <> "&key=" <> key
    modifyState this \state -> state{ mapQ = Just q }

  tpeText :: PassengerType -> String
  tpeText Medical = "Medical"
  tpeText Police = "Police"
  tpeText Firefighter = "Firefighter"
  tpeText Army = "Army"
  tpeText Farmacy = "Farmacy"
  tpeText Cashier = "Cashier"
  tpeText Regular = "Regular"

  render this = do
    props <- getProps this
    state <- getState this
    pure $ div [ cn "m-2  " ]
      [ form [ noValidate true ]
        [ h6 [] [ text $ props.keyText "key.passenger_data" ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "name" ] [ text $ props.keyText "key.name" ]
            , input [ _type "text", cn "form-control", _id "name", required true 
                    , value state.name
                    , onChangeValue \v -> modifyState this _{ name=v }
                    , value state.name
                    ]
            ]
          , div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "phone" ] [ text $ props.keyText "key.phone" ]
            , input [ _type "text", cn "form-control", _id "phone", autoComplete "phone", required true 
                    , value state.phone
                    , onChangeValue \v -> modifyState this _{ phone=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.phone.hint" ]
            ]
          , div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "specialization" ] [ text $ props.keyText "key.specialization" ]
            , select [ cn "custom-select"
                     , _id "type"
                     , value $ case state.tpe of
                                  Medical -> "Medical"
                                  Police -> "Police"
                                  Firefighter -> "Firefighter"
                                  Army -> "Army"
                                  Farmacy -> "Farmacy"
                                  Cashier -> "Cashier"
                                  Regular -> "Regular"
                     , onChangeValue \v -> do
                          let tpe = case v of
                                      "Medical"     -> Medical
                                      "Police"      -> Police
                                      "Firefighter" -> Firefighter
                                      "Army"        -> Army
                                      "Farmacy"     -> Farmacy
                                      "Cashier"     -> Cashier
                                      _             -> Regular
                          modifyState this _{ tpe = tpe }
                     ]
              [ option [ value "Medical" ] [ text "Медицинский работник" ]
              , option [ value "Police" ] [ text "Сотрудник полиции" ]
              , option [ value "Firefighter" ] [ text "Сотрудник МЧС" ]
              , option [ value "Army" ] [ text "Армия" ]
              , option [ value "Farmacy" ] [ text "Фармацевт" ]
              , option [ value "Cashier" ] [ text "Сотрудник торговли" ]
              , option [ value "Regular" ] [ text "Другое" ]
              ]
            ]
          ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "date" ] [ text $ props.keyText "key.date" ]
            , input [ _type "datetime-local", cn "form-control", _id "date", required true
                    , value state.date
                    , onChangeValue \v -> modifyState this _{ date=v }
                    ]
            ]
          ]
        , div [] [ h6 [] [ text $ props.keyText "key.route_start" ] ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "cityFrom" ] [ text $ props.keyText "key.city" ]
            , input [ _type "text", cn "form-control", _id "cityFrom", required true
                    , value state.from.city
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ city=v } }
                    ]
            ]
          , div [ cn "col-md-3 mb-3" ]
            [ label [ htmlFor "streetFrom" ] [ text $ props.keyText "key.street" ]
            , input [ _type "text", cn "form-control", _id "streetFrom", required true
                    , value state.from.street
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ street=v } }
                    ]
            ] 
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "buildingFrom" ] [ text $ props.keyText "key.building" ]
            , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                    , value state.from.building
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ building=v } }
                    ]
            ]
          ]
        , div [] [ h6 [] [ text $ props.keyText "key.route_end" ] ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-4 mb-3" ]
            [ label [ htmlFor "cityTo" ] [ text $ props.keyText "key.city" ]
            , input [ _type "text", cn "form-control", _id "cityTo", required true
                    , value state.to.city
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ city=v } } 
                    ]
            ]
          , div [ cn "col-md-3 mb-3" ]
            [ label [ htmlFor "streetTo" ] [ text $ props.keyText "key.street" ]
            , input [ _type "text", cn "form-control", _id "streetTo", required true
                    , value state.to.street
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ street=v } } 
                    ]
            ]
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "houseTo" ] [ text $ props.keyText "key.building" ]
            , input [ _type "text", cn "form-control", _id "houseTo", required true
                    , value state.to.building
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ building=v } } 
                    ]
            ]
          ]
        , button [ cn "btn btn-secondary mb-3", _type "button", onClick \_ -> updateMap this ] [ text $ props.keyText "key.overview_route" ]
        , case state.mapQ of
            Just q -> 
              div [ cn "form-row" ]
              [ div [ cn "col-md-6 mb-3" ]
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
        , button [ cn "btn btn-primary mb-3", _type "button"
                --  , disabled $ isNothing state.mapQ
                 , onClick \_ -> sendPassenger this 
                 ] 
          [ text $ props.keyText "key.add"
          ]
        ]
      ]
