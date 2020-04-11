module App.Driver
  ( driverClass
  , Props
  ) where

import Prelude hiding (div, min, max)
import Data.Array (fromFoldable) as Array
import Data.Set (Set, delete, insert, member, fromFoldable)
import Data.Either (Either(Left, Right))
import Data.JSDate (parse, now, getTime, toISOString)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isNothing)
import Data.String (take)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (error)
import Global (encodeURI)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component)
import React.DOM (text, div, form, label, input, button, h6, small, iframe)
import React.DOM.Props (htmlFor, _id, _type, noValidate, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked)

import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Model(PassengerType(..))
import Api (Address)
import Api.Pull as P
import Api.Pull (Pull(AddDriver), encodePull)
import Api.Push (decodePush, Push(AddRouteOk, LoginOk))

type Props =
  { ws :: WebSocket
  , lang :: String
  , keyText :: String -> String
  }

type State =
  { mapQ :: Maybe String
  , routeN :: Maybe String
  , name :: String
  , phone :: String
  , carPlate :: String
  , date :: String
  , lap :: Int
  , seats :: Int
  , from :: Address
  , to :: Address
  , types :: Set PassengerType
  }

driverClass :: ReactClass Props
driverClass = component "Driver" \this -> do
  date <- today
  props <- getProps this
  pure
    { state:
      { mapQ: Nothing
      , routeN: Nothing
      , name: ""
      , phone: ""
      , carPlate: ""
      , date: date
      , lap: 3
      , seats: 1  
      , from: { city: "Киев", street: "Спортивная", building: "1" }
      , to: { city: "Киев", street: "Льва Толстого", building: "1" }
      , types: fromFoldable [ Medical, Police, Firefighter, Army, Farmacy, Cashier, Regular ]
      } :: State
    , render: render this
    , componentDidMount: do
        p <- getProps this
        let ws = p.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right { val: AddRouteOk r } -> modifyState this _{ routeN=Just r.n }
          Right { val: LoginOk {name: Just name}} -> modifyState this _{ name=name }
          Right _ -> pure unit
        ) (sequence <<< map error)
    }
  where

  today :: Effect String 
  today = now >>= toISOString <#> (take 19)
  
  sendDriver :: ReactThis Props State -> Effect Unit
  sendDriver this = do
    s <- getState this
    p <- getProps this
    d <- parse s.date
    let driver = AddDriver { 
        name: s.name
      , phone: s.phone
      , carPlate: s.carPlate
      , date: getTime d
      , lap: s.lap
      , seats: s.seats
      , from: s.from
      , to: s.to
      , types: Array.fromFoldable s.types <#> case _ of
          Medical     -> P.Medical
          Police      -> P.Police
          Firefighter -> P.Firefighter
          Army        -> P.Army
          Farmacy     -> P.Farmacy
          Cashier     -> P.Cashier
          Regular     -> P.Regular
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


  render this = do
    props <- getProps this
    state <- getState this
    pure $ div [ cn "m-2  " ]
      [ form [ noValidate true ]
        [ h6 [] [ text $ props.keyText "key.driver_data" ]
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
            [ label [ htmlFor "carPlate" ] [ text $ props.keyText "key.car_plate"]
            , input [ _type "text", cn "form-control", _id "carPlate", autoComplete "carPlate", required true 
                    , value state.carPlate
                    , onChangeValue \v -> modifyState this _{ carPlate=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.car_plate.hint" ]
            ]
          ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-8" ]
            [ div [ cn "form-row" ]
              [ div [ cn "col-md-6 mb-3" ]
                [ label [ htmlFor "date" ] [ text $ props.keyText "key.date" ]
                , input [ _type "datetime-local", cn "form-control", _id "date", required true
                        , value state.date
                        , onChangeValue \v -> modifyState this _{ date=v }
                        ]
                ]
              , div [ cn "col-md-2 mb-3" ]
                [ label [ htmlFor "lap" ] [ text $ props.keyText "key.lap" ]
                , input [ _type "number", cn "form-control", _id "lap", min "2", max "10", value "3", required true 
                        , value $ show state.lap
                        , onChangeValueInt \v -> modifyState this _{ lap=v }
                        ]
                , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.lap.hint" ]
                ]
              , div [ cn "col-md-2 mb-3" ]
                [ label [ htmlFor "seats" ] [ text $ props.keyText "key.seats" ]
                , input [ _type "number", cn "form-control", _id "seats", min "1", max "5", value "1", disabled true, required true
                        , value $ show state.seats
                        , onChangeValueInt \v -> modifyState this _{ seats=v }
                        ]
                , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.seats.hint" ]
                ]
              ]
            , div [] [ h6 [] [ text $ props.keyText "key.route_start" ] ]
            , div [ cn "form-row" ]
              [ div [ cn "col-md-6 mb-3" ]
                [ label [ htmlFor "cityFrom" ] [ text $ props.keyText "key.city" ]
                , input [ _type "text", cn "form-control", _id "cityFrom", required true
                        , value state.from.city
                        , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ city=v } }
                        ]
                ]
              , div [ cn "col-md-4 mb-3" ]
                [ label [ htmlFor "streetFrom" ] [ text $ props.keyText "key.street" ]
                , input [ _type "text", cn "form-control", _id "streetFrom", required true
                        , value state.from.street
                        , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ street=v } }
                        ]
                ] 
              , div [ cn "col-md-2 mb-3" ]
                [ label [ htmlFor "buildingFrom" ] [ text $ props.keyText "key.building" ]
                , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                        , value state.from.building
                        , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ building=v } }
                        ]
                ]
              ]
            , div [] [ h6 [] [ text $ props.keyText "key.route_end" ] ]
            , div [ cn "form-row" ]
              [ div [ cn "col-md-6 mb-3" ]
                [ label [ htmlFor "cityTo" ] [ text $ props.keyText "key.city" ]
                , input [ _type "text", cn "form-control", _id "cityTo", required true
                        , value state.to.city
                        , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ city=v } } 
                        ]
                ]
              , div [ cn "col-md-4 mb-3" ]
                [ label [ htmlFor "streetTo" ] [ text $ props.keyText "key.street" ]
                , input [ _type "text", cn "form-control", _id "streetTo", required true
                        , value state.to.street
                        , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ street=v } } 
                        ]
                ]
              , div [ cn "col-md-2 mb-3" ]
                [ label [ htmlFor "houseTo" ] [ text $ props.keyText "key.building" ]
                , input [ _type "text", cn "form-control", _id "houseTo", required true
                        , value state.to.building
                        , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ building=v } } 
                        ]
                ]
              ]
            ]
          , div [ cn "col-md-4" ]
            [ div [] [ h6 [] [ text $ props.keyText "key.passenger" ] ]
            , div [] $
                map (\v ->
                  div [ cn "form-check" ]
                  [ input [ cn "form-check-input", _type "checkbox",  value "", _id "c1", checked $ member v state.types 
                          , onChange \_ -> modifyState this _ { types = if member v state.types then delete v state.types else insert v state.types }
                          ]
                  , label  [ cn "form-check-label", htmlFor "c1" ] [ text $ props.keyText $ show v ]
                  ]
                ) [ Medical, Police, Firefighter, Army, Farmacy, Cashier, Regular ]
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
                 , disabled $ isNothing state.mapQ
                 , onClick \_ -> sendDriver this 
                 ] 
          [ text $ props.keyText "key.add"
          ]
        ]
      ]
