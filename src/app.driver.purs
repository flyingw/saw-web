module App.Driver
  ( driverClass
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
import React.DOM (text, div, form, label, input, button, h6, small, iframe)
import React.DOM.Props (htmlFor, placeholder, _id, _type, noValidate, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, value, disabled)

import Lib.React(cn, targetValue, onChangeValue, onChangeValueInt)
import Lib.WebSocket (WebSocket)
import Lib.WebSocket as WS

import Api (Address)
import Api.Pull (Pull(AddDriver), encodePull)
import Api.Push (decodePush, Push(Pong, AddRouteOk))

type Props =
  { ws :: WebSocket
  , name :: Maybe String
  }

type State =
  { answer :: String
  , mapQ :: Maybe String
  , routeN :: Maybe String
  , name :: String
  , phone :: String
  , carPlate :: String
  , date :: String
  , lap :: Int
  , seats :: Int
  , from :: Address
  , to :: Address
  }

driverClass :: ReactClass Props
driverClass = component "Driver" \this -> do
  date <- today
  props <- getProps this
  pure
    { state:
      { answer: "press the button"
      , mapQ: Nothing
      , routeN: Nothing
      , name: fromMaybe "" props.name
      , phone: ""
      , carPlate: ""
      , date: date
      , lap: 3
      , seats: 1  
      , from: mempty
      , to: mempty
      } :: State
    , render: render this
    , componentDidMount: do
        props <- getProps this
        let ws = props.ws
        WS.onMsg ws (\x -> case decodePush x of
          Left y -> error $ show y
          Right msg -> handle this msg.val
        ) (sequence <<< map error)
    }
  where

  today :: Effect String 
  today = now >>= toISOString <#> (take 19)

  handle :: ReactThis Props State -> Push -> Effect Unit
  handle this msg =
    case msg of
      AddRouteOk r -> do
        p <- getProps this
        modifyState this _{ routeN=Just r.n }
      msg -> pure unit
  
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
      }
    WS.send p.ws $ encodePull driver


  updateMap :: ReactThis Props State -> Effect Unit
  updateMap this = do
    s <- getState this
    let host = "https://www.google.com/maps/embed/v1/directions"
    let origin = fromMaybe "" $ encodeURI $ s.from.street <> ", " <> s.from.building <> ", " <> s.from.city
    let destination = fromMaybe "" $ encodeURI $ s.to.street <> ", " <> s.to.building <> ", " <> s.to.city
    let key = "AIzaSyAijTi54AQ6VowZ-PMGUb7w1MFK54Zrwbw"
    let q = host <> "?origin=" <> origin <> "&destination=" <> destination <> "&key=" <> key
    modifyState this \state -> state{ mapQ = Just q }


  render this = ado
    props <- getProps this
    state <- getState this
    in div [ cn "m-2  " ]
      [ form [ noValidate true ]
        [ h6 [] [ text "Данные водителя" ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "name" ] [ text "Имя" ]
            , input [ _type "text", cn "form-control", _id "name", required true 
                    , onChangeValue \v -> modifyState this _{ name=v }
                    ]
            ]
          , div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "phone" ] [ text "Телефон" ]
            , input [ _type "text", cn "form-control", _id "phone", autoComplete "phone", required true 
                    , onChangeValue \v -> modifyState this _{ phone=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text "Виден только пасажиру" ]
            ]
          , div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "carPlate" ] [ text "Номер машины" ]
            , input [ _type "text", cn "form-control", _id "carPlate", autoComplete "carPlate", required true 
                    , onChangeValue \v -> modifyState this _{ carPlate=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text "Виден только пасажиру" ]
            ]
          ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "date" ] [ text "Дата" ]
            , input [ _type "datetime-local", cn "form-control", _id "date", required true
                    , value state.date
                    , onChangeValue \v -> modifyState this _{ date=v }
                    ]
            ]
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "lap" ] [ text "Заезд (км)" ]
            , input [ _type "number", cn "form-control", _id "lap", min "1", max "100", value "3", required true 
                    , onChangeValueInt \v -> modifyState this _{ lap=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text "По маршруту" ]
            ]
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "seats" ] [ text "Мест" ]
            , input [ _type "number", cn "form-control", _id "seats", min "1", max "5", value "1", required true 
                    , onChangeValueInt \v -> modifyState this _{ seats=v }
                    ]
            , small [ cn "form-text text-muted" ] [ text "Макс (1-5)" ]
            ]
          ]
        , div [] [ h6 [] [ text "Начало маршрута" ] ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "cityFrom" ] [ text "Город" ]
            , input [ _type "text", cn "form-control", _id "cityFrom", required true
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ city=v } }
                    ]
            ]
          , div [ cn "col-md-3 mb-3" ]
            [ label [ htmlFor "streetFrom" ] [ text "Улица" ]
            , input [ _type "text", cn "form-control", _id "streetFrom", required true
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ street=v } }
                    ]
            ] 
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "buildingFrom" ] [ text "Дом" ]
            , input [ _type "text", cn "form-control", _id "buildingFrom", required true
                    , onChangeValue \v -> modifyState this \s -> s{ from=s.from{ building=v } }
                    ]
            ]
          ]
        , div [] [ h6 [] [ text "Конец маршрута" ] ]
        , div [ cn "form-row" ]
          [ div [ cn "col-md-2 mb-3" ]
            [ label [ htmlFor "cityTo" ] [ text "Город" ]
            , input [ _type "text", cn "form-control", _id "cityTo", required true
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ city=v } } 
                    ]
            ]
          , div [ cn "col-md-3 mb-3" ]
            [ label [ htmlFor "streetTo" ] [ text "Улица" ]
            , input [ _type "text", cn "form-control", _id "streetTo", required true
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ street=v } } 
                    ]
            ]
          , div [ cn "col-md-1 mb-3" ]
            [ label [ htmlFor "houseTo" ] [ text "Дом" ]
            , input [ _type "text", cn "form-control", _id "houseTo", required true
                    , onChangeValue \v -> modifyState this \s -> s{ to=s.to{ building=v } } 
                    ]
            ]
          ]
        , button [ cn "btn btn-secondary mb-3", _type "button", onClick \_ -> updateMap this ] [ text "Посмотреть маршрут" ]
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
            , label [ htmlFor "agree_terms", cn "form-check-label" ] [ text "Я согласен с условиями и положениями использования сервиса" ]
            ]
          , div [ cn "form-check" ]
            [ input [ _type "checkbox", cn "form-check-input", _id "agree_rules" ]
            , label [ htmlFor "agree_rules", cn "form-check-label" ] [ text "Я ознакомился с правилами безопасности" ]
            ]
          ]
        , div [ cn "alert alert-info col-md-6" ] [ text "Перед добавлением посмотрите предпологаемый маршрут" ]
        , button [ cn "btn btn-primary mb-3", _type "button", disabled $ isNothing state.mapQ, onClick \_ -> sendDriver this ] [ text "Добавить" ]
        ]
      ]

  -- <div class="form-group">
  --   <div class="form-check">
  --     <input class="form-check-input" type="checkbox" value="" id="invalidCheck" required>
  --     <label class="form-check-label" for="invalidCheck">
  --       Согласен с условиями и положениями
  --     </label>
  --     <div class="invalid-feedback">
  --       You must agree before submitting.
  --     </div>
  --   </div>
  -- </div>
  -- <button class="btn btn-primary" type="submit">Submit form</button>

  --     ]
