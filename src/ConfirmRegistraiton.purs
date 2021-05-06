module App.ConfirmRegistration
  ( confirmRegistrationClass
  , Props
  ) where

import Prelude hiding (div)

import Data.Array (fromFoldable, elem, delete, (:))
import Data.JSDate (JSDate, now, getTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, datalist, option, select, form, span)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, min, max, value, src, width, height, frameBorder, onClick, onChange, disabled, checked, list, selected, autoComplete)
import Effect.Class.Console (infoShow)

import Api.Pull (Pull(GetUserData, ConfirmRegistration), Address)
import Api.Push (Push(UserDataOk, ConfirmRegistrationOk, ConfirmRegistrationErr), UserData)
import Lib.React(cn, onChangeValue, onChangeValueInt)
import Lib.WebSocket (Ws)
import Lib.WebSocket as WS

type Props =
  { ws :: Ws
  , keyText :: String -> String
  , lang :: String
  , done :: Effect Unit
  }

type State =
  { unsub :: Effect Unit
  , firstName :: String
  , lastName :: String
  , phone :: String
  , carPlate :: String
  , carColor :: String
  , driver :: Boolean
  , await :: Boolean
  , error :: Maybe String
  }

type This = ReactThis Props State

confirmRegistrationClass :: ReactClass Props
confirmRegistrationClass = component "ConfirmRegistration" \this -> do
  date  <- now
  props <- getProps this
  pure
    { state:
      { unsub: pure unit
      , firstName: ""
      , lastName: ""
      , phone: ""
      , carPlate: ""
      , carColor: ""
      , driver: false
      , await: false
      , error: Nothing
      } :: State
    , render: render this
    , componentDidMount: do
        WS.sub props.ws (onMsg this) >>= \unsub -> modifyState this _{ unsub = unsub }
        WS.snd props.ws $ GetUserData
    , componentWillUnmount: getState this >>= _.unsub
    }
  where
  onMsg :: This -> Maybe Push -> Effect Unit
  onMsg this (Just (UserDataOk r)) = do
    modifyState this _{ firstName = r.userData.firstName
                      , lastName = fromMaybe "" r.userData.lastName
                      , phone = r.userData.phone
                      , carPlate = fromMaybe "" r.userData.carPlate
                      }
    pure unit
  onMsg this (Just (ConfirmRegistrationOk)) = getProps this >>= _.done
  onMsg this (Just (ConfirmRegistrationErr r)) = modifyState this _{ error = r.err }
  onMsg this _ = pure unit

  confirmRegistration :: This -> Effect Unit
  confirmRegistration this = do
    s <- getState this
    p <- getProps this
    _ <- modifyState this _{ await = true }
    let req = ConfirmRegistration { firstName: s.firstName
                                  , lastName: s.lastName
                                  , phone: s.phone
                                  , carPlate: s.carPlate
                                  , carColor: s.carColor
                                  }
    WS.snd p.ws req

  requiredSymbol :: ReactElement
  requiredSymbol = span [ cn "text-danger ml-2"] [ text "*" ]

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $
      div []
      [ div [ cn "d-flex justify-content-center form-row mb-3" ]
        [ h6 [] 
          [ text $ props.keyText "key.confirm_registration.head"
          ]
        ]
      , div [ cn "d-flex justify-content-center form-row mb-3" ] 
        [ span [ cn "col-md-10 col-lg-6 mb-3" ]
          [ text $ props.keyText "key.confirm_registration.text"
          ]
        ] 
      , div [ cn "d-flex justify-content-center form-row" ] 
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "firstName" ] [ text $ props.keyText "key.first_name", requiredSymbol ]
          , input [ _type "text", cn "form-control", _id "firstName", required true 
                  , autoComplete "given-name cc-given-name"
                  , value state.firstName
                  , onChangeValue \v -> modifyState this _{ firstName=v }
                  ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "lastName" ] [ text $ props.keyText "key.last_name" ]
          , input [ _type "text", cn "form-control", _id "lastName", required true
                  , autoComplete "family-name cc-family-name"
                  , value state.lastName
                  , onChangeValue \v -> modifyState this _{ lastName=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.last_name.hint" ]
          ]
        ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ label [ htmlFor "phone" ] [ text $ props.keyText "key.phone", requiredSymbol ]
          , input [ _type "text", cn "form-control", _id "phone", required true 
                  , autoComplete "phone"
                  , value state.phone
                  , onChangeValue \v -> modifyState this _{ phone=v }
                  ]
          , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.phone.hint" ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 d-none d-md-block" ] []
        ]
      , div [ cn "d-flex justify-content-center form-row" ]
        [ div [ cn "col-md-5 col-lg-3 mb-3" ]
          [ div [ cn "form-check" ]
            [ input [ _type "checkbox", cn "form-check-input", _id "i_am_driver" , checked state.driver
                    , onChange \_ -> modifyState this _{ driver = not state.driver }
                    ]
            , label [ htmlFor "i_am_driver", cn "form-check-label" ] [ text $ props.keyText "key.i_am_driver" ]
            ]
          ]
        , div [ cn "col-md-5 col-lg-3 mb-3 d-none d-md-block" ] []
        ]
      , if state.driver
          then
            div [ cn "d-flex justify-content-center form-row" ]
            [ div [ cn "col-md-5 col-lg-3 mb-3" ]
              [ label [ htmlFor "carPlate" ] [ text $ props.keyText "key.car_plate", requiredSymbol ]
              , input [ _type "text", cn "form-control", _id "carPlate", autoComplete "carPlate", required true 
                      , value state.carPlate
                      , onChangeValue \v -> modifyState this _{ carPlate=v }
                      ]
              , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.car_plate.hint" ]
              ]
            , div [ cn "col-md-5 col-lg-3 mb-3" ]
              [ label [ htmlFor "carColor" ] [ text $ props.keyText "key.car_color", requiredSymbol ]
              , input [ _type "text", cn "form-control", _id "carColor", autoComplete "carColor", required true 
                      , value state.carColor
                      , onChangeValue \v -> modifyState this _{ carColor=v }
                      ]
              , small [ cn "form-text text-muted" ] [ text $ props.keyText "key.car_color.hint" ]
              ]
            ]
          else
            mempty
      , div [cn "d-flex justify-content-center form-row" ] 
        [ case state.error of
            Just err -> div [ cn "alert alert-danger" ] [ text $ props.keyText $ "key.err." <> err ]
            Nothing  -> mempty
        ]
      , div [ cn "d-flex justify-content-center form-row" ] 
        [ button  [ cn "btn btn-primary mb-3", _type "button"               
                  , disabled state.await
                  , onClick \_ -> confirmRegistration this 
                  ]
          [ text $ props.keyText "key.confirm_registration"
          , if state.await then div [ cn "spinner-border text-light spinner-border-sm ml-1" ] [] else mempty
          ] 
        ]
      ]