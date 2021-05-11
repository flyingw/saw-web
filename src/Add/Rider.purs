module App.Add.Rider
  ( addRiderClass
  , Props
  ) where

import Prelude hiding (div, min, max)

import Data.JSDate (JSDate, now, getTime)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import React (ReactClass, ReactThis, getProps, getState, modifyState, component, createLeafElement, ReactElement)
import React.DOM (text, div, label, input, button, h6, small, iframe, select, option)
import React.DOM.Props (htmlFor, _id, _type, required, autoComplete, value, src, width, height, frameBorder, onClick, disabled)

import Api (PassengerType(..))
import Api.Pull (Pull(AddPassenger), Address)
import Api.Push (UserData, Push(AddRouteOk))
import Proto.BigInt (fromNumber)

import Keys (keyPassengerType)

import Lib.Datepicker (datepickerClass)
import Lib.React(cn, onChangeValue)
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
  , date :: JSDate
  , tpe :: PassengerType
  , from :: Address
  , to :: Address
  , await :: Boolean
  , unsub :: Effect Unit
  }

type This = ReactThis Props State

addRiderClass :: ReactClass Props
addRiderClass = component "AddRider" \this -> do
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
      , tpe: fromMaybe Medical $ props.user <#> _.tpe
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
      , date: fromNumber $ getTime s.date
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

  render :: This -> Effect ReactElement
  render this = do
    props <- getProps this
    state <- getState this
    pure $ 
      div [] []