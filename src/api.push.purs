module Api.Push
  ( Push(..)
  , SessionData
  , UserStatus(..)
  , Active
  , UserData
  , defaultUserData
  , AddRouteOk
  , AddRouteErr
  , defaultAddRouteErr
  , FreeDrivers
  , defaultFreeDrivers
  , DriverInfo
  , defaultDriverInfo
  , RouteInfo
  , FreePassengers
  , defaultFreePassengers
  , PassengerInfo
  , CitiesList
  , defaultCitiesList
  , UserDataOk
  , ConfirmRegistrationErr
  , defaultConfirmRegistrationErr
  , GetAutocompleteOk
  , defaultGetAutocompleteOk
  , GetDirectionsOk
  , defaultGetDirectionsOk
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.Either (Either(Left))
import Data.Eq (class Eq)
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<), (<<<))
import Proto.BigInt (BigInt)
import Proto.Decode as Decode
import Proto.Uint8Array (Uint8Array)
import Api

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Pong | LoginOk | LoginErr | SessionData SessionData | AddRouteOk AddRouteOk | AddRouteErr AddRouteErr | FreeDrivers FreeDrivers | FreePassengers FreePassengers | CitiesList CitiesList | UserDataOk UserDataOk | ConfirmRegistrationOk | ConfirmRegistrationErr ConfirmRegistrationErr | GetAutocompleteOk GetAutocompleteOk | GetDirectionsOk GetDirectionsOk
derive instance eqPush :: Eq Push
type SessionData = { status :: UserStatus }
type SessionData' = { status :: Maybe UserStatus }
data UserStatus = Active Active | Guest
derive instance eqUserStatus :: Eq UserStatus
type Active = { user :: UserData }
type Active' = { user :: Maybe UserData }
type UserData = { username :: Maybe String, firstName :: Maybe String, lastName :: Maybe String, photo :: Maybe String, phone :: Maybe String, carPlate :: Maybe String, tpe :: PassengerType }
defaultUserData :: { username :: Maybe String, firstName :: Maybe String, lastName :: Maybe String, photo :: Maybe String, phone :: Maybe String, carPlate :: Maybe String }
defaultUserData = { username: Nothing, firstName: Nothing, lastName: Nothing, photo: Nothing, phone: Nothing, carPlate: Nothing }
type UserData' = { username :: Maybe String, firstName :: Maybe String, lastName :: Maybe String, photo :: Maybe String, phone :: Maybe String, carPlate :: Maybe String, tpe :: Maybe PassengerType }
type AddRouteOk = { id :: String }
type AddRouteOk' = { id :: Maybe String }
type AddRouteErr = { err :: Maybe String }
defaultAddRouteErr :: { err :: Maybe String }
defaultAddRouteErr = { err: Nothing }
type FreeDrivers = { freeDrivers :: Array DriverInfo }
defaultFreeDrivers :: { freeDrivers :: Array DriverInfo }
defaultFreeDrivers = { freeDrivers: [] }
type DriverInfo = { id :: String, date :: BigInt, routes :: Array RouteInfo, types :: Array PassengerType }
defaultDriverInfo :: { routes :: Array RouteInfo, types :: Array PassengerType }
defaultDriverInfo = { routes: [], types: [] }
type DriverInfo' = { id :: Maybe String, date :: Maybe BigInt, routes :: Array RouteInfo, types :: Array PassengerType }
type RouteInfo = { fromAddress :: String, fromLocation :: Location, toAddress :: String, toLocation :: Location }
type RouteInfo' = { fromAddress :: Maybe String, fromLocation :: Maybe Location, toAddress :: Maybe String, toLocation :: Maybe Location }
type Location' = { lat :: Maybe Number, lng :: Maybe Number }
type FreePassengers = { freePassengers :: Array PassengerInfo }
defaultFreePassengers :: { freePassengers :: Array PassengerInfo }
defaultFreePassengers = { freePassengers: [] }
type PassengerInfo = { id :: String, date :: BigInt, fromAddress :: String, fromLocation :: Location, toAddress :: String, toLocation :: Location, tpe :: PassengerType }
type PassengerInfo' = { id :: Maybe String, date :: Maybe BigInt, fromAddress :: Maybe String, fromLocation :: Maybe Location, toAddress :: Maybe String, toLocation :: Maybe Location, tpe :: Maybe PassengerType }
type CitiesList = { cities :: Array String }
defaultCitiesList :: { cities :: Array String }
defaultCitiesList = { cities: [] }
type UserDataOk = { userData :: UserData }
type UserDataOk' = { userData :: Maybe UserData }
type ConfirmRegistrationErr = { err :: Maybe String }
defaultConfirmRegistrationErr :: { err :: Maybe String }
defaultConfirmRegistrationErr = { err: Nothing }
type GetAutocompleteOk = { predictions :: Array Waypoint }
defaultGetAutocompleteOk :: { predictions :: Array Waypoint }
defaultGetAutocompleteOk = { predictions: [] }
type Waypoint' = { description :: Maybe String, tpe :: Maybe WaypointType, placeId :: Maybe String }
type GetDirectionsOk = { routes :: Array String }
defaultGetDirectionsOk :: { routes :: Array String }
defaultGetDirectionsOk = { routes: [] }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.unsignedVarint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodePong _xs_ pos1) \_ -> Pong
    2 -> decode (decodeLoginOk _xs_ pos1) \_ -> LoginOk
    3 -> decode (decodeLoginErr _xs_ pos1) \_ -> LoginErr
    4 -> decode (decodeSessionData _xs_ pos1) SessionData
    10 -> decode (decodeAddRouteOk _xs_ pos1) AddRouteOk
    11 -> decode (decodeAddRouteErr _xs_ pos1) AddRouteErr
    30 -> decode (decodeFreeDrivers _xs_ pos1) FreeDrivers
    40 -> decode (decodeFreePassengers _xs_ pos1) FreePassengers
    50 -> decode (decodeCitiesList _xs_ pos1) CitiesList
    60 -> decode (decodeUserDataOk _xs_ pos1) UserDataOk
    61 -> decode (decodeConfirmRegistrationOk _xs_ pos1) \_ -> ConfirmRegistrationOk
    62 -> decode (decodeConfirmRegistrationErr _xs_ pos1) ConfirmRegistrationErr
    70 -> decode (decodeGetAutocompleteOk _xs_ pos1) GetAutocompleteOk
    80 -> decode (decodeGetDirectionsOk _xs_ pos1) GetDirectionsOk
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodePong :: Uint8Array -> Int -> Decode.Result Unit
decodePong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeLoginOk :: Uint8Array -> Int -> Decode.Result Unit
decodeLoginOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeLoginErr :: Uint8Array -> Int -> Decode.Result Unit
decodeLoginErr _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeSessionData :: Uint8Array -> Int -> Decode.Result SessionData
decodeSessionData _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { status: Nothing } pos
  case val of
    { status: Just status } -> pure { pos: pos1, val: { status } }
    _ -> Left $ Decode.MissingFields "SessionData"
    where
    decode :: Int -> SessionData' -> Int -> Decode.Result' (Step { a :: Int, b :: SessionData', c :: Int } { pos :: Int, val :: SessionData' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeUserStatus _xs_ pos2) \val -> acc { status = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeUserStatus :: Uint8Array -> Int -> Decode.Result UserStatus
decodeUserStatus _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe UserStatus -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe UserStatus, c :: Int } { pos :: Int, val :: UserStatus })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeActive _xs_ pos2) (Just <<< Active)
        2 -> decodeFieldLoop end (decodeGuest _xs_ pos2) \_ -> Just Guest
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "UserStatus"

decodeActive :: Uint8Array -> Int -> Decode.Result Active
decodeActive _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { user: Nothing } pos
  case val of
    { user: Just user } -> pure { pos: pos1, val: { user } }
    _ -> Left $ Decode.MissingFields "Active"
    where
    decode :: Int -> Active' -> Int -> Decode.Result' (Step { a :: Int, b :: Active', c :: Int } { pos :: Int, val :: Active' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeUserData _xs_ pos2) \val -> acc { user = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeUserData :: Uint8Array -> Int -> Decode.Result UserData
decodeUserData _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { username: Nothing, firstName: Nothing, lastName: Nothing, photo: Nothing, phone: Nothing, carPlate: Nothing, tpe: Nothing } pos
  case val of
    { username, firstName, lastName, photo, phone, carPlate, tpe: Just tpe } -> pure { pos: pos1, val: { username, firstName, lastName, photo, phone, carPlate, tpe } }
    _ -> Left $ Decode.MissingFields "UserData"
    where
    decode :: Int -> UserData' -> Int -> Decode.Result' (Step { a :: Int, b :: UserData', c :: Int } { pos :: Int, val :: UserData' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { username = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { firstName = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { lastName = Just val }
        4 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { photo = Just val }
        5 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { phone = Just val }
        6 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { carPlate = Just val }
        7 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { tpe = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePassengerType :: Uint8Array -> Int -> Decode.Result PassengerType
decodePassengerType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe PassengerType -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe PassengerType, c :: Int } { pos :: Int, val :: PassengerType })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeMedical _xs_ pos2) \_ -> Just Medical
        2 -> decodeFieldLoop end (decodePolice _xs_ pos2) \_ -> Just Police
        3 -> decodeFieldLoop end (decodeFirefighter _xs_ pos2) \_ -> Just Firefighter
        4 -> decodeFieldLoop end (decodeArmy _xs_ pos2) \_ -> Just Army
        5 -> decodeFieldLoop end (decodeFarmacy _xs_ pos2) \_ -> Just Farmacy
        6 -> decodeFieldLoop end (decodeCashier _xs_ pos2) \_ -> Just Cashier
        7 -> decodeFieldLoop end (decodeRegular _xs_ pos2) \_ -> Just Regular
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "PassengerType"

decodeMedical :: Uint8Array -> Int -> Decode.Result Unit
decodeMedical _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodePolice :: Uint8Array -> Int -> Decode.Result Unit
decodePolice _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeFirefighter :: Uint8Array -> Int -> Decode.Result Unit
decodeFirefighter _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeArmy :: Uint8Array -> Int -> Decode.Result Unit
decodeArmy _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeFarmacy :: Uint8Array -> Int -> Decode.Result Unit
decodeFarmacy _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeCashier :: Uint8Array -> Int -> Decode.Result Unit
decodeCashier _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeRegular :: Uint8Array -> Int -> Decode.Result Unit
decodeRegular _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeGuest :: Uint8Array -> Int -> Decode.Result Unit
decodeGuest _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeAddRouteOk :: Uint8Array -> Int -> Decode.Result AddRouteOk
decodeAddRouteOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing } pos
  case val of
    { id: Just id } -> pure { pos: pos1, val: { id } }
    _ -> Left $ Decode.MissingFields "AddRouteOk"
    where
    decode :: Int -> AddRouteOk' -> Int -> Decode.Result' (Step { a :: Int, b :: AddRouteOk', c :: Int } { pos :: Int, val :: AddRouteOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeAddRouteErr :: Uint8Array -> Int -> Decode.Result AddRouteErr
decodeAddRouteErr _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { err: Nothing } pos
    where
    decode :: Int -> AddRouteErr -> Int -> Decode.Result' (Step { a :: Int, b :: AddRouteErr, c :: Int } { pos :: Int, val :: AddRouteErr })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { err = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFreeDrivers :: Uint8Array -> Int -> Decode.Result FreeDrivers
decodeFreeDrivers _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { freeDrivers: [] } pos
    where
    decode :: Int -> FreeDrivers -> Int -> Decode.Result' (Step { a :: Int, b :: FreeDrivers, c :: Int } { pos :: Int, val :: FreeDrivers })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeDriverInfo _xs_ pos2) \val -> acc { freeDrivers = snoc acc.freeDrivers val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeDriverInfo :: Uint8Array -> Int -> Decode.Result DriverInfo
decodeDriverInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, date: Nothing, routes: [], types: [] } pos
  case val of
    { id: Just id, date: Just date, routes, types } -> pure { pos: pos1, val: { id, date, routes, types } }
    _ -> Left $ Decode.MissingFields "DriverInfo"
    where
    decode :: Int -> DriverInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: DriverInfo', c :: Int } { pos :: Int, val :: DriverInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        6 -> decodeFieldLoop end (Decode.bigInt _xs_ pos2) \val -> acc { date = Just val }
        8 -> decodeFieldLoop end (decodeRouteInfo _xs_ pos2) \val -> acc { routes = snoc acc.routes val }
        9 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { types = snoc acc.types val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeRouteInfo :: Uint8Array -> Int -> Decode.Result RouteInfo
decodeRouteInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { fromAddress: Nothing, fromLocation: Nothing, toAddress: Nothing, toLocation: Nothing } pos
  case val of
    { fromAddress: Just fromAddress, fromLocation: Just fromLocation, toAddress: Just toAddress, toLocation: Just toLocation } -> pure { pos: pos1, val: { fromAddress, fromLocation, toAddress, toLocation } }
    _ -> Left $ Decode.MissingFields "RouteInfo"
    where
    decode :: Int -> RouteInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: RouteInfo', c :: Int } { pos :: Int, val :: RouteInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { fromAddress = Just val }
        2 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { fromLocation = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { toAddress = Just val }
        4 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { toLocation = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeLocation :: Uint8Array -> Int -> Decode.Result Location
decodeLocation _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { lat: Nothing, lng: Nothing } pos
  case val of
    { lat: Just lat, lng: Just lng } -> pure { pos: pos1, val: { lat, lng } }
    _ -> Left $ Decode.MissingFields "Location"
    where
    decode :: Int -> Location' -> Int -> Decode.Result' (Step { a :: Int, b :: Location', c :: Int } { pos :: Int, val :: Location' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { lat = Just val }
        2 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { lng = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFreePassengers :: Uint8Array -> Int -> Decode.Result FreePassengers
decodeFreePassengers _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { freePassengers: [] } pos
    where
    decode :: Int -> FreePassengers -> Int -> Decode.Result' (Step { a :: Int, b :: FreePassengers, c :: Int } { pos :: Int, val :: FreePassengers })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodePassengerInfo _xs_ pos2) \val -> acc { freePassengers = snoc acc.freePassengers val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePassengerInfo :: Uint8Array -> Int -> Decode.Result PassengerInfo
decodePassengerInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, date: Nothing, fromAddress: Nothing, fromLocation: Nothing, toAddress: Nothing, toLocation: Nothing, tpe: Nothing } pos
  case val of
    { id: Just id, date: Just date, fromAddress: Just fromAddress, fromLocation: Just fromLocation, toAddress: Just toAddress, toLocation: Just toLocation, tpe: Just tpe } -> pure { pos: pos1, val: { id, date, fromAddress, fromLocation, toAddress, toLocation, tpe } }
    _ -> Left $ Decode.MissingFields "PassengerInfo"
    where
    decode :: Int -> PassengerInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: PassengerInfo', c :: Int } { pos :: Int, val :: PassengerInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        5 -> decodeFieldLoop end (Decode.bigInt _xs_ pos2) \val -> acc { date = Just val }
        6 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { fromAddress = Just val }
        7 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { fromLocation = Just val }
        8 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { toAddress = Just val }
        9 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { toLocation = Just val }
        10 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { tpe = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeCitiesList :: Uint8Array -> Int -> Decode.Result CitiesList
decodeCitiesList _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { cities: [] } pos
    where
    decode :: Int -> CitiesList -> Int -> Decode.Result' (Step { a :: Int, b :: CitiesList, c :: Int } { pos :: Int, val :: CitiesList })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { cities = snoc acc.cities val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeUserDataOk :: Uint8Array -> Int -> Decode.Result UserDataOk
decodeUserDataOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { userData: Nothing } pos
  case val of
    { userData: Just userData } -> pure { pos: pos1, val: { userData } }
    _ -> Left $ Decode.MissingFields "UserDataOk"
    where
    decode :: Int -> UserDataOk' -> Int -> Decode.Result' (Step { a :: Int, b :: UserDataOk', c :: Int } { pos :: Int, val :: UserDataOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeUserData _xs_ pos2) \val -> acc { userData = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeConfirmRegistrationOk :: Uint8Array -> Int -> Decode.Result Unit
decodeConfirmRegistrationOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeConfirmRegistrationErr :: Uint8Array -> Int -> Decode.Result ConfirmRegistrationErr
decodeConfirmRegistrationErr _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { err: Nothing } pos
    where
    decode :: Int -> ConfirmRegistrationErr -> Int -> Decode.Result' (Step { a :: Int, b :: ConfirmRegistrationErr, c :: Int } { pos :: Int, val :: ConfirmRegistrationErr })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { err = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeGetAutocompleteOk :: Uint8Array -> Int -> Decode.Result GetAutocompleteOk
decodeGetAutocompleteOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { predictions: [] } pos
    where
    decode :: Int -> GetAutocompleteOk -> Int -> Decode.Result' (Step { a :: Int, b :: GetAutocompleteOk, c :: Int } { pos :: Int, val :: GetAutocompleteOk })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeWaypoint _xs_ pos2) \val -> acc { predictions = snoc acc.predictions val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeWaypoint :: Uint8Array -> Int -> Decode.Result Waypoint
decodeWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { description: Nothing, tpe: Nothing, placeId: Nothing } pos
  case val of
    { description: Just description, tpe: Just tpe, placeId } -> pure { pos: pos1, val: { description, tpe, placeId } }
    _ -> Left $ Decode.MissingFields "Waypoint"
    where
    decode :: Int -> Waypoint' -> Int -> Decode.Result' (Step { a :: Int, b :: Waypoint', c :: Int } { pos :: Int, val :: Waypoint' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { description = Just val }
        2 -> decodeFieldLoop end (decodeWaypointType _xs_ pos2) \val -> acc { tpe = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { placeId = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeWaypointType :: Uint8Array -> Int -> Decode.Result WaypointType
decodeWaypointType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe WaypointType -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe WaypointType, c :: Int } { pos :: Int, val :: WaypointType })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeSubwayWaypoint _xs_ pos2) \_ -> Just SubwayWaypoint
        2 -> decodeFieldLoop end (decodeBusStationWaypoint _xs_ pos2) \_ -> Just BusStationWaypoint
        3 -> decodeFieldLoop end (decodeAirportWaypoint _xs_ pos2) \_ -> Just AirportWaypoint
        4 -> decodeFieldLoop end (decodeRouteWaypoint _xs_ pos2) \_ -> Just RouteWaypoint
        5 -> decodeFieldLoop end (decodeStreetAddressWaypoint _xs_ pos2) \_ -> Just StreetAddressWaypoint
        6 -> decodeFieldLoop end (decodeLocalityWaypoint _xs_ pos2) \_ -> Just LocalityWaypoint
        7 -> decodeFieldLoop end (decodeShoppingMallWaypoint _xs_ pos2) \_ -> Just ShoppingMallWaypoint
        8 -> decodeFieldLoop end (decodePointOfInterest _xs_ pos2) \_ -> Just PointOfInterest
        9 -> decodeFieldLoop end (decodeUnknownWaypoint _xs_ pos2) \_ -> Just UnknownWaypoint
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end (Just acc) pos1 = pure $ Done { pos: pos1, val: acc }
    decode end acc@Nothing pos1 = Left $ Decode.MissingFields "WaypointType"

decodeSubwayWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeSubwayWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeBusStationWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeBusStationWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeAirportWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeAirportWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeRouteWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeRouteWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeStreetAddressWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeStreetAddressWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeLocalityWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeLocalityWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeShoppingMallWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeShoppingMallWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodePointOfInterest :: Uint8Array -> Int -> Decode.Result Unit
decodePointOfInterest _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeUnknownWaypoint :: Uint8Array -> Int -> Decode.Result Unit
decodeUnknownWaypoint _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeGetDirectionsOk :: Uint8Array -> Int -> Decode.Result GetDirectionsOk
decodeGetDirectionsOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.unsignedVarint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { routes: [] } pos
    where
    decode :: Int -> GetDirectionsOk -> Int -> Decode.Result' (Step { a :: Int, b :: GetDirectionsOk, c :: Int } { pos :: Int, val :: GetDirectionsOk })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.unsignedVarint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { routes = snoc acc.routes val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }