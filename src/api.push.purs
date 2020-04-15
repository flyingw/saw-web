module Api.Push
  ( Push(..)
  , LoginOk
  , UserData
  , AddRouteOk
  , FreeDrivers
  , DriverInfo
  , RouteInfo
  , Location
  , FreePassengers
  , PassengerInfo
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.Array (snoc)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import Api

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Pong | LoginOk LoginOk | AddRouteOk AddRouteOk | FreeDrivers FreeDrivers | FreePassengers FreePassengers
type LoginOk = { user :: UserData }
type LoginOk' = { user :: Maybe UserData }
type UserData = { id :: Number, username :: String, firstName :: Maybe String, lastName :: Maybe String, photo :: Maybe String, phone :: Maybe String, carPlate :: Maybe String, tpe :: Maybe PassengerType, created :: Number }
type UserData' = { id :: Maybe Number, username :: Maybe String, firstName :: Maybe String, lastName :: Maybe String, photo :: Maybe String, phone :: Maybe String, carPlate :: Maybe String, tpe :: Maybe PassengerType, created :: Maybe Number }
type AddRouteOk = { id :: String }
type AddRouteOk' = { id :: Maybe String }
type FreeDrivers = { freeDrivers :: Array DriverInfo }
type DriverInfo = { id :: String, date :: Number, routes :: Array RouteInfo, types :: Array PassengerType }
type DriverInfo' = { id :: Maybe String, date :: Maybe Number, routes :: Array RouteInfo, types :: Array PassengerType }
type RouteInfo = { fromAddress :: String, fromLocation :: Location, toAddress :: String, toLocation :: Location }
type RouteInfo' = { fromAddress :: Maybe String, fromLocation :: Maybe Location, toAddress :: Maybe String, toLocation :: Maybe Location }
type Location = { lat :: Number, lng :: Number }
type Location' = { lat :: Maybe Number, lng :: Maybe Number }
type FreePassengers = { freePassengers :: Array PassengerInfo }
type PassengerInfo = { id :: String, date :: Number, fromAddress :: String, fromLocation :: Location, toAddress :: String, toLocation :: Location, tpe :: PassengerType }
type PassengerInfo' = { id :: Maybe String, date :: Maybe Number, fromAddress :: Maybe String, fromLocation :: Maybe Location, toAddress :: Maybe String, toLocation :: Maybe Location, tpe :: Maybe PassengerType }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodePong _xs_ pos1) \_ -> Pong
    2 -> decode (decodeLoginOk _xs_ pos1) LoginOk
    10 -> decode (decodeAddRouteOk _xs_ pos1) AddRouteOk
    30 -> decode (decodeFreeDrivers _xs_ pos1) FreeDrivers
    40 -> decode (decodeFreePassengers _xs_ pos1) FreePassengers
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodePong :: Uint8Array -> Int -> Decode.Result Unit
decodePong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeLoginOk :: Uint8Array -> Int -> Decode.Result LoginOk
decodeLoginOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { user: Nothing } pos
  case val of
    { user: Just user } -> pure { pos: pos1, val: { user } }
    _ -> Left $ Decode.MissingFields "LoginOk"
    where
    decode :: Int -> LoginOk' -> Int -> Decode.Result' (Step { a :: Int, b :: LoginOk', c :: Int } { pos :: Int, val :: LoginOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeUserData _xs_ pos2) \val -> acc { user = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeUserData :: Uint8Array -> Int -> Decode.Result UserData
decodeUserData _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, username: Nothing, firstName: Nothing, lastName: Nothing, photo: Nothing, phone: Nothing, carPlate: Nothing, tpe: Nothing, created: Nothing } pos
  case val of
    { id: Just id, username: Just username, firstName, lastName, photo, phone, carPlate, tpe, created: Just created } -> pure { pos: pos1, val: { id, username, firstName, lastName, photo, phone, carPlate, tpe, created } }
    _ -> Left $ Decode.MissingFields "UserData"
    where
    decode :: Int -> UserData' -> Int -> Decode.Result' (Step { a :: Int, b :: UserData', c :: Int } { pos :: Int, val :: UserData' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { id = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { username = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { firstName = Just val }
        4 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { lastName = Just val }
        5 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { photo = Just val }
        6 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { phone = Just val }
        7 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { carPlate = Just val }
        8 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { tpe = Just val }
        9 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { created = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePassengerType :: Uint8Array -> Int -> Decode.Result PassengerType
decodePassengerType _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) Nothing pos
    where
    decode :: Int -> Maybe PassengerType -> Int -> Decode.Result' (Step { a :: Int, b :: Maybe PassengerType, c :: Int } { pos :: Int, val :: PassengerType })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
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
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodePolice :: Uint8Array -> Int -> Decode.Result Unit
decodePolice _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeFirefighter :: Uint8Array -> Int -> Decode.Result Unit
decodeFirefighter _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeArmy :: Uint8Array -> Int -> Decode.Result Unit
decodeArmy _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeFarmacy :: Uint8Array -> Int -> Decode.Result Unit
decodeFarmacy _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeCashier :: Uint8Array -> Int -> Decode.Result Unit
decodeCashier _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeRegular :: Uint8Array -> Int -> Decode.Result Unit
decodeRegular _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeAddRouteOk :: Uint8Array -> Int -> Decode.Result AddRouteOk
decodeAddRouteOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing } pos
  case val of
    { id: Just id } -> pure { pos: pos1, val: { id } }
    _ -> Left $ Decode.MissingFields "AddRouteOk"
    where
    decode :: Int -> AddRouteOk' -> Int -> Decode.Result' (Step { a :: Int, b :: AddRouteOk', c :: Int } { pos :: Int, val :: AddRouteOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFreeDrivers :: Uint8Array -> Int -> Decode.Result FreeDrivers
decodeFreeDrivers _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { freeDrivers: [] } pos
    where
    decode :: Int -> FreeDrivers -> Int -> Decode.Result' (Step { a :: Int, b :: FreeDrivers, c :: Int } { pos :: Int, val :: FreeDrivers })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodeDriverInfo _xs_ pos2) \val -> acc { freeDrivers = snoc acc.freeDrivers val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeDriverInfo :: Uint8Array -> Int -> Decode.Result DriverInfo
decodeDriverInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, date: Nothing, routes: [], types: [] } pos
  case val of
    { id: Just id, date: Just date, routes, types } -> pure { pos: pos1, val: { id, date, routes, types } }
    _ -> Left $ Decode.MissingFields "DriverInfo"
    where
    decode :: Int -> DriverInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: DriverInfo', c :: Int } { pos :: Int, val :: DriverInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        6 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { date = Just val }
        8 -> decodeFieldLoop end (decodeRouteInfo _xs_ pos2) \val -> acc { routes = snoc acc.routes val }
        9 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { types = snoc acc.types val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeRouteInfo :: Uint8Array -> Int -> Decode.Result RouteInfo
decodeRouteInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { fromAddress: Nothing, fromLocation: Nothing, toAddress: Nothing, toLocation: Nothing } pos
  case val of
    { fromAddress: Just fromAddress, fromLocation: Just fromLocation, toAddress: Just toAddress, toLocation: Just toLocation } -> pure { pos: pos1, val: { fromAddress, fromLocation, toAddress, toLocation } }
    _ -> Left $ Decode.MissingFields "RouteInfo"
    where
    decode :: Int -> RouteInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: RouteInfo', c :: Int } { pos :: Int, val :: RouteInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { fromAddress = Just val }
        2 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { fromLocation = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { toAddress = Just val }
        4 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { toLocation = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeLocation :: Uint8Array -> Int -> Decode.Result Location
decodeLocation _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { lat: Nothing, lng: Nothing } pos
  case val of
    { lat: Just lat, lng: Just lng } -> pure { pos: pos1, val: { lat, lng } }
    _ -> Left $ Decode.MissingFields "Location"
    where
    decode :: Int -> Location' -> Int -> Decode.Result' (Step { a :: Int, b :: Location', c :: Int } { pos :: Int, val :: Location' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { lat = Just val }
        2 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { lng = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeFreePassengers :: Uint8Array -> Int -> Decode.Result FreePassengers
decodeFreePassengers _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  tailRecM3 decode (pos + msglen) { freePassengers: [] } pos
    where
    decode :: Int -> FreePassengers -> Int -> Decode.Result' (Step { a :: Int, b :: FreePassengers, c :: Int } { pos :: Int, val :: FreePassengers })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (decodePassengerInfo _xs_ pos2) \val -> acc { freePassengers = snoc acc.freePassengers val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodePassengerInfo :: Uint8Array -> Int -> Decode.Result PassengerInfo
decodePassengerInfo _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { id: Nothing, date: Nothing, fromAddress: Nothing, fromLocation: Nothing, toAddress: Nothing, toLocation: Nothing, tpe: Nothing } pos
  case val of
    { id: Just id, date: Just date, fromAddress: Just fromAddress, fromLocation: Just fromLocation, toAddress: Just toAddress, toLocation: Just toLocation, tpe: Just tpe } -> pure { pos: pos1, val: { id, date, fromAddress, fromLocation, toAddress, toLocation, tpe } }
    _ -> Left $ Decode.MissingFields "PassengerInfo"
    where
    decode :: Int -> PassengerInfo' -> Int -> Decode.Result' (Step { a :: Int, b :: PassengerInfo', c :: Int } { pos :: Int, val :: PassengerInfo' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { id = Just val }
        5 -> decodeFieldLoop end (Decode.double _xs_ pos2) \val -> acc { date = Just val }
        6 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { fromAddress = Just val }
        7 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { fromLocation = Just val }
        8 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { toAddress = Just val }
        9 -> decodeFieldLoop end (decodeLocation _xs_ pos2) \val -> acc { toLocation = Just val }
        10 -> decodeFieldLoop end (decodePassengerType _xs_ pos2) \val -> acc { tpe = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }