module Api.Pull
  ( Pull(..)
  , TelegramLogin
  , defaultTelegramLogin
  , TelegramData(..)
  , TelegramString
  , TelegramNum
  , SimpleLogin
  , AddDriver
  , defaultAddDriver
  , Address
  , AddPassenger
  , GetFreeDrivers
  , GetFreePassengers
  , GetCitiesList
  , ConfirmRegistration
  , GetAutocomplete
  , defaultGetAutocomplete
  , GetDirections
  , defaultGetDirections
  , GetRevGeocoding
  , AddDriverRoute
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), fromMaybe)
import Prelude (map, ($))
import Proto.BigInt (BigInt)
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll, fromArray)
import Api

data Pull = Ping | Logout | TelegramLogin TelegramLogin | SimpleLogin SimpleLogin | AddDriver AddDriver | AddPassenger AddPassenger | GetFreeDrivers GetFreeDrivers | GetFreePassengers GetFreePassengers | GetCitiesList GetCitiesList | GetUserData | ConfirmRegistration ConfirmRegistration | GetAutocomplete GetAutocomplete | GetDirections GetDirections | GetRevGeocoding GetRevGeocoding | AddDriverRoute AddDriverRoute
derive instance eqPull :: Eq Pull
type TelegramLogin = { d :: Array TelegramData }
defaultTelegramLogin :: { d :: Array TelegramData }
defaultTelegramLogin = { d: [] }
data TelegramData = TelegramString TelegramString | TelegramNum TelegramNum
derive instance eqTelegramData :: Eq TelegramData
type TelegramString = { key :: String, value :: String }
type TelegramNum = { key :: String, value :: Number }
type SimpleLogin = { f1 :: String, f2 :: String }
type AddDriver = { firstName :: String, lastName :: String, phone :: String, carPlate :: String, date :: BigInt, deviationDistance :: Int, deviationTime :: Int, seats :: Int, from :: Address, to :: Address, types :: Array PassengerType, lang :: String }
defaultAddDriver :: { types :: Array PassengerType }
defaultAddDriver = { types: [] }
type Address = { country :: String, city :: String, street :: String, building :: String }
type AddPassenger = { firstName :: String, lastName :: String, phone :: String, date :: BigInt, tpe :: PassengerType, from :: Address, to :: Address }
type GetFreeDrivers = { date :: BigInt }
type GetFreePassengers = { date :: BigInt }
type GetCitiesList = { country :: String, lang :: String }
type ConfirmRegistration = { firstName :: String, lastName :: String, phone :: String, carPlate :: String, carColor :: String }
type GetAutocomplete = { text :: String, location :: Maybe Location, lang :: String }
defaultGetAutocomplete :: { location :: Maybe Location }
defaultGetAutocomplete = { location: Nothing }
type GetDirections = { waypoints :: Array Waypoint, departure :: BigInt, lang :: String }
defaultGetDirections :: { waypoints :: Array Waypoint }
defaultGetDirections = { waypoints: [] }
type GetRevGeocoding = { location :: Location, lang :: String }
type AddDriverRoute = { date :: BigInt, route :: String }

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.unsignedVarint32 10, encodePing ]
encodePull Logout = concatAll [ Encode.unsignedVarint32 18, encodeLogout ]
encodePull (TelegramLogin x) = concatAll [ Encode.unsignedVarint32 26, encodeTelegramLogin x ]
encodePull (SimpleLogin x) = concatAll [ Encode.unsignedVarint32 34, encodeSimpleLogin x ]
encodePull (AddDriver x) = concatAll [ Encode.unsignedVarint32 82, encodeAddDriver x ]
encodePull (AddPassenger x) = concatAll [ Encode.unsignedVarint32 162, encodeAddPassenger x ]
encodePull (GetFreeDrivers x) = concatAll [ Encode.unsignedVarint32 242, encodeGetFreeDrivers x ]
encodePull (GetFreePassengers x) = concatAll [ Encode.unsignedVarint32 322, encodeGetFreePassengers x ]
encodePull (GetCitiesList x) = concatAll [ Encode.unsignedVarint32 402, encodeGetCitiesList x ]
encodePull GetUserData = concatAll [ Encode.unsignedVarint32 482, encodeGetUserData ]
encodePull (ConfirmRegistration x) = concatAll [ Encode.unsignedVarint32 490, encodeConfirmRegistration x ]
encodePull (GetAutocomplete x) = concatAll [ Encode.unsignedVarint32 562, encodeGetAutocomplete x ]
encodePull (GetDirections x) = concatAll [ Encode.unsignedVarint32 642, encodeGetDirections x ]
encodePull (GetRevGeocoding x) = concatAll [ Encode.unsignedVarint32 722, encodeGetRevGeocoding x ]
encodePull (AddDriverRoute x) = concatAll [ Encode.unsignedVarint32 802, encodeAddDriverRoute x ]

encodePing :: Uint8Array
encodePing = Encode.unsignedVarint32 0

encodeLogout :: Uint8Array
encodeLogout = Encode.unsignedVarint32 0

encodeTelegramLogin :: TelegramLogin -> Uint8Array
encodeTelegramLogin msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeTelegramData x ]) msg.d
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeTelegramData :: TelegramData -> Uint8Array
encodeTelegramData (TelegramString x) = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeTelegramString x ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeTelegramData (TelegramNum x) = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodeTelegramNum x ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeTelegramString :: TelegramString -> Uint8Array
encodeTelegramString msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.key
        , Encode.unsignedVarint32 18
        , Encode.string msg.value
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeTelegramNum :: TelegramNum -> Uint8Array
encodeTelegramNum msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.key
        , Encode.unsignedVarint32 17
        , Encode.double msg.value
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeSimpleLogin :: SimpleLogin -> Uint8Array
encodeSimpleLogin msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.f1
        , Encode.unsignedVarint32 18
        , Encode.string msg.f2
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAddDriver :: AddDriver -> Uint8Array
encodeAddDriver msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.firstName
        , Encode.unsignedVarint32 18
        , Encode.string msg.lastName
        , Encode.unsignedVarint32 26
        , Encode.string msg.phone
        , Encode.unsignedVarint32 34
        , Encode.string msg.carPlate
        , Encode.unsignedVarint32 40
        , Encode.bigInt msg.date
        , Encode.unsignedVarint32 48
        , Encode.signedVarint32 msg.deviationDistance
        , Encode.unsignedVarint32 56
        , Encode.signedVarint32 msg.deviationTime
        , Encode.unsignedVarint32 64
        , Encode.signedVarint32 msg.seats
        , Encode.unsignedVarint32 74
        , encodeAddress msg.from
        , Encode.unsignedVarint32 82
        , encodeAddress msg.to
        , concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 90, encodePassengerType x ]) msg.types
        , Encode.unsignedVarint32 98
        , Encode.string msg.lang
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAddress :: Address -> Uint8Array
encodeAddress msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.country
        , Encode.unsignedVarint32 18
        , Encode.string msg.city
        , Encode.unsignedVarint32 26
        , Encode.string msg.street
        , Encode.unsignedVarint32 34
        , Encode.string msg.building
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodePassengerType :: PassengerType -> Uint8Array
encodePassengerType Medical = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeMedical ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Police = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodePolice ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Firefighter = do
  let xs = concatAll [ Encode.unsignedVarint32 26, encodeFirefighter ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Army = do
  let xs = concatAll [ Encode.unsignedVarint32 34, encodeArmy ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Farmacy = do
  let xs = concatAll [ Encode.unsignedVarint32 42, encodeFarmacy ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Cashier = do
  let xs = concatAll [ Encode.unsignedVarint32 50, encodeCashier ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodePassengerType Regular = do
  let xs = concatAll [ Encode.unsignedVarint32 58, encodeRegular ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeMedical :: Uint8Array
encodeMedical = Encode.unsignedVarint32 0

encodePolice :: Uint8Array
encodePolice = Encode.unsignedVarint32 0

encodeFirefighter :: Uint8Array
encodeFirefighter = Encode.unsignedVarint32 0

encodeArmy :: Uint8Array
encodeArmy = Encode.unsignedVarint32 0

encodeFarmacy :: Uint8Array
encodeFarmacy = Encode.unsignedVarint32 0

encodeCashier :: Uint8Array
encodeCashier = Encode.unsignedVarint32 0

encodeRegular :: Uint8Array
encodeRegular = Encode.unsignedVarint32 0

encodeAddPassenger :: AddPassenger -> Uint8Array
encodeAddPassenger msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.firstName
        , Encode.unsignedVarint32 18
        , Encode.string msg.lastName
        , Encode.unsignedVarint32 26
        , Encode.string msg.phone
        , Encode.unsignedVarint32 32
        , Encode.bigInt msg.date
        , Encode.unsignedVarint32 42
        , encodePassengerType msg.tpe
        , Encode.unsignedVarint32 50
        , encodeAddress msg.from
        , Encode.unsignedVarint32 58
        , encodeAddress msg.to
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetFreeDrivers :: GetFreeDrivers -> Uint8Array
encodeGetFreeDrivers msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.bigInt msg.date
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetFreePassengers :: GetFreePassengers -> Uint8Array
encodeGetFreePassengers msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.bigInt msg.date
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetCitiesList :: GetCitiesList -> Uint8Array
encodeGetCitiesList msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.country
        , Encode.unsignedVarint32 18
        , Encode.string msg.lang
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetUserData :: Uint8Array
encodeGetUserData = Encode.unsignedVarint32 0

encodeConfirmRegistration :: ConfirmRegistration -> Uint8Array
encodeConfirmRegistration msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.firstName
        , Encode.unsignedVarint32 18
        , Encode.string msg.lastName
        , Encode.unsignedVarint32 26
        , Encode.string msg.phone
        , Encode.unsignedVarint32 34
        , Encode.string msg.carPlate
        , Encode.unsignedVarint32 42
        , Encode.string msg.carColor
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetAutocomplete :: GetAutocomplete -> Uint8Array
encodeGetAutocomplete msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.text
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 18, encodeLocation x ]) msg.location
        , Encode.unsignedVarint32 26
        , Encode.string msg.lang
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeLocation :: Location -> Uint8Array
encodeLocation msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 9
        , Encode.double msg.lat
        , Encode.unsignedVarint32 17
        , Encode.double msg.lng
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetDirections :: GetDirections -> Uint8Array
encodeGetDirections msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.unsignedVarint32 10, encodeWaypoint x ]) msg.waypoints
        , Encode.unsignedVarint32 16
        , Encode.bigInt msg.departure
        , Encode.unsignedVarint32 26
        , Encode.string msg.lang
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeWaypoint :: Waypoint -> Uint8Array
encodeWaypoint msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , Encode.string msg.description
        , Encode.unsignedVarint32 18
        , encodeWaypointType msg.tpe
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.unsignedVarint32 26, Encode.string x ]) msg.placeId
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeWaypointType :: WaypointType -> Uint8Array
encodeWaypointType SubwayWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 10, encodeSubwayWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType BusStationWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 18, encodeBusStationWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType AirportWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 26, encodeAirportWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType RouteWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 34, encodeRouteWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType StreetAddressWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 42, encodeStreetAddressWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType LocalityWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 50, encodeLocalityWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType ShoppingMallWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 58, encodeShoppingMallWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType PointOfInterest = do
  let xs = concatAll [ Encode.unsignedVarint32 66, encodePointOfInterest ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]
encodeWaypointType UnknownWaypoint = do
  let xs = concatAll [ Encode.unsignedVarint32 74, encodeUnknownWaypoint ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeSubwayWaypoint :: Uint8Array
encodeSubwayWaypoint = Encode.unsignedVarint32 0

encodeBusStationWaypoint :: Uint8Array
encodeBusStationWaypoint = Encode.unsignedVarint32 0

encodeAirportWaypoint :: Uint8Array
encodeAirportWaypoint = Encode.unsignedVarint32 0

encodeRouteWaypoint :: Uint8Array
encodeRouteWaypoint = Encode.unsignedVarint32 0

encodeStreetAddressWaypoint :: Uint8Array
encodeStreetAddressWaypoint = Encode.unsignedVarint32 0

encodeLocalityWaypoint :: Uint8Array
encodeLocalityWaypoint = Encode.unsignedVarint32 0

encodeShoppingMallWaypoint :: Uint8Array
encodeShoppingMallWaypoint = Encode.unsignedVarint32 0

encodePointOfInterest :: Uint8Array
encodePointOfInterest = Encode.unsignedVarint32 0

encodeUnknownWaypoint :: Uint8Array
encodeUnknownWaypoint = Encode.unsignedVarint32 0

encodeGetRevGeocoding :: GetRevGeocoding -> Uint8Array
encodeGetRevGeocoding msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 10
        , encodeLocation msg.location
        , Encode.unsignedVarint32 18
        , Encode.string msg.lang
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeAddDriverRoute :: AddDriverRoute -> Uint8Array
encodeAddDriverRoute msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.bigInt msg.date
        , Encode.unsignedVarint32 18
        , Encode.string msg.route
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]