module Api.Pull
  ( Pull(..)
  , TelegramLogin
  , defaultTelegramLogin
  , TelegramData(..)
  , TelegramString
  , TelegramNum
  , AddDriver
  , defaultAddDriver
  , Address
  , AddPassenger
  , GetFreeDrivers
  , GetFreePassengers
  , GetCitiesList
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.Eq (class Eq)
import Prelude (($))
import Proto.Encode as Encode
import Proto.Uint8Array (Uint8Array, length, concatAll)
import Api

data Pull = Ping | TelegramLogin TelegramLogin | AddDriver AddDriver | AddPassenger AddPassenger | GetFreeDrivers GetFreeDrivers | GetFreePassengers GetFreePassengers | GetCitiesList GetCitiesList
derive instance eqPull :: Eq Pull
type TelegramLogin = { d :: Array TelegramData }
defaultTelegramLogin :: { d :: Array TelegramData }
defaultTelegramLogin = { d: [] }
data TelegramData = TelegramString TelegramString | TelegramNum TelegramNum
derive instance eqTelegramData :: Eq TelegramData
type TelegramString = { key :: String, value :: String }
type TelegramNum = { key :: String, value :: Number }
type AddDriver = { firstName :: String, lastName :: String, phone :: String, carPlate :: String, date :: Number, deviationDistance :: Int, deviationTime :: Int, seats :: Int, from :: Address, to :: Address, types :: Array PassengerType, lang :: String }
defaultAddDriver :: { types :: Array PassengerType }
defaultAddDriver = { types: [] }
type Address = { country :: String, city :: String, street :: String, building :: String }
type AddPassenger = { firstName :: String, lastName :: String, phone :: String, date :: Number, tpe :: PassengerType, from :: Address, to :: Address }
type GetFreeDrivers = { date :: Number }
type GetFreePassengers = { date :: Number }
type GetCitiesList = { country :: String, lang :: String }

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.unsignedVarint32 10, encodePing ]
encodePull (TelegramLogin x) = concatAll [ Encode.unsignedVarint32 26, encodeTelegramLogin x ]
encodePull (AddDriver x) = concatAll [ Encode.unsignedVarint32 82, encodeAddDriver x ]
encodePull (AddPassenger x) = concatAll [ Encode.unsignedVarint32 162, encodeAddPassenger x ]
encodePull (GetFreeDrivers x) = concatAll [ Encode.unsignedVarint32 242, encodeGetFreeDrivers x ]
encodePull (GetFreePassengers x) = concatAll [ Encode.unsignedVarint32 322, encodeGetFreePassengers x ]
encodePull (GetCitiesList x) = concatAll [ Encode.unsignedVarint32 402, encodeGetCitiesList x ]

encodePing :: Uint8Array
encodePing = Encode.unsignedVarint32 0

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
        , Encode.signedVarint64 msg.date
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
        , Encode.signedVarint64 msg.date
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
        , Encode.signedVarint64 msg.date
        ]
  concatAll [ Encode.unsignedVarint32 $ length xs, xs ]

encodeGetFreePassengers :: GetFreePassengers -> Uint8Array
encodeGetFreePassengers msg = do
  let xs = concatAll
        [ Encode.unsignedVarint32 8
        , Encode.signedVarint64 msg.date
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