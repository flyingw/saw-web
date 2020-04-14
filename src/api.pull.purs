module Api.Pull
  ( Pull(..)
  , LoginAttempt
  , TelegramLogin
  , TelegramData(..)
  , TelegramString
  , TelegramNum
  , AddDriver
  , Address
  , AddPassenger
  , GetFreeDrivers
  , GetFreePassengers
  , encodePull
  ) where

import Data.Array (concatMap)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Eq (class Eq)
import Data.Maybe (Maybe, fromMaybe)
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import Api

data Pull = Ping | LoginAttempt LoginAttempt | TelegramLogin TelegramLogin | AddDriver AddDriver | AddPassenger AddPassenger | GetFreeDrivers GetFreeDrivers | GetFreePassengers GetFreePassengers
type LoginAttempt = { data_check_string :: String, hash :: String, auth_date :: Number, name :: Maybe String, id :: String }
type TelegramLogin = { d :: Array TelegramData }
data TelegramData = TelegramString TelegramString | TelegramNum TelegramNum
derive instance eqTelegramData :: Eq TelegramData
type TelegramString = { key :: String, value :: String }
type TelegramNum = { key :: String, value :: Number }
type AddDriver = { name :: String, phone :: String, carPlate :: String, date :: Number, lap :: Int, seats :: Int, from :: Address, to :: Address, types :: Array PassengerType }
type Address = { city :: String, street :: String, building :: String }
type AddPassenger = { name :: String, phone :: String, date :: Number, tpe :: PassengerType, from :: Address, to :: Address }
type GetFreeDrivers = { date :: Number }
type GetFreePassengers = { date :: Number }

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.uint32 10, encodePing ]
encodePull (LoginAttempt x) = concatAll [ Encode.uint32 18, encodeLoginAttempt x ]
encodePull (TelegramLogin x) = concatAll [ Encode.uint32 26, encodeTelegramLogin x ]
encodePull (AddDriver x) = concatAll [ Encode.uint32 82, encodeAddDriver x ]
encodePull (AddPassenger x) = concatAll [ Encode.uint32 162, encodeAddPassenger x ]
encodePull (GetFreeDrivers x) = concatAll [ Encode.uint32 242, encodeGetFreeDrivers x ]
encodePull (GetFreePassengers x) = concatAll [ Encode.uint32 322, encodeGetFreePassengers x ]

encodePing :: Uint8Array
encodePing = Encode.uint32 0

encodeLoginAttempt :: LoginAttempt -> Uint8Array
encodeLoginAttempt msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.data_check_string
        , Encode.uint32 18
        , Encode.string msg.hash
        , Encode.uint32 25
        , Encode.double msg.auth_date
        , fromMaybe (fromArray []) $ map (\x -> concatAll [ Encode.uint32 34, Encode.string x ]) msg.name
        , Encode.uint32 42
        , Encode.string msg.id
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeTelegramLogin :: TelegramLogin -> Uint8Array
encodeTelegramLogin msg = do
  let xs = concatAll
        [ concatAll $ concatMap (\x -> [ Encode.uint32 10, encodeTelegramData x ]) msg.d
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeTelegramData :: TelegramData -> Uint8Array
encodeTelegramData (TelegramString x) = do
  let xs = concatAll [ Encode.uint32 10, encodeTelegramString x ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodeTelegramData (TelegramNum x) = do
  let xs = concatAll [ Encode.uint32 18, encodeTelegramNum x ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeTelegramString :: TelegramString -> Uint8Array
encodeTelegramString msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.key
        , Encode.uint32 18
        , Encode.string msg.value
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeTelegramNum :: TelegramNum -> Uint8Array
encodeTelegramNum msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.key
        , Encode.uint32 17
        , Encode.double msg.value
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeAddDriver :: AddDriver -> Uint8Array
encodeAddDriver msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.name
        , Encode.uint32 18
        , Encode.string msg.phone
        , Encode.uint32 26
        , Encode.string msg.carPlate
        , Encode.uint32 33
        , Encode.double msg.date
        , Encode.uint32 40
        , Encode.uint32 msg.lap
        , Encode.uint32 48
        , Encode.uint32 msg.seats
        , Encode.uint32 58
        , encodeAddress msg.from
        , Encode.uint32 66
        , encodeAddress msg.to
        , concatAll $ concatMap (\x -> [ Encode.uint32 74, encodePassengerType x ]) msg.types
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeAddress :: Address -> Uint8Array
encodeAddress msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.city
        , Encode.uint32 18
        , Encode.string msg.street
        , Encode.uint32 26
        , Encode.string msg.building
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodePassengerType :: PassengerType -> Uint8Array
encodePassengerType Medical = do
  let xs = concatAll [ Encode.uint32 10, encodeMedical ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Police = do
  let xs = concatAll [ Encode.uint32 18, encodePolice ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Firefighter = do
  let xs = concatAll [ Encode.uint32 26, encodeFirefighter ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Army = do
  let xs = concatAll [ Encode.uint32 34, encodeArmy ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Farmacy = do
  let xs = concatAll [ Encode.uint32 42, encodeFarmacy ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Cashier = do
  let xs = concatAll [ Encode.uint32 50, encodeCashier ]
  concatAll [ Encode.uint32 $ length xs, xs ]
encodePassengerType Regular = do
  let xs = concatAll [ Encode.uint32 58, encodeRegular ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeMedical :: Uint8Array
encodeMedical = Encode.uint32 0

encodePolice :: Uint8Array
encodePolice = Encode.uint32 0

encodeFirefighter :: Uint8Array
encodeFirefighter = Encode.uint32 0

encodeArmy :: Uint8Array
encodeArmy = Encode.uint32 0

encodeFarmacy :: Uint8Array
encodeFarmacy = Encode.uint32 0

encodeCashier :: Uint8Array
encodeCashier = Encode.uint32 0

encodeRegular :: Uint8Array
encodeRegular = Encode.uint32 0

encodeAddPassenger :: AddPassenger -> Uint8Array
encodeAddPassenger msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.name
        , Encode.uint32 18
        , Encode.string msg.phone
        , Encode.uint32 25
        , Encode.double msg.date
        , Encode.uint32 34
        , encodePassengerType msg.tpe
        , Encode.uint32 42
        , encodeAddress msg.from
        , Encode.uint32 50
        , encodeAddress msg.to
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeGetFreeDrivers :: GetFreeDrivers -> Uint8Array
encodeGetFreeDrivers msg = do
  let xs = concatAll
        [ Encode.uint32 9
        , Encode.double msg.date
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]

encodeGetFreePassengers :: GetFreePassengers -> Uint8Array
encodeGetFreePassengers msg = do
  let xs = concatAll
        [ Encode.uint32 9
        , Encode.double msg.date
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]