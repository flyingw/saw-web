module Api.Pull
  ( Pull(..)
  , LoginAttempt
  , AddDriver
  , AddRider
  , encodePull
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe, fromMaybe)
import Prelude (map, ($))
import Proto.Encode as Encode
import Proto.Uint8ArrayExt (length, concatAll, fromArray)
import Api

data Pull = Ping | LoginAttempt LoginAttempt | AddDriver AddDriver | AddRider AddRider
type LoginAttempt = { data_check_string :: String, hash :: String, auth_date :: Number, name :: Maybe String }
type AddDriver = { name :: String, phone :: String, carPlate :: String, date :: Number, lap :: Int, seats :: Int, from :: Address, to :: Address }
type AddRider = { name :: String, from :: Address, to :: Address }

encodePull :: Pull -> Uint8Array
encodePull Ping = concatAll [ Encode.uint32 10, encodePing ]
encodePull (LoginAttempt x) = concatAll [ Encode.uint32 18, encodeLoginAttempt x ]
encodePull (AddDriver x) = concatAll [ Encode.uint32 82, encodeAddDriver x ]
encodePull (AddRider x) = concatAll [ Encode.uint32 162, encodeAddRider x ]

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

encodeAddRider :: AddRider -> Uint8Array
encodeAddRider msg = do
  let xs = concatAll
        [ Encode.uint32 10
        , Encode.string msg.name
        , Encode.uint32 18
        , encodeAddress msg.from
        , Encode.uint32 26
        , encodeAddress msg.to
        ]
  concatAll [ Encode.uint32 $ length xs, xs ]