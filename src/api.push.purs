module Api.Push
  ( Push(..)
  , AddRouteOk
  , decodePush
  ) where

import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM3)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(Left))
import Data.Int.Bits (zshr, (.&.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Prelude (map, bind, pure, ($), (+), (<))
import Proto.Decode as Decode
import Api.Common

decodeFieldLoop :: forall a b c. Int -> Decode.Result a -> (a -> b) -> Decode.Result' (Step { a :: Int, b :: b, c :: Int } { pos :: Int, val :: c })
decodeFieldLoop end res f = map (\{ pos, val } -> Loop { a: end, b: f val, c: pos }) res

data Push = Pong | AddRouteOk AddRouteOk
type AddRouteOk = { n :: String, from :: Address }
type AddRouteOk' = { n :: Maybe String, from :: Maybe Address }
type Address' = { city :: Maybe String, street :: Maybe String, building :: Maybe String }

decodePush :: Uint8Array -> Decode.Result Push
decodePush _xs_ = do
  { pos: pos1, val: tag } <- Decode.uint32 _xs_ 0
  case tag `zshr` 3 of
    1 -> decode (decodePong _xs_ pos1) \_ -> Pong
    10 -> decode (decodeAddRouteOk _xs_ pos1) AddRouteOk
    i -> Left $ Decode.BadType i
  where
  decode :: forall a. Decode.Result a -> (a -> Push) -> Decode.Result Push
  decode res f = map (\{ pos, val } -> { pos, val: f val }) res

decodePong :: Uint8Array -> Int -> Decode.Result Unit
decodePong _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  pure { pos: pos + msglen, val: unit }

decodeAddRouteOk :: Uint8Array -> Int -> Decode.Result AddRouteOk
decodeAddRouteOk _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { n: Nothing, from: Nothing } pos
  case val of
    { n: Just n, from: Just from } -> pure { pos: pos1, val: { n, from } }
    _ -> Left $ Decode.MissingFields "AddRouteOk"
    where
    decode :: Int -> AddRouteOk' -> Int -> Decode.Result' (Step { a :: Int, b :: AddRouteOk', c :: Int } { pos :: Int, val :: AddRouteOk' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { n = Just val }
        2 -> decodeFieldLoop end (decodeAddress _xs_ pos2) \val -> acc { from = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }

decodeAddress :: Uint8Array -> Int -> Decode.Result Address
decodeAddress _xs_ pos0 = do
  { pos, val: msglen } <- Decode.uint32 _xs_ pos0
  { pos: pos1, val } <- tailRecM3 decode (pos + msglen) { city: Nothing, street: Nothing, building: Nothing } pos
  case val of
    { city: Just city, street: Just street, building: Just building } -> pure { pos: pos1, val: { city, street, building } }
    _ -> Left $ Decode.MissingFields "Address"
    where
    decode :: Int -> Address' -> Int -> Decode.Result' (Step { a :: Int, b :: Address', c :: Int } { pos :: Int, val :: Address' })
    decode end acc pos1 | pos1 < end = do
      { pos: pos2, val: tag } <- Decode.uint32 _xs_ pos1
      case tag `zshr` 3 of
        1 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { city = Just val }
        2 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { street = Just val }
        3 -> decodeFieldLoop end (Decode.string _xs_ pos2) \val -> acc { building = Just val }
        _ -> decodeFieldLoop end (Decode.skipType _xs_ pos2 $ tag .&. 7) \_ -> acc
    decode end acc pos1 = pure $ Done { pos: pos1, val: acc }