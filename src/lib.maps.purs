module Lib.Maps
  ( GoogleMap
  , GooglePolyline
  , createMap
  , setCenter
  , getLocation
  , decodeGoogle
  , createPolyline
  , attachPolyline
  , detachPolyline
  ) where

import Prelude

import Data.Array (snoc)
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Int.Bits ((.|.), (.&.), shl, shr, complement)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Foreign (Foreign)

import Api (Location)

newtype GoogleMap      = GoogleMap Foreign
newtype GooglePolyline = GooglePolyline Foreign

type MapSettings =
  { center :: Location
  , zoom :: Int
  }

createMap :: String -> MapSettings -> Effect GoogleMap
createMap id settings = GoogleMap <$> createMapImpl id settings

setCenter :: GoogleMap -> Location -> Effect Unit
setCenter (GoogleMap m) location = setCenterImpl m location

foreign import createMapImpl :: String -> MapSettings -> Effect Foreign
foreign import setCenterImpl :: Foreign -> Location -> Effect Unit

foreign import getLocation :: (Number -> Number -> Effect Unit) -> (Effect Unit) -> Effect Unit

type PolylineSettings =
  { path :: Array Location 
  , strokeColor :: String -- #FF0000
  , strokeOpacity :: Number --max 1.0
  , strokeWeight :: Int
  }

createPolyline :: PolylineSettings -> Effect GooglePolyline
createPolyline settings = GooglePolyline <$> createPolylineImpl settings

attachPolyline :: GooglePolyline -> GoogleMap -> Effect Unit
attachPolyline (GooglePolyline v) (GoogleMap m) = attachPolylineImpl v m

detachPolyline :: GooglePolyline -> Effect Unit
detachPolyline (GooglePolyline v) = detachPolylineImpl v

foreign import createPolylineImpl :: PolylineSettings -> Effect Foreign
foreign import attachPolylineImpl :: Foreign -> Foreign -> Effect Unit
foreign import detachPolylineImpl :: Foreign -> Effect Unit

decodeGoogle :: String -> Array Location
decodeGoogle = decode googleFactor

type PolylineFactor = Number
googleFactor = 1e5 :: PolylineFactor
osmFactor    = 1e6 :: PolylineFactor

decode :: PolylineFactor -> String -> Array Location
decode factor string = do
  let init = 
        { lat: Nothing :: Maybe Number
        , res: [] :: Array Location
        }
  let foldRes =
        foldl (\acc loc -> case acc.lat of
          Just v  -> do
            acc{ lat = Nothing, res = snoc acc.res { lat: v, lng: loc } }
          Nothing ->
            acc{ lat = Just loc }
        )
        init
        (decodeFlatList factor $ toCharArray string )
  foldRes.res

decodeFlatList :: PolylineFactor -> Array Char -> Array Number
decodeFlatList factor bytes = do
  let init =
        { shift: 0 :: Int
        , curr: 0 :: Int
        , v1: 0 :: Int
        , v2: 0 :: Int
        , res: [] :: Array Number
        }
  let foldRes = 
        foldl (\acc c -> do
          let b = toCharCode c - 63
          let v = acc.curr .|. (shl (b .&. 0x1F) acc.shift)
          if b >= 0x20 then
            acc{ shift = acc.shift + 5, curr = v }
          else do
            let fullCurr = acc.v1 + if (v .&. 1) == 1 then complement (shr v 1) else shr v 1
            acc{ shift = 0, curr = 0, v1 = acc.v2, v2 = fullCurr, res = snoc acc.res (toNumber fullCurr / factor) }
        ) init bytes
  foldRes.res
