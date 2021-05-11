module Lib.Maps(loadMap, createMap, GoogleMaps(GoogleMaps), getLocation) where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

newtype GoogleMaps = GoogleMaps Foreign

loadMap :: String -> Effect GoogleMaps
loadMap id = GoogleMaps <$> createMap id

foreign import createMap :: String -> Effect Foreign

foreign import getLocation :: (Number -> Number -> Effect Unit) -> (Effect Unit) -> Effect Unit
