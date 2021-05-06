module Lib.Maps(loadMap, createMap, GoogleMaps(GoogleMaps)) where

import Effect (Effect)
import Foreign (Foreign)
import Prelude ((<$>))

newtype GoogleMaps = GoogleMaps Foreign

loadMap :: String -> Effect GoogleMaps
loadMap id = GoogleMaps <$> createMap id

foreign import createMap :: String -> Effect Foreign
