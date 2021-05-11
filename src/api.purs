module Api
  ( PassengerType(..)
  , Coordinates 
  ) where

import Data.Eq (class Eq)

data PassengerType = Medical | Police | Firefighter | Army | Farmacy | Cashier | Regular
derive instance eqPassengerType :: Eq PassengerType
type Coordinates = { lat :: Number, lng :: Number }