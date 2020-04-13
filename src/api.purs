module Api
  ( PassengerType(..) 
  ) where

import Data.Eq (class Eq)

data PassengerType = Medical | Police | Firefighter | Army | Farmacy | Cashier | Regular
derive instance eqPassengerType :: Eq PassengerType