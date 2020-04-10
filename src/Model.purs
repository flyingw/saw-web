module Model
  ( PassengerType(..)
  ) where

import Prelude hiding (div)

data PassengerType = Medical | Police | Firefighter | Army | Farmacy | Cashier | Regular
instance showPassengerType :: Show PassengerType where
  show :: PassengerType -> String
  show Medical = "key.medical"
  show Police = "key.police"
  show Firefighter = "key.firefighter"
  show Army = "key.army"
  show Farmacy = "key.farmacy"
  show Cashier = "key.cashier"
  show Regular = "key.regular"
derive instance eqPassengerType :: Eq PassengerType
derive instance ordPassengerType :: Ord PassengerType
