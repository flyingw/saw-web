module Keys where

import Prelude hiding (div)

import Api (PassengerType(..))

keyPassengerType :: PassengerType -> String
keyPassengerType Medical = "key.medical"
keyPassengerType Police = "key.police"
keyPassengerType Firefighter = "key.firefighter"
keyPassengerType Army = "key.army"
keyPassengerType Farmacy = "key.farmacy"
keyPassengerType Cashier = "key.cashier"
keyPassengerType Regular = "key.regular"
