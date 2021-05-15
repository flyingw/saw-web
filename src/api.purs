module Api
  ( PassengerType(..)
  , Location
  , Waypoint
  , defaultWaypoint
  , WaypointType(..) 
  ) where

import Data.Eq (class Eq)
import Data.Maybe (Maybe(Nothing))

data PassengerType = Medical | Police | Firefighter | Army | Farmacy | Cashier | Regular
derive instance eqPassengerType :: Eq PassengerType
type Location = { lat :: Number, lng :: Number }
type Waypoint = { description :: String, tpe :: WaypointType, placeId :: Maybe String }
defaultWaypoint :: { placeId :: Maybe String }
defaultWaypoint = { placeId: Nothing }
data WaypointType = SubwayWaypoint | BusStationWaypoint | AirportWaypoint | RouteWaypoint | StreetAddressWaypoint | LocalityWaypoint | ShoppingMallWaypoint | PointOfInterest | UnknownWaypoint
derive instance eqWaypointType :: Eq WaypointType