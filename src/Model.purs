module Model
  ( UserInfo
  ) where

import Data.Maybe (Maybe(..))

type UserInfo =
  { username :: String
  , firstName :: Maybe String
  , lastName :: Maybe String
  , photo :: Maybe String
  }
