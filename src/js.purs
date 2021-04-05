module Lib.JS where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

foreign import _encodeURI :: forall a. Fn3 (String -> a) (String -> a) String a

-- | URI encoding. Returns `Nothing` when given a value with unencodeable
-- | characters.
encodeURI :: String -> Maybe String
encodeURI s = runFn3 _encodeURI (const Nothing) Just s
