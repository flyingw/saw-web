module Lib.React where

import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Prelude hiding (div)
import React.DOM.Props (Props, className)
import React.SyntheticEvent (SyntheticInputEvent)
import Unsafe.Coerce (unsafeCoerce)
import React.DOM.Props (onChange)

cn :: String -> Props
cn = className

targetValue :: SyntheticInputEvent -> String
targetValue e = (unsafeCoerce e).target.value

onChangeValue :: (String -> Effect Unit) -> Props
onChangeValue f = onChange \e -> f $ (unsafeCoerce e).target.value

onChangeValueInt :: (Int -> Effect Unit) -> Props
onChangeValueInt f = onChange \e -> f $ fromMaybe 0 $ fromString $ (unsafeCoerce e).target.value
