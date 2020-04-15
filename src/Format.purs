module Format
  ( todayISO
  , formatISO
  , todayDateISO
  , formatDateISO
  , formatDate
  , formatTime
  ) where

import Data.Int (floor)
import Data.JSDate (JSDate, getFullYear, getMonth, getDate, getHours, getMinutes, getSeconds, now)
import Effect (Effect)
import Prelude hiding (div)

formatNum :: Number -> String
formatNum n = do
  let v = floor n
  if v < 10 then "0" <> (show v) else (show v)

todayISO :: Effect String 
todayISO = now >>= formatISO

formatISO :: JSDate -> Effect String
formatISO date = do
  yyyy <- formatNum <$> getFullYear date
  mM   <- formatNum <$> (_ + 1.0) <$> (getMonth date)
  dd   <- formatNum <$> getDate date
  hh   <- formatNum <$> getHours date
  mm   <- formatNum <$> getMinutes date
  ss   <- formatNum <$> getSeconds date
  pure $ yyyy <> "-" <> mM <> "-" <> dd <> "T" <> hh <> ":" <> mm <> ":" <> ss

todayDateISO :: Effect String 
todayDateISO = now >>= formatDateISO

formatDateISO :: JSDate -> Effect String
formatDateISO date = do
  yyyy <- formatNum <$> getFullYear date
  mM   <- formatNum <$> (_ + 1.0) <$> (getMonth date)
  dd   <- formatNum <$> getDate date
  pure $ yyyy <> "-" <> mM <> "-" <> dd

formatDate :: JSDate -> Effect String
formatDate = toLocaleDateString

formatTime :: JSDate -> Effect String
formatTime = toLocaleTimeString

foreign import toLocaleDateString :: JSDate -> Effect String
foreign import toLocaleTimeString :: JSDate -> Effect String
