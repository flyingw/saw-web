module Lib.Datepicker
  ( datepickerClass
  , datepickerLoad
  , toLocaleTimeString
  , Props
  ) where

import Prelude hiding (div, min, max)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)

import Data.JSDate (JSDate, fromTime, getTime, now, parse)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold, find)

import React (ReactClass, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (input)
import React.DOM.Props (_id, _type, value, min, max)

import Lib.React (cn, onChangeValue)

type Props =
  { onChange :: JSDate -> Effect Unit
  , lang :: String
  , showTime :: Boolean
  , className :: String
  , wrapperClassName :: String
  , _id :: String
  , keyText :: String -> String
  }

type State = { date :: JSDate }

datepickerClass :: ReactClass Props
datepickerClass = component "Datepicker" \this -> do
  today  <- now
  native <- nativeDate
  pure
    { state: { date: today }
    , render: do
        props  <- getProps this
        state  <- getState this
        if native
          then do
            vv  <- if props.showTime then isoDateTime state.date else isoDate state.date
            t1 <- isoDateTime today
            t2 <- isoDateTime $ fromTime $ getTime today + 432000000.0
            pure $ input [ _type $ if props.showTime then "datetime-local" else "date"
                         , cn $ props.className <> " " <> props.wrapperClassName
                         , _id props._id
                         , value vv
                         , onChangeValue \v -> do
                             d <- parse v 
                             _ <- modifyState this _{ date=d } 
                             props.onChange d
                         , min t1
                         , max t2
                         ]
          else do
            dclass <- datepickerExtClass
            locale <- case props.lang of
                        "uk" -> uk
                        "ru" -> ru
                        _    -> uk
            maxD   <- pure $ fromTime $ getTime today + 432000000.0
            pure $ createLeafElement dclass
              { selected: state.date
              , onChange: \d -> unsafePerformEffect $ do
                  _ <- modifyState this _{ date=d }
                  props.onChange d
              , showTimeSelect: props.showTime
              , className: props.className
              , wrapperClassName: props.wrapperClassName
              , popperClassName: "react-datepicker-popper-fix"
              , id: props._id
              , locale: locale
              , timeFormat: if props.showTime then "p" else ""
              , dateFormat: if props.showTime then "Pp" else "P"
              , minDate: today
              , maxDate: maxD
              , timeCaption: props.keyText $ "key.time"
              }
    }

foreign import formatISO_ :: EffectFn1 JSDate (Array { type :: String, value :: String })

extract :: Array { type :: String, value :: String } -> String -> String
extract xs _type = fromMaybe "" $ map _.value $ find (\x -> x.type == _type) xs

isoDate :: JSDate -> Effect String
isoDate date = do
  xs <- runEffectFn1 formatISO_ date
  let  y = extract xs "year"
  let _M = extract xs "month"
  let  d = extract xs "day"
  pure $ fold [ y, "-", _M, "-", d ]

isoDateTime :: JSDate -> Effect String
isoDateTime date = do
  ymd <- isoDate date
  xs <- runEffectFn1 formatISO_ date
  let  h = extract xs "hour"
  let  m = extract xs "minute"
  pure $ fold [ ymd, "T", h, ":", m ]

foreign import toLocaleTimeString_ :: EffectFn1 JSDate String

toLocaleTimeString :: JSDate -> Effect String
toLocaleTimeString d = runEffectFn1 toLocaleTimeString_ d

type ExternalProps =
  { selected :: JSDate
  , onChange :: JSDate -> Unit
  , showTimeSelect :: Boolean
  , id :: String
  , className :: String
  , wrapperClassName :: String
  , popperClassName :: String
  , locale :: Foreign
  , timeFormat :: String
  , dateFormat :: String
  , minDate :: JSDate
  , maxDate :: JSDate
  , timeCaption :: String
  }

foreign import nativeDate :: Effect Boolean
foreign import datepickerLoad :: Effect Unit
foreign import datepickerExtClass :: Effect (ReactClass ExternalProps)
foreign import uk :: Effect Foreign
foreign import ru :: Effect Foreign
