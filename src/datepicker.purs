module Datepicker
  ( datepickerClass
  , datepickerLoad
  , toLocaleTimeString
  , Props
  ) where

import Prelude hiding (div)

import Data.Int (floor)
import Data.JSDate (JSDate, getFullYear, getMonth, getDate, getHours, getMinutes, now, parse, fromTime, getTime)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React (ReactClass, component, createLeafElement, getProps, getState, modifyState)
import React.DOM (input)
import React.DOM.Props (_id, _type, value, min, max)
import Foreign (Foreign)

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
  native <- isMobileOrChrome
  pure
    { state: { date: today }
    , render: do
        props  <- getProps this
        state  <- getState this
        if native
          then do
            vv  <- if props.showTime then formatISO state.date else formatDateISO state.date
            t1 <- formatISO today
            t2 <- formatISO $ fromTime $ getTime today + 432000000.0
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
            pure $ createLeafElement dclass { selected: state.date
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

formatNum :: Number -> String
formatNum n = do
  let v = floor n
  if v < 10 then "0" <> (show v) else (show v)

formatISO :: JSDate -> Effect String
formatISO date = do
  yyyy <- formatNum <$> getFullYear date
  mM   <- formatNum <$> (_ + 1.0) <$> (getMonth date)
  dd   <- formatNum <$> getDate date
  hh   <- formatNum <$> getHours date
  mm   <- formatNum <$> getMinutes date
  pure $ yyyy <> "-" <> mM <> "-" <> dd <> "T" <> hh <> ":" <> mm <> ":00"

formatDateISO :: JSDate -> Effect String
formatDateISO date = do
  yyyy <- formatNum <$> getFullYear date
  mM   <- formatNum <$> (_ + 1.0) <$> (getMonth date)
  dd   <- formatNum <$> getDate date
  pure $ yyyy <> "-" <> mM <> "-" <> dd
  
foreign import toLocaleDateString :: JSDate -> Effect String
foreign import toLocaleTimeString :: JSDate -> Effect String

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

foreign import isMobileOrChrome :: Effect Boolean
foreign import datepickerLoad :: Effect Unit
foreign import datepickerExtClass :: Effect (ReactClass ExternalProps)
foreign import uk :: Effect Foreign
foreign import ru :: Effect Foreign
