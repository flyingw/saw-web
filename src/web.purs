module Web where

import Data.Maybe
import Effect (Effect)
import Effect.Exception (throw)
import Prelude hiding (div, min, max)
import React
import React.DOM
import ReactDOM
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Props = {}
type State = {}

view :: Effect Unit
view = do
  container <- byId "container"
  let element = createLeafElement webClass {}
  void $ render element container

webClass :: ReactClass {}
webClass = component "web" \this -> do
  let state = {}
  let render = render' this
  pure
    { state
    , render
    }
  where
  render' :: ReactThis Props State -> Effect ReactElement
  render' this = do
    pure $
      div [] [ text "app" ]

byId :: String -> Effect Element
byId id = do
  doc <- window >>= document
  let d = toNonElementParentNode doc
  e :: Maybe Element <- getElementById id d
  case e of
    Just e' -> pure e'
    Nothing -> throw $ "not found="<>id
