module Lib.WebSocket
  ( create
  , setBinary
  , onOpen
  , onClose
  , onError
  , onMsg
  , send
  , close
  , module Web.Socket.WebSocket
  , readArrayBuffer
  , Ws, new, sub, snd, reconnect, OnMsgF, Unsub
  ) where

import Control.Monad.Except (runExcept)
import Data.Array (singleton, fromFoldable, cons, filter, head)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), fst)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Foreign (F, Foreign, renderForeignError, unsafeReadTagged)
import Prelude hiding (div)
import Web.Event.Event (Event)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window) as DOM
import Web.HTML.Location (protocol) as DOM
import Web.HTML.Window (location) as DOM
import Web.Socket.BinaryType (BinaryType(ArrayBuffer))
import Web.Socket.Event.EventTypes (onOpen, onMessage, onError, onClose) as WS
import Web.Socket.Event.MessageEvent (MessageEvent, fromEvent, data_)
import Web.Socket.ReadyState (ReadyState(Connecting, Open))
import Web.Socket.WebSocket (create, sendArrayBufferView, toEventTarget, setBinaryType, close) as WS
import Web.Socket.WebSocket (WebSocket, readyState)
import Proto.Uint8Array (Uint8Array, unwrap)

import Api.Pull (Pull, encodePull)
import Api.Push (Push, decodePush)

type OnMsgF = Maybe Push -> Effect Unit
type Unsub = Effect Unit

data Ws = Ws (Ref WsState)
type WsState =
  { con :: WebSocket
  , subs :: Array (Tuple Int OnMsgF)
  , q :: Array Pull
  , url :: String
  , await :: Boolean
  }

new :: String -> Effect Ws
new url = do
  con <- create url
  st  <- Ref.new { con: con, subs: [], q: [], url: url, await: true }
  ws  <- pure $ Ws st
  _   <- initWs ws
  pure ws

reconnect :: Ws -> Effect Unit
reconnect (Ws ref) = do
  st  <- Ref.read ref
  con <- create st.url
  _   <- Ref.modify_ _{ con = con, await = true } ref
  initWs $ Ws ref

initWs :: Ws -> Effect Unit
initWs (Ws ref) = do
  st <- Ref.read ref
  _  <- onOpen st.con \_ -> do
          curr <- Ref.read ref
          _    <- setBinary curr.con
          _    <- sequence $ curr.q <#> \msg -> send curr.con $ encodePull msg
          Ref.modify_ _{ q = [], await = false } ref
  _  <- onError st.con \e -> do
          curr <- Ref.read ref
          _    <- sequence $ curr.subs <#> \(Tuple _ f) -> f Nothing
          _    <- Ref.modify_ _{ await = true } ref
          setTimeout 3000 $ reconnect $ Ws ref
  _  <- onClose st.con \_ -> Ref.modify_ _{ await = false } ref
  _  <- onMsg st.con (\d -> case decodePush d of
          Left y -> error $ show y
          Right { val: msg } -> do
            curr <- Ref.read ref
            void $ sequence $ curr.subs <#> \(Tuple _ f) -> f $ Just msg
        ) (sequence <<< map error)
  pure unit

snd :: Ws -> Pull -> Effect Unit
snd (Ws ref) msg = do
  st <- Ref.read ref
  rs <- readyState st.con
  case rs of
    Open       -> send st.con $ encodePull msg
    Connecting -> Ref.modify_ _{ q = cons msg st.q } ref
    _          -> do
                    Ref.modify_ _{ q = cons msg st.q } ref
                    if st.await then pure unit else reconnect $ Ws ref

sub :: Ws -> OnMsgF -> Effect Unsub
sub (Ws ref) f = do
  st   <- Ref.read ref
  n    <- pure $ (+) 1 $ fromMaybe 0 $ head st.subs <#> fst
  _    <- Ref.modify_ _{ subs = cons (Tuple n f) st.subs } ref
  pure $ Ref.modify_  (\s -> s{ subs = filter (\(Tuple nn _) -> nn == n) s.subs }) ref

create :: String -> Effect WebSocket
create path = do
  location <- DOM.window >>= DOM.location
  protocol <- DOM.protocol location
  let protocol' = if protocol == "https:" then "wss:" else "ws:"
  let url = protocol' <> "//" <> path
  WS.create url []

setBinary :: WebSocket -> Effect Unit
setBinary ws = WS.setBinaryType ws ArrayBuffer

onOpen :: forall a. WebSocket -> (Event -> Effect a) -> Effect Unit
onOpen ws handler =
  let
    useCapture = false
    target = WS.toEventTarget ws
  in do
    l <- eventListener handler
    addEventListener WS.onOpen l useCapture target

onError :: forall a. WebSocket -> (Event -> Effect a) -> Effect Unit
onError ws handler =
  let
    useCapture = false
    target = WS.toEventTarget ws
  in do
    l <- eventListener handler
    addEventListener WS.onError l useCapture target

onClose :: forall a. WebSocket -> (Event -> Effect a) -> Effect Unit
onClose ws handler =
  let
    useCapture = false
    target = WS.toEventTarget ws
  in do
    l <- eventListener handler
    addEventListener WS.onClose l useCapture target

close :: WebSocket -> Effect Unit
close = WS.close

send :: WebSocket -> Uint8Array -> Effect Unit
send ws = WS.sendArrayBufferView ws <<< unwrap

onMsg :: forall e. WebSocket -> (Uint8Array -> Effect Unit) -> (Array String -> Effect e) -> Effect Unit
onMsg ws success failure = onMsg' readArrayBuffer ws (success <<< uint8Array) failure

onMsg' :: forall e a. (Foreign -> F a) -> WebSocket -> (a -> Effect Unit) -> (Array String -> Effect e) -> Effect Unit
onMsg' f ws success failure =
  let
    useCapture = false
    target = WS.toEventTarget ws
  in do
    l <- eventListener \x -> either (void <<< failure) success $ parseEvent x
    addEventListener WS.onMessage l useCapture target
  where
  parseEvent :: Event -> Either (Array String) a
  parseEvent ev = (readMessageEvent ev) >>= parseMessageEvent f

parseMessageEvent :: forall a. (Foreign -> F a) -> MessageEvent -> Either (Array String) a
parseMessageEvent f = lmap (map renderForeignError <<< fromFoldable <<< toList) <<< runExcept <<< f <<< data_

readMessageEvent :: Event -> Either (Array String) MessageEvent
readMessageEvent = maybe (Left $ singleton "Can't get event") Right <<< fromEvent

readArrayBuffer :: Foreign -> F ArrayBuffer
readArrayBuffer = unsafeReadTagged "ArrayBuffer"

foreign import uint8Array :: ArrayBuffer -> Uint8Array
