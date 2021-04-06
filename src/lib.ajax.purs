module Lib.Ajax
  ( getEff
  , postEff
  ) where

import Prelude hiding (div)

import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(StatusCode))
import Data.ArrayBuffer.Types (Uint8Array, ArrayBuffer)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error)
import Effect.Class.Console (errorShow)

foreign import uint8Array :: ArrayBuffer -> Uint8Array

post :: String -> Uint8Array -> Aff (Either String Uint8Array)
post url d = do
  resp <- Affjax.post ResponseFormat.arrayBuffer url (Just $ RequestBody.arrayView d)
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> (Affjax.printError err)
    Right { status: StatusCode code, body: body } -> Right $ uint8Array body

postEff :: forall a b. String -> Uint8Array -> (String -> Effect a) -> (Uint8Array -> Effect b) -> Effect Unit
postEff url d failure success = runAff_ f (post url d)
  where
    f :: Either Error (Either String Uint8Array) -> Effect Unit
    f (Left e) = void $ errorShow e >>= \_ -> failure (show e)
    f (Right (Left e)) = void $ errorShow e >>= \_ -> failure (show e)
    f (Right (Right a)) = void $ success a

get :: String -> Aff (Either String String)
get url = do
  resp <- Affjax.get ResponseFormat.string url
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> (Affjax.printError err)
    Right { status: StatusCode  200, body: d } -> Right d
    Right { status: StatusCode    0, body: d } -> Right d
    Right { status: StatusCode code, body: b } -> Left $ "Request failed: " <> (show code) <> ", b:" <> b

getEff :: forall a. String -> (String -> Effect a) -> Effect Unit
getEff url success = runAff_ f (get url)
  where
    f :: Either Error (Either String String) -> Effect Unit
    f (Left e) = void $ errorShow e
    f (Right (Left e)) = void $ errorShow e
    f (Right (Right a)) = void $ success a
