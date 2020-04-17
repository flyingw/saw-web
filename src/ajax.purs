module Ajax
  ( get
  , getEff
  ) where

import Prelude hiding (div)

import Affjax as Affjax
import Affjax.ResponseFormat as Resp
import Affjax.StatusCode (StatusCode(StatusCode))
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (Error)

get :: String -> Aff (Either String String)
get url = do 
  resp <- Affjax.request (Affjax.defaultRequest { url = url, responseFormat = Resp.string })
  pure $ case resp of
    Left err -> Left $ "Request failed: " <> (Affjax.printError err)
    Right { status: StatusCode 200, body: d } -> Right d
    Right { status: StatusCode code, body: b } -> Left $ "Request failed: " <> (show code) <> ", b:" <> b

getEff :: forall a b. String -> (String -> Effect a) -> (String -> Effect b) -> Effect Unit
getEff url failure success = runAff_ f (get url)
  where
    f :: Either Error (Either String String) -> Effect Unit
    f (Left e) = void $ failure (show e)
    f (Right (Left e)) = void $ failure (show e)
    f (Right (Right a)) = void $ success a
