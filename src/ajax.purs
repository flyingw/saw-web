module Ajax
  ( get
  , getEff
  ) where

import Prelude (Unit, show, bind, discard, map, pure, void, unit, mempty, (<<<), ($), (>>=), (/=))

import Affjax as Affjax
import Affjax.RequestBody as Req
import Affjax.ResponseFormat as Resp
import Affjax.StatusCode (StatusCode(StatusCode))
import Data.Array as Array
import Data.Either (Either(Left, Right))
import Data.FormURLEncoded (FormURLEncoded)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, runAff_)
import Effect.Exception (Error)
import Prelude hiding (div)
import Web.XHR.FormData (FormData)
import Data.Map (Map)

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
