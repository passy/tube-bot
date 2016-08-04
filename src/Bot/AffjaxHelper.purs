module Bot.AffjaxHelper where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))

data AjaxError = UnexpectedHTTPStatus (AffjaxResponse String)
               | DecodingError String

instance showAjaxError :: Show AjaxError where
  show (UnexpectedHTTPStatus resp) =
    "AJAX request failed with code: " <> show resp.status
  show (DecodingError str) =
    "Decoding failed: " <> str

doJsonRequest
  :: forall e a b.
    (DecodeJson a, Requestable b)
  => AffjaxRequest b
  -> Aff (ajax :: AJAX | e) a
doJsonRequest req = do
  liftAff (affjax req) >>= getJson

getJson
  :: forall e a.
     DecodeJson a
  => AffjaxResponse String
  -> Aff e a
getJson resp = do
  when (resp.status /= StatusCode 200) $
    throwError <<< error <<< show $ UnexpectedHTTPStatus resp
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< error <<< show <<< DecodingError) pure result
