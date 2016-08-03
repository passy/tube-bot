module Bot.AffjaxHelper where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (liftAff, class MonadAff)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExceptT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, either)
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
  :: forall e a b m.
    (DecodeJson a, Requestable b, MonadAff (ajax :: AJAX | e) m, MonadError AjaxError m)
  => AffjaxRequest b
  -> m a
doJsonRequest req = do
  liftAff (affjax req) >>= getJson

doJsonRequest'
  :: forall e a b.
    (DecodeJson a, Requestable b)
  => AffjaxRequest b
  -> Aff (ajax :: AJAX | e) (Either AjaxError a)
doJsonRequest' = runExceptT <<< doJsonRequest 

getJson
  :: forall m a.
     (DecodeJson a, MonadError AjaxError m)
  => AffjaxResponse String
  -> m a
getJson resp = do
  when (resp.status /= StatusCode 200) $
    throwError $ UnexpectedHTTPStatus resp
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< DecodingError) pure result
