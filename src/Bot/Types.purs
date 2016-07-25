module Bot.Types where

import Prelude
import Data.Argonaut as J
import Control.Alt ((<|>))
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut ((~>), (:=))
import Data.Generic (class Generic, gShow, gEq)
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (toRequest, class Requestable)
import Text.Parsing.StringParser (ParseError())

type SequenceNumber = Int
type MessageId = String

newtype LineStatusRow = LineStatusRow
  { name :: RouteName
  , description :: String
  , disruptions :: Array Disruption
  , level :: Int }

newtype Disruption = Disruption
  { level :: Int
  , summary :: String
  , stops :: Array String }

newtype RouteInfo = RouteInfo
  { id :: String
  , name :: RouteName }

derive instance genericRouteInfo :: Generic RouteInfo

instance showRouteInfo :: Show RouteInfo where
  show = gShow

newtype User = User { id :: String }

instance encodeJsonUser :: J.EncodeJson User where
  encodeJson (User { id }) =
    "id" := id ~> J.jsonEmptyObject

-- | The unique name for a line without the "Line" suffix, e.g. "District" or
-- "Hammersmith & City". Only for type-safty reasons.
newtype RouteName = RouteName String

derive instance genericRouteName :: Generic RouteName

instance showRouteName :: Show RouteName where
  show = gShow

instance eqRouteName :: Eq RouteName where
  eq = gEq

newtype MessengerConfig = MessengerConfig { pageAccessToken :: String }

derive instance genericUser :: Generic User

instance showUser :: Show User where
  show = gShow

newtype Message = Message
  { mid :: MessageId
  , seq :: SequenceNumber
  , text :: String
  }

derive instance genericMessage :: Generic Message

instance showMessage :: Show Message where
  show = gShow

newtype MessagingEvent = MessagingEvent
  { sender :: User
  , recipient :: User
  , timestamp :: Int
  , message :: Message
  }

derive instance genericMessagingEvent :: Generic MessagingEvent

instance showMessagingEvent :: Show MessagingEvent where
  show = gShow

data Template = TmplPlainText { text :: String }
              | TmplGenericError { err :: Error }
              | TmplParseError { err :: ParseError }
              | TmplImageText { text :: String
                              , imageUrl :: URL }

data MessageResponse = RspText { text :: String, recipient :: User }
                     | RspAttachment { attachment :: Attachment
                                     , recipient :: User }

derive instance genericMessageResponse :: Generic MessageResponse

instance showMessageResponse :: Show MessageResponse where
  show = gShow

instance eqMessageResponse :: Eq MessageResponse where
  eq = gEq

instance encodeJsonMessageResponse :: J.EncodeJson MessageResponse where
  encodeJson (RspText { text, recipient })
    = "recipient" := J.encodeJson recipient
   ~> "message" := ("text" := text ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject

  encodeJson (RspAttachment { recipient, attachment })
    = "recipient" := J.encodeJson recipient
   ~> "message" := ( "attachment" := J.encodeJson attachment
                  ~> J.jsonEmptyObject )
   ~> J.jsonEmptyObject

instance requestableMessageResponse :: Requestable MessageResponse where
  toRequest = toRequest <<< J.encodeJson

data Command = CmdSubscribe { route :: RouteName }
             | CmdUnsubscribe { route :: RouteName }

derive instance genericCommand :: Generic Command

instance showCommand :: Show Command where
  show = gShow

instance eqCommand :: Eq Command where
  eq = gEq

newtype Attachment = AttImage { url :: URL }

derive instance genericAttachment :: Generic Attachment

instance showAttachment :: Show Attachment where
  show = gShow

instance eqAttachment :: Eq Attachment where
  eq = gEq

instance encodeJsonAttachment :: J.EncodeJson Attachment where
  encodeJson (AttImage { url })
    = "type" := "image"
   ~> "payload" := ("url" := url ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject
