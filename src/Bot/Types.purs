module Bot.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut ((~>), (:=))
import Data.Generic (class Generic, gShow, gEq)
import Network.HTTP.Affjax.Request (toRequest, class Requestable)

import Data.Argonaut as J

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

newtype User = User { id :: Int }

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

data MessageResponse = RspNoop
                     | RspText { text :: String, recipient :: User }

derive instance genericMessageResponse :: Generic MessageResponse

instance showMessageResponse :: Show MessageResponse where
  show = gShow

instance eqMessageResponse :: Eq MessageResponse where
  eq = gEq

instance encodeJsonMessageResponse :: J.EncodeJson MessageResponse where
  encodeJson RspNoop
    = J.jsonEmptyObject

  encodeJson (RspText { text: text, recipient: (User { id: id }) })
    = "recipient" := ("id" := id ~> J.jsonEmptyObject)
   ~> "message" := ("text" := text ~> J.jsonEmptyObject)
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
