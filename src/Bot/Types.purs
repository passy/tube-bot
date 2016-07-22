module Bot.Types where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut ((~>), (:=))
import Data.Generic (class Generic, gShow, gEq)
import Network.HTTP.Affjax.Request (toRequest, class Requestable)

import Data.Argonaut as J

type SequenceNumber = Int
type MessageId = String

newtype MessagingParticipant = MessagingParticipant { id :: Int }

newtype MessengerConfig = MessengerConfig { pageAccessToken :: String }

derive instance genericMessagingParticipant :: Generic MessagingParticipant

instance showMessagingParticipant :: Show MessagingParticipant where
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
  { sender :: MessagingParticipant
  , recipient :: MessagingParticipant
  , timestamp :: Int
  , message :: Message
  }

derive instance genericMessagingEvent :: Generic MessagingEvent

instance showMessagingEvent :: Show MessagingEvent where
  show = gShow

data MessageResponse = RspNoop
                     | RspText { text :: String, recipient :: MessagingParticipant }

derive instance genericMessageResponse :: Generic MessageResponse

instance showMessageResponse :: Show MessageResponse where
  show = gShow

instance eqMessageResponse :: Eq MessageResponse where
  eq = gEq

instance encodeJsonMessageResponse :: J.EncodeJson MessageResponse where
  encodeJson RspNoop
    = J.jsonEmptyObject

  encodeJson (RspText { text: text, recipient: (MessagingParticipant { id: id }) })
    = "recipient" := ("id" := id ~> J.jsonEmptyObject)
   ~> "message" := ("text" := text ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject

instance requestableMessageResponse :: Requestable MessageResponse where
  toRequest = toRequest <<< J.encodeJson

data Command = CmdSubscribe { channel :: String }
             | CmdUnsubscribe { channel :: String }

derive instance genericCommand :: Generic Command

instance showCommand :: Show Command where
  show = gShow

instance eqCommand :: Eq Command where
  eq = gEq
