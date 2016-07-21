module Bot where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Generic (class Generic, gShow, gEq)
import Text.Parsing.StringParser (Parser, ParseError(..), try, runParser)
import Text.Parsing.StringParser.String as S
import Data.String as String

type SequenceNumber = Int
type MessageId = String

newtype MessagingParticipant = MessagingParticipant { id :: Int }

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

derive instance genericMessageResponse :: Generic MessageResponse

instance showMessageResponse :: Show MessageResponse where
  show = gShow

instance eqMessageResponse :: Eq MessageResponse where
  eq = gEq

commandParser :: Parser Command
commandParser =
  S.string "subscribe" *> pure (CmdSubscribe { channel: "wat", recipientId: MessagingParticipant { id: 1 } })

data Command = CmdSubscribe { channel :: String
                            , recipientId :: MessagingParticipant }

derive instance genericCommand :: Generic Command

instance showCommand :: Show Command where
  show = gShow

instance eqCommand :: Eq Command where
  eq = gEq

handleReceivedMessage :: MessagingEvent -> forall e. Eff (console :: CONSOLE | e) Unit
handleReceivedMessage (MessagingEvent e) = do
  let txt ({ message: Message { text: text } }) = text
  let res = runParser commandParser (txt e)
  logShow res
