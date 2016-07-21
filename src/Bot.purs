module Bot where

import Prelude
import Data.String as String
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Generic (class Generic, gShow, gEq)
import Data.List (List, toUnfoldable)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (skipSpaces)

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

listToString :: List Char -> String
listToString = String.fromCharArray <<< toUnfoldable

commandParser :: MessagingParticipant -> Parser Command
commandParser rid = do
  void $ StringParser.string "subscribe"
  skipSpaces
  channel <- listToString <$> Parser.many1 StringParser.anyLetter
  pure $ CmdSubscribe { channel: channel, recipientId: rid }

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
  let res = runParser (commandParser e.sender) (txt e)
  logShow res
