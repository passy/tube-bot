module Bot where

import Prelude
import Bot.Types as Bot
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Bot.Types (MessageResponse(RspNoop))
import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (info, CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(Left, Right))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(POST))
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just))
import Network.HTTP.Affjax (AJAX)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (skipSpaces)

listToString :: List Char -> String
listToString = String.fromCharArray <<< toUnfoldable

channelCommandParser
  :: String
  -> ({ channel :: String } -> Bot.Command)
  -> Parser Bot.Command
channelCommandParser str ctor = do
  void $ StringParser.string str
  skipSpaces
  channel <- listToString <$> Parser.many1 StringParser.anyLetter
  pure $ ctor { channel: channel }

commandParser :: Parser Bot.Command
commandParser =
  channelCommandParser "subscribe" Bot.CmdSubscribe
    <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe

handleReceivedMessage
  :: Bot.MessengerConfig
  -> Bot.MessagingEvent
  -> forall e.
     Eff (console :: CONSOLE, err :: EXCEPTION, ajax :: AJAX | e) Unit
handleReceivedMessage config ev@(Bot.MessagingEvent e) =
  let txt ({ message: Bot.Message { text: text } }) = text
      res = runParser commandParser (txt e)
      msg = case res of
        -- TODO: Make this an error response message.
        Left err -> RspNoop
        Right cmd -> evalCommand e.sender cmd
  in void <<< launchAff $ callSendAPI config msg

evalCommand
  :: Bot.MessagingParticipant
  -> Bot.Command
  -> Bot.MessageResponse
evalCommand senderId = go
  where go (Bot.CmdSubscribe channel) =
          Bot.RspText { text: "Hoo-fucking-ray!", recipient: senderId }
        go (Bot.CmdUnsubscribe channel) = unsafeThrow "unsubscribe not implemented"

callSendAPI :: forall e. Bot.MessengerConfig -> Bot.MessageResponse -> Affjax.Affjax e Foreign
callSendAPI (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/messages"
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = url <> query
                                  , content = Just msg }
  in Affjax.affjax req
