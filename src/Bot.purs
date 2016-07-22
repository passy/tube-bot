module Bot where

import Prelude
import Bot.Types as Bot
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Control.Alt ((<|>))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just))
import Network.HTTP.Affjax (AJAX, Affjax)
import Network.HTTP.Affjax.Response (class Respondable)
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

handleReceivedMessage :: Bot.MessengerConfig -> Bot.MessagingEvent -> forall e. Eff (console :: CONSOLE | e) Unit
handleReceivedMessage config (Bot.MessagingEvent e) = do
  let txt ({ message: Bot.Message { text: text } }) = text
  let res = runParser commandParser (txt e)
  case res of
    Left err -> log $ "Unable to handle message: " <> show err
    Right cmd -> evalCommand config cmd

evalCommand
  :: forall e. Bot.MessengerConfig
  -> Bot.MessagingParticipant
  -> Bot.Command
  -> Eff (err :: EXCEPTION, ajax :: AJAX | e) Unit
evalCommand conf rid = go
  where go (Bot.CmdSubscribe channel) =
          let rsp = Bot.RspText { text: "Hoo-fucking-ray!", recipientId: rid }
          in void <<< launchAff $ callSendAPI conf rsp
        go (Bot.CmdUnsubscribe channel) = unsafeThrow "unsubscribe not implemented"

callSendAPI :: forall e b. Respondable b => Bot.MessengerConfig -> Bot.MessageResponse -> Affjax.Affjax e b
callSendAPI (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/messages"
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = url <> query
                                  , content = Just msg }
  in Affjax.affjax req
