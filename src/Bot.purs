module Bot where

import Prelude

import Bot.Types as Bot

import Data.Either (Either(Left, Right))
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List, toUnfoldable)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (skipSpaces)

import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Data.String as String

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

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
commandParser = do
  channelCommandParser "subscribe" Bot.CmdSubscribe <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe

handleReceivedMessage :: Bot.MessagingEvent -> forall e. Eff (console :: CONSOLE | e) Unit
handleReceivedMessage (Bot.MessagingEvent e) = do
  let txt ({ message: Bot.Message { text: text } }) = text
  let res = runParser commandParser (txt e)
  case res of
    Left err -> log $ "Unable to handle message: " <> show err
    Right cmd -> evalCommand cmd

evalCommand :: forall e. Bot.Command -> Eff e Unit
evalCommand = unsafeThrow "not implemented"
