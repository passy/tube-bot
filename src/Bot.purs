module Bot where

import Prelude
import Bot.DB as DB
import Bot.Types as Bot
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser

import Bot.DB (RETHINKDB)
import Bot.Types (LineStatusRow(LineStatusRow), MessageResponse(RspNoop), RouteName(RouteName))
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeTrace)
import Control.Monad.Eff (Eff)
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
  -> ({ route :: RouteName } -> Bot.Command)
  -> Parser Bot.Command
channelCommandParser str ctor = do
  void $ StringParser.string str
  skipSpaces
  name <- listToString <$> Parser.many1 StringParser.anyLetter
  pure $ ctor { route: RouteName name }

commandParser :: Parser Bot.Command
commandParser =
  channelCommandParser "subscribe" Bot.CmdSubscribe
    <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe

handleReceivedMessage
  :: forall e.
     Bot.MessengerConfig
  -> Bot.MessagingEvent
  -> Eff (err :: EXCEPTION, rethinkdb :: RETHINKDB, ajax :: AJAX | e) Unit
handleReceivedMessage config ev@(Bot.MessagingEvent e) = do
  let txt ({ message: Bot.Message { text: text } }) = text
  let res = runParser commandParser (txt e)
  let msg = case res of
              Left err -> pure RspNoop
              Right cmd -> evalCommand e.sender cmd
  void $ launchAff (callSendAPI config =<< msg)

evalCommand
  :: forall e.
     Bot.User
  -> Bot.Command
  -> Aff (rethinkdb :: RETHINKDB | e) Bot.MessageResponse
evalCommand sender = go
  where go (Bot.CmdSubscribe channel) = do
          DB.subscribeUserToRoute sender channel.route
          -- TODO: Extract text.
          pure $ Bot.RspText { text: "You will now receive updates for " <> show channel.route, recipient: sender }
        go (Bot.CmdUnsubscribe channel) = unsafeThrow "unsubscribe not implemented"

callSendAPI :: forall e. Bot.MessengerConfig -> Bot.MessageResponse -> Affjax.Affjax e Foreign
callSendAPI (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/messages"
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = url <> query
                                  , content = Just msg }
  in Affjax.affjax req

listen
  :: forall e.
     Bot.MessengerConfig
  -> Eff (rethinkdb :: DB.RETHINKDB, err :: EXCEPTION | e) Unit
listen config = void <<< launchAff $ do
  disruption <- DB.disruptionChanges
  recipients <- DB.findRecipientsForDisruption $ getName disruption
  unsafeTrace recipients

  where
    getName (LineStatusRow { name }) = name
