module Bot where

import Prelude
import Bot.DB as DB
import Bot.Types as Bot
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Bot.DB (RETHINKDB)
import Bot.Types (LineStatusRow(LineStatusRow), RouteInfo(RouteInfo), RouteName(RouteName), Template(TmplImageText, TmplParseError, TmplGenericError, TmplPlainText))
import Control.Alt ((<|>))
import Control.Monad.Aff (forkAff, Aff, launchAff)
import Control.Monad.Aff.Unsafe (unsafeTrace)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(Left, Right))
import Data.Foreign (Foreign)
import Data.HTTP.Method (Method(POST))
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL, AJAX)
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
handleReceivedMessage config (Bot.MessagingEvent { message: Bot.Message { text: text }, sender }) = do
  let res = runParser commandParser text
  let msg = case res of
              Left err -> pure $ Bot.TmplParseError { err }
              Right cmd -> evalCommand sender cmd
  void $ launchAff (callSendAPI config =<< renderTemplate sender <$> msg)

renderTemplate
  :: Bot.User
  -> Bot.Template
  -> Bot.MessageResponse
renderTemplate user (TmplPlainText { text }) =
  Bot.RspText { text: text, recipient: user }
renderTemplate user (TmplGenericError { err }) =
  Bot.RspText { text: "Sorry, an error has occurred: " <> show err
              , recipient: user }
renderTemplate user (TmplParseError { err }) =
  Bot.RspText { text: "Sorry, I didn't get that. " <> show err
              , recipient: user }
renderTemplate user (TmplImageText { text, imageUrl }) =
  let att = Bot.AttImage { url: imageUrl }
  in Bot.RspAttachment { attachment: att
                       , recipient: user }

evalCommand
  :: forall e.
     Bot.User
  -> Bot.Command
  -> Aff (rethinkdb :: RETHINKDB | e) Bot.Template
evalCommand sender = go
  where
    go (Bot.CmdSubscribe channel) = do
      let extractRouteName (RouteInfo { name: RouteName name }) = name
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ Bot.TmplPlainText { text: "Sorry, I don't know about that line yet." }
        Just r -> do
          DB.subscribeUserToRoute sender channel.route
          pure $ Bot.TmplPlainText { text: "You will now receive updates for the "
                                  <> extractRouteName r
                                  <> " line. Hooray!" }

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
  -> Eff (rethinkdb :: DB.RETHINKDB, ajax :: AJAX, err :: EXCEPTION | e) Unit
listen config = void <<< launchAff $ do
  disruption <- DB.disruptionChanges
  unsafeTrace $ "Obtained new disruption " <> unsafeStringify disruption
  forkAff $ do
    recipients <- DB.findRecipientsForDisruption $ extractName disruption
    unsafeTrace $ "Found recipients " <> unsafeStringify recipients
    for recipients \user -> do
      unsafeTrace $ "Going into user: " <> unsafeStringify user
      -- TBD
      let att = Bot.AttImage { url: "https://pbs.twimg.com/profile_images/666202270946729984/cBrnNhH-.png" }
      let rsp = Bot.RspText
                { text: "Oh noes, a new disruption on the "
               <> (extractRoute <<< extractName $ disruption)
               <> " line."
                , recipient: user }
      callSendAPI config rsp

  where
    extractName (LineStatusRow { name }) = name
    extractRoute (RouteName name) = name
