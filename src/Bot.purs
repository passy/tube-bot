module Bot where

import Prelude
import Bot.DB as DB
import Bot.Types as Bot
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Bot.AffjaxHelper (doJsonRequest)
import Bot.DB (RETHINKDB)
import Control.Alt ((<|>))
import Control.Monad.Aff (forkAff, Aff, launchAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Aff.Unsafe (unsafeTrace)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error, try)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Either (Either(Left, Right))
import Data.HTTP.Method (Method(POST))
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (for)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)
import Text.Parsing.StringParser (Parser, runParser)

listToString :: List Char -> String
listToString = String.fromCharArray <<< toUnfoldable

channelCommandParser
  :: String
  -> ({ route :: Bot.RouteName } -> Bot.Command)
  -> Parser Bot.Command
channelCommandParser str ctor = do
  void $ StringParser.string str
  StringParser.skipSpaces
  name <- listToString <$> Parser.many1 StringParser.anyLetter
  pure $ ctor { route: Bot.RouteName name }

commandParser :: Parser Bot.Command
commandParser =
  channelCommandParser "subscribe" Bot.CmdSubscribe
    <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe

handleReceivedMessage
  :: forall e.
     Bot.MessengerConfig
  -> Bot.MessagingEvent
  -> Eff (rethinkdb :: RETHINKDB, ajax :: AJAX, console :: CONSOLE | e) Unit
handleReceivedMessage config (Bot.MessagingEvent { message: Bot.Message { text: text }, sender }) = do
  let res = runParser commandParser text
  let msg = case res of
              Left err -> pure $ Bot.TmplParseError { err }
              Right cmd -> evalCommand sender cmd

  -- The error handling here is dreadful. Help welcome!
  result <- try $ void $ launchAff $ callSendAPI config =<< renderTemplate sender <$> msg

  case result of
    Right _ -> pure unit
    Left err -> unsafeThrow $ show err

renderTemplate
  :: Bot.User
  -> Bot.Template
  -> Bot.MessageResponse
renderTemplate user (Bot.TmplPlainText { text }) =
  Bot.RspText { text: text, recipient: user }
renderTemplate user (Bot.TmplGenericError { err }) =
  Bot.RspText { text: "Sorry, an error has occurred: " <> show err
              , recipient: user }
renderTemplate user (Bot.TmplParseError { err }) =
  Bot.RspText { text: "Sorry, I didn't get that. " <> show err
              , recipient: user }
renderTemplate user (Bot.TmplImage { imageUrl }) =
  let att = Bot.AttImage { url: imageUrl }
  in Bot.RspAttachment { attachment: att
                       , recipient: user }
renderTemplate user (Bot.TmplGenericImage { title, subtitle, imageUrl }) =
  let el = Bot.Element { title: title
                       , subtitle: subtitle
                       , imageUrl: Just imageUrl
                       , itemUrl: Nothing
                       , buttons: [] }
      att = Bot.AttGenericTemplate { elements: pure el }
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
      let extractRouteName (Bot.RouteInfoRow { name: Bot.RouteName name }) = name
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ Bot.TmplPlainText { text: "Sorry, I don't know about that line yet." }
        Just r -> do
          DB.subscribeUserToRoute sender channel.route
          pure $ Bot.TmplPlainText { text: "You will now receive updates for the "
                                  <> extractRouteName r
                                  <> " line. Hooray!" }

    go (Bot.CmdUnsubscribe channel) =
      pure $ Bot.TmplGenericError { err: error "Sorry, unsubscribing is still in the works." }

callSendAPI
  :: forall e.
     Bot.MessengerConfig
  -> Bot.MessageResponse
  -> Aff (ajax :: AJAX | e) Bot.SendMessageResponse
callSendAPI (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/messages"
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = (url <> query)
                                  , content = Just msg }
  in doJsonRequest req

callThreadSettingsAPI
  :: forall e.
     Bot.MessengerConfig
  -> Bot.ThreadSettingsRequest
  -> Aff (ajax :: AJAX | e) Bot.ThreadSettingsResponse
callThreadSettingsAPI (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/thread_settings"
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = (url <> query)
                                  , content = Just msg }
  in doJsonRequest req

listen
  :: forall e.
     Bot.MessengerConfig
  -> Eff (rethinkdb :: DB.RETHINKDB, ajax :: AJAX, err :: EXCEPTION | e) Unit
listen config = void <<< launchAff $ do
  disruption <- DB.disruptionChanges
  unsafeTrace $ "Obtained new disruption " <> unsafeStringify disruption
  forkAff $ do
    recipients <- DB.findRecipientsForDisruption $ extractName disruption
    -- TODO: Exit here if there's no info.
    routeInfo <- DB.findRouteByName $ extractName disruption
    for recipients \user -> do
      let txt = "Oh noes, a new disruption on the "
             <> (extractRoute <<< extractName $ disruption)
             <> " line."
      let tmpl = Bot.TmplGenericImage
                { title: extractRoute <<< extractName $ disruption
                , subtitle: pure txt
                , imageUrl: extractInfoImageUrl routeInfo }
      callSendAPI config $ renderTemplate user tmpl

  where
    extractName (Bot.LineStatusRow { name }) = name
    extractRoute (Bot.RouteName name) = name

    extractInfoImageUrl info =
      case info of
        Just (Bot.RouteInfoRow r) -> r.image_url
        Nothing -> "https://cldup.com/WeSoPrcj4I.svg"

setupThreadSettings
  :: forall e.
     Bot.MessengerConfig
  -> Eff (ajax :: AJAX, err :: EXCEPTION, console :: CONSOLE | e) Unit
setupThreadSettings config = void <<< launchAff $ do
  log $ "Setting up thread."
  let greeting = Bot.Greeting { text: "Welcome to Disruption Bot! Say 'subscribe <line>' to get update for any London Underground line." }
  res <- callThreadSettingsAPI config greeting
  logShow res
