module Bot where

import Prelude
import Bot.DB as DB
import Bot.Strings as Strings
import Bot.Types as Bot
import Control.Monad.Eff.Console as EffConsole
import Control.Monad.Eff.Exception as Ex
import Data.String as String
import Network.HTTP.Affjax as Affjax
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser
import Bot.AffjaxHelper (doJsonRequest)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff, attempt, forkAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Aff.Unsafe (unsafeTrace)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Argonaut (class DecodeJson)
import Data.Array ((:))
import Data.Either (either, Either(Left, Right))
import Data.Foldable (for_)
import Data.HTTP.Method (Method(POST))
import Data.List (List, toUnfoldable)
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Data.Traversable (for)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL, AJAX)
import Network.HTTP.Affjax.Request (class Requestable)
import Text.Parsing.StringParser (Parser, runParser, try)

listToString :: List Char -> String
listToString = String.fromCharArray <<< toUnfoldable

maxMessageLength :: Int
maxMessageLength = 320

roundelUrlFromRoute :: Bot.RouteInfoRow -> URL
roundelUrlFromRoute (Bot.RouteInfoRow { color: Bot.HexColor c }) =
  "https://tube-roundel.rdrei.net/roundel/no-text/" <> String.drop 1 c <> "/image.png"

channelCommandParser
  :: String
  -> ({ route :: Bot.RouteName } -> Bot.Command)
  -> Parser Bot.Command
channelCommandParser str ctor = do
  void $ StringParser.string str
  StringParser.skipSpaces
  name <- listToString <$> Parser.manyTill StringParser.anyChar StringParser.eof
  pure $ ctor { route: Bot.RouteName name }

listLinesParser
  :: Bot.Command
  -> Parser Bot.Command
listLinesParser cmd = try $ do
  void $ StringParser.string "show" <|> StringParser.string "list"
  StringParser.skipSpaces
  void $ StringParser.string "lines" <|> StringParser.string "routes"
  pure cmd

commandParser :: Parser Bot.Command
commandParser =
  channelCommandParser "subscribe" Bot.CmdSubscribe
    <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe
    <|> listLinesParser Bot.CmdListLines

sanitizeInput :: String -> String
sanitizeInput = String.trim >>> String.toLower

withTypingIndicator
  :: forall eff a.
     Bot.User
  -> Bot.MessengerConfig
  -> Aff ( ajax :: AJAX
         | eff ) a
  -> Aff ( ajax :: AJAX
         | eff ) Bot.SendMessageResponse
withTypingIndicator sender config fn = do
    callSendAPI config $ indicator Bot.TypingOn
    fn
    callSendAPI config $ indicator Bot.TypingOff
    where
      indicator t = Bot.RspTypingIndicator { indicator: t, recipient: sender }

handleReceivedMessage
  :: forall e.
     Bot.MessengerConfig
  -> Bot.MessagingEvent
  -> Eff (rethinkdb :: DB.RETHINKDB, ajax :: AJAX, console :: CONSOLE | e) Unit
handleReceivedMessage config (Bot.MessagingEvent { message: Bot.Message { text: text }, sender }) = do
  let res = runParser commandParser $ sanitizeInput text
      tmpl :: forall e'. Aff (rethinkdb :: DB.RETHINKDB | e') Bot.Template
      tmpl = case res of
              Left err -> pure $ Bot.TmplParseError { err }
              Right cmd -> evalCommand sender cmd

  -- The error handling here is dreadful. Help welcome!
  result <- Ex.try $ void $ launchAff $ do
    rsps <- renderTemplate sender <$> tmpl
    withTypingIndicator sender config $ for rsps $ callSendAPI config

  case result of
    Right _ -> pure unit
    Left err -> unsafeThrow $ show err

segmentResponse
  :: forall a.
     Int
  -> String
  -> (String -> { text :: String | a })
  -> Array { text :: String | a }
segmentResponse maxLength txt mapfn =
  mapfn <$> Strings.segmentMessage maxLength txt

renderTemplate
  :: Bot.User
  -> Bot.Template
  -> Array Bot.MessageResponse
renderTemplate user (Bot.TmplPlainText { text }) =
  Bot.RspText <$> segmentResponse maxMessageLength text (\t -> { text: t, recipient: user })
renderTemplate user (Bot.TmplGenericError { err }) =
  pure $ Bot.RspText { text: String.take maxMessageLength $ "Sorry, an error has occurred: " <> Ex.message err
                     , recipient: user }
renderTemplate user (Bot.TmplParseError { err }) =
  pure $ Bot.RspText { text: String.take maxMessageLength $ "Sorry, I didn't get that. Error: " <> show err
                     , recipient: user }
renderTemplate user (Bot.TmplImage { imageUrl }) =
  let att = Bot.AttImage { url: imageUrl }
  in pure $ Bot.RspAttachment { attachment: att
                              , recipient: user }
renderTemplate user (Bot.TmplGenericImage { title, subtitle, imageUrl }) =
  let el = Bot.Element { title: title
                       , subtitle: subtitle
                       , imageUrl: Just imageUrl
                       , itemUrl: Nothing
                       , buttons: [] }
      att = Bot.AttGenericTemplate { elements: pure el }
  in pure $ Bot.RspAttachment { attachment: att
                              , recipient: user }

evalCommand
  :: forall e.
     Bot.User
  -> Bot.Command
  -> Aff (rethinkdb :: DB.RETHINKDB | e) Bot.Template
evalCommand sender = go
  where
    extractRouteName (Bot.RouteInfoRow { display: name }) = name

    go (Bot.CmdSubscribe channel) = do
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ Bot.TmplPlainText { text: "Sorry, I don't know about that line yet." }
        Just r -> do
          DB.subscribeUserToRoute sender channel.route
          pure $ Bot.TmplPlainText { text: "You will now receive updates for the "
                                  <> extractRouteName r
                                  <> " line. Hooray!" }

    go (Bot.CmdUnsubscribe channel) = do
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ Bot.TmplPlainText { text: "I haven't heard about that line. Sure you're subscribed to it?" }
        Just r -> do
          DB.unsubscribeUserFromRoute sender channel.route
          pure $ Bot.TmplPlainText { text: "Sorry for the noise. You will no longer get updates for the "
                                  <> extractRouteName r
                                  <> " line." }

    go Bot.CmdListLines = do
      let extract (Bot.RouteInfoRow { display: name }) = "- " <> name
      routes <- map extract <$> DB.getAllRoutes
      let header = "Here's a list of routes you can currently subscribe to:"

      pure $ Bot.TmplPlainText { text: header <> "\n" <> String.joinWith "\n" routes }

callFBAPI
  :: forall req rsp eff.
   ( DecodeJson rsp , Requestable req )
  => String
  -> Bot.MessengerConfig
  -> req
  -> Aff ( ajax :: AJAX | eff ) rsp
callFBAPI endpoint (Bot.MessengerConfig config) msg =
  let url = "https://graph.facebook.com/v2.7/me/" <> endpoint
      query = "?access_token=" <> config.pageAccessToken
      req = Affjax.defaultRequest { method = Left POST
                                  , url = (url <> query)
                                  , content = Just msg }
  in doJsonRequest req

callSendAPI
  :: forall e.
     Bot.MessengerConfig
  -> Bot.MessageResponse
  -> Aff (ajax :: AJAX | e) Bot.SendMessageResponse
callSendAPI = callFBAPI "messages"

callThreadSettingsAPI
  :: forall e.
     Bot.MessengerConfig
  -> Bot.ThreadSettingsRequest
  -> Aff (ajax :: AJAX | e) Bot.ThreadSettingsResponse
callThreadSettingsAPI = callFBAPI "thread_settings"

listen
  :: forall e.
     Bot.MessengerConfig
  -> Eff (rethinkdb :: DB.RETHINKDB, ajax :: AJAX, err :: Ex.EXCEPTION, console :: CONSOLE | e) Unit
listen config = void <<< launchAff $ do
  disruption <- DB.disruptionChanges
  unsafeTrace $ "Obtained new disruption " <> unsafeStringify disruption
  forkAff $ do
    recipients <- DB.findRecipientsForDisruption $ extractDisruptionName disruption
    -- TODO: Exit here if there's no info.
    routeInfo <- DB.findRouteByName $ extractDisruptionName disruption
    for recipients \user -> do
      unsafeTrace $ "Found user: " <> show user
      case Bot.getLevelFromStatusRow disruption of
        Bot.GoodService -> forkAff $ sendServiceRecoveryNote user routeInfo disruption
        otherwise -> forkAff $ sendServiceDisruptionNote user routeInfo disruption
  where
    extractRouteName :: Bot.RouteInfoRow -> String
    extractRouteName (Bot.RouteInfoRow { display }) = display

    extractDisruptionName :: Bot.LineStatusRow -> Bot.RouteName
    extractDisruptionName (Bot.LineStatusRow { name }) =
      Bot.normalizeRouteName name

    extractInfoImageUrl :: Maybe Bot.RouteInfoRow -> URL
    extractInfoImageUrl info =
      case info of
        Just r -> roundelUrlFromRoute r
        Nothing -> "https://cldup.com/WeSoPrcj4I.svg"

    -- TODO: Shouldn't accept a Maybe for the route info.
    sendServiceRecoveryNote
      :: forall eff.
         Bot.User
      -> Maybe Bot.RouteInfoRow
      -> Bot.LineStatusRow
      -> Aff ( ajax :: AJAX
             , rethinkdb :: DB.RETHINKDB
             , console :: CONSOLE | eff) Unit
    sendServiceRecoveryNote user routeInfo disruption = do
      let title = fromMaybe "Unknown Line" $ extractRouteName <$> routeInfo
      let txt = ( title
               <> ": The line has recovered and is operating again "
               <> "with a good service on the entire line. \\o/" )
      let tmpl = Bot.TmplPlainText { text: txt }

      for_ (renderTemplate user tmpl) $ \rendered -> do
        e <- attempt $ callSendAPI config rendered
        liftEff $ either (EffConsole.log <<< Ex.message) (const $ pure unit) e

    sendServiceDisruptionNote
      :: forall eff.
         Bot.User
      -> Maybe Bot.RouteInfoRow
      -> Bot.LineStatusRow
      -> Aff ( ajax :: AJAX
             , rethinkdb :: DB.RETHINKDB
             , console :: CONSOLE | eff ) Unit
    sendServiceDisruptionNote user routeInfo disruption@(Bot.LineStatusRow { description }) = do
      let title = fromMaybe "Unknown Line" $ extractRouteName <$> routeInfo
      let tmpl = Bot.TmplGenericImage
                { title: title
                , subtitle: pure description
                , imageUrl: extractInfoImageUrl routeInfo }

      for_ (renderTemplate user tmpl) $ \rendered -> do
        e <- attempt $ callSendAPI config rendered
        liftEff $ either (EffConsole.log <<< Ex.message) (const $ pure unit) e

setupThreadSettings
  :: forall e.
     Bot.MessengerConfig
  -> Eff (ajax :: AJAX, err :: Ex.EXCEPTION, console :: CONSOLE | e) Unit
setupThreadSettings config = void <<< launchAff $ do
  log $ "Setting up thread."
  let greeting = Bot.Greeting { text: "Welcome to Disruption Bot! Say 'subscribe <line>' to get update for any London Underground line." }
  res <- callThreadSettingsAPI config greeting
  logShow res
