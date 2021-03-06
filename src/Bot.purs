module Bot where

import Bot.AffjaxHelper (doJsonRequest)
import Bot.DB as DB
import Bot.Strings as Strings
import Bot.Types as Bot
import Control.Alt ((<|>))
import Control.Monad.Aff (later', Aff, launchAff, attempt, forkAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Aff.Unsafe (unsafeTrace)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as EffConsole
import Control.Monad.Eff.Exception as Ex
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Control.Monad.Reader as Reader
import Control.Monad.Trans (lift)
import Data.Argonaut (class DecodeJson)
import Data.Array ((:))
import Data.Either (either, Either(Left, Right))
import Data.Foldable (for_)
import Data.HTTP.Method (Method(POST))
import Data.Lazy (defer, force, Lazy)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String as String
import Data.Tuple (snd, fst, Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (URL, AJAX)
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Request (class Requestable)
import Prelude
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators as Parser
import Text.Parsing.StringParser.String as StringParser

type SendingEnv = { recipient :: Bot.User
                  , config :: Bot.MessengerConfig }

type SendingCtx = Reader.ReaderT SendingEnv

runSendingCtx :: forall m a. SendingEnv -> SendingCtx m a -> m a
runSendingCtx = flip Reader.runReaderT

listToString :: List Char -> String
listToString = String.fromCharArray <<< toUnfoldable

maxMessageLength :: Int
maxMessageLength = 320

typingDelayMillis :: Int
typingDelayMillis = 200

roundelUrlFromRoute :: Bot.RouteInfoRow -> URL
roundelUrlFromRoute (Bot.RouteInfoRow { color: Bot.HexColor c }) =
  "https://tube-roundel.rdrei.net/roundel/no-text/" <> String.drop 1 c <> "/image.png"

helpText :: String
helpText = """Tube Bot understands the following commands:
- "list lines" - Show all available lines.
- "subscribe <line>" - Subscribe disruption alerts for a given line.
- "unsubscribe <line>" - Unsubscribe to no longer receive alerts for a given line.
"""

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

helloParser
  :: Bot.Command
  -> Parser Bot.Command
helloParser cmd = do
  void $ StringParser.string "hello" <|> StringParser.string "hi"
  void $ Parser.manyTill StringParser.anyChar StringParser.eof
  pure cmd

commandParser :: Parser Bot.Command
commandParser =
  channelCommandParser "subscribe" Bot.CmdSubscribe
    <|> channelCommandParser "unsubscribe" Bot.CmdUnsubscribe
    <|> listLinesParser Bot.CmdListLines
    <|> helloParser Bot.CmdHello

sanitizeInput :: String -> String
sanitizeInput = String.trim >>> String.toLower

withTypingIndicator
  :: forall eff a.
     Lazy (Aff ( ajax :: AJAX | eff ) a)
  -> SendingCtx (Aff ( ajax :: AJAX | eff )) a
withTypingIndicator fn = do
    recipient :: Bot.User <- _.recipient <$> Reader.ask
    let indicator t = Bot.RspTypingIndicator { indicator: t, recipient: recipient }
    _ <- callSendAPI' $ indicator Bot.TypingOn
    lift $ later' typingDelayMillis $ pure unit
    res <- liftAff $ force fn
    callSendAPI' $ indicator Bot.TypingOff
    pure res

handleReceivedMessage
  :: forall eff.
     Bot.MessengerConfig
  -> Bot.MessagingEvent
  -> Eff (rethinkdb :: DB.RETHINKDB, ajax :: AJAX, console :: CONSOLE | eff) Unit
handleReceivedMessage config (Bot.MessagingEvent { message: Bot.Message { text: text }, sender }) = do
  let res = runParser commandParser $ sanitizeInput text
      tmpl :: forall eff'. Aff (rethinkdb :: DB.RETHINKDB | eff') (Array Bot.Template)
      tmpl = case res of
              Left err -> pure $ pure $ Bot.TmplParseError { err }
              Right cmd -> evalCommand sender cmd

  result <- Ex.try <<< launchAff $ do
    rsps <- join <<< map (renderTemplate sender) <$> tmpl
    for_ rsps \rsp ->
      runSendingCtx { recipient: sender, config: config } do
        context <- Reader.ask
        -- TODO: Figure out how to avoid running the context here again.
        withTypingIndicator $ defer (\_ -> runSendingCtx context $ callSendAPI' rsp)

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

segmentPlainText
  :: Int
  -> Bot.User
  -> String
  -> Array Bot.MessageResponse
segmentPlainText maxLength user text =
  Bot.RspText <$> segmentResponse maxMessageLength text (\t -> { text: t, recipient: user })

renderTemplate
  :: Bot.User
  -> Bot.Template
  -> Array Bot.MessageResponse
renderTemplate user (Bot.TmplPlainText { text }) =
  segmentPlainText maxMessageLength user text
renderTemplate user (Bot.TmplGenericError { err }) =
  pure $ Bot.RspText { text: String.take maxMessageLength $ "Sorry, an error has occurred: " <> Ex.message err
                     , recipient: user }
renderTemplate user (Bot.TmplParseError { err }) =
  let errorRsp =
        Bot.RspText { text: String.take maxMessageLength $ "Sorry, I didn't get that. Error: " <> show err
                    , recipient: user }
      ctaRsps =
        segmentPlainText maxMessageLength user helpText
  in errorRsp : ctaRsps
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
  -> Aff (rethinkdb :: DB.RETHINKDB | e) (Array Bot.Template)
evalCommand sender = go
  where
    extractRouteName (Bot.RouteInfoRow { display: name }) = name

    go (Bot.CmdSubscribe channel) = do
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ pure $ Bot.TmplPlainText { text: "Sorry, I don't know about that line yet." }
        Just r -> do
          DB.subscribeUserToRoute sender channel.route
          pure $ pure $ Bot.TmplPlainText { text: "You will now receive updates for the "
                                         <> extractRouteName r
                                         <> " line. Hooray!" }

    go (Bot.CmdUnsubscribe channel) = do
      routeInfo <- DB.findRouteByName channel.route
      case routeInfo of
        Nothing ->
          pure $ pure $ Bot.TmplPlainText { text: "I haven't heard about that line. Sure you're subscribed to it?" }
        Just r -> do
          DB.unsubscribeUserFromRoute sender channel.route
          let tmpl = Bot.TmplPlainText { text: "Sorry for the noise. You will no longer get updates for the "
                                      <> extractRouteName r
                                      <> " line." }
          pure $ pure $ tmpl

    go Bot.CmdListLines = do
      let extract (Bot.RouteInfoRow { display: name }) = "- " <> name
      routes <- map extract <$> DB.getAllRoutes
      let header = "Here's a list of routes you can currently subscribe to:"
      let linesTmpl = Bot.TmplPlainText { text: header <> "\n" <> String.joinWith "\n" routes }
      let ctaTmpl =
            Bot.TmplPlainText
              { text: "To subscribe to any line, type \"subscribe Line\", "
                   <> "e.g. \"subscribe Hammersmith & City\"." }

      pure [linesTmpl, ctaTmpl]

    go Bot.CmdHello = do
      let txt = """Why hello there!
      I'm Tube Bot and I can give you updates on TfL service disruptions. Just
      send one of these commands commands to get started.
      """
      let helloTmpl = Bot.TmplPlainText { text: txt }
      let helpTmpl = Bot.TmplPlainText { text: helpText }

      pure [helloTmpl, helpTmpl]

callFBAPI
  :: forall req rsp eff.
   ( DecodeJson rsp, Requestable req )
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

callSendAPI'
  :: forall e.
     Bot.MessageResponse
  -> SendingCtx (Aff (ajax :: AJAX | e)) Bot.SendMessageResponse
callSendAPI' rsp = do
  config <- _.config <$> Reader.ask
  liftAff $ callFBAPI "messages" config rsp

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
  change <- extractChange <$> DB.disruptionChanges
  unsafeTrace $ "Obtained new disruption " <> unsafeStringify (fst change)
  if isImportant (fst change) (snd change)
    then void $ forkAff <<< sendNote $ fst change
    else unsafeTrace $ "Ignoring disruption change without important change."
  where
    sendNote disruption = do
      recipients <- DB.findRecipientsForDisruption $ extractDisruptionName disruption
      routeInfo <- DB.findRouteByName $ extractDisruptionName disruption
      for_ recipients $ \user -> do
        case Tuple routeInfo (Bot.getLevelFromStatusRow disruption) of
          Nothing         /\ _ -> forkAff $ sendErrorMessage user disruption
          Just routeInfo' /\ Bot.GoodService -> forkAff $ sendServiceRecoveryNote user routeInfo' disruption
          Just routeInfo' /\ _ -> forkAff $ sendServiceDisruptionNote user routeInfo' disruption
    
    extractChange :: forall a. DB.RethinkChange a -> Tuple a (Maybe a)
    extractChange (DB.RethinkChange a) = Tuple a.newVal a.oldVal

    -- | Verify that this is a noteworthy event. We often get flip-flopping
    -- reports which can get really spammy, so we will only send a message
    -- if the severity has changed even if there's been an update to the
    -- message or the stops.
    isImportant :: Bot.LineStatusRow -> Maybe Bot.LineStatusRow -> Boolean
    isImportant (Bot.LineStatusRow new) Nothing = true
    isImportant (Bot.LineStatusRow new) (Just (Bot.LineStatusRow old))
      = new.level > old.level || new.level == 0

    extractRouteName :: Bot.RouteInfoRow -> String
    extractRouteName (Bot.RouteInfoRow { display }) = display

    extractDisruptionName :: Bot.LineStatusRow -> Bot.RouteName
    extractDisruptionName (Bot.LineStatusRow { name }) =
      Bot.normalizeRouteName name

    sendErrorMessage
      :: forall eff.
         Bot.User
      -> Bot.LineStatusRow
      -> Aff ( console :: CONSOLE
             , ajax :: AJAX | eff) Unit
    sendErrorMessage user disruption@(Bot.LineStatusRow { name: (Bot.RouteName name) }) = do
      log $ "ERR: Couldn't look up line info for disruption " <> show disruption
      let header =
            """My systems are running in a degraded mode, but there's a new
            disruption:
            """
      let severity = Bot.getLevelFromStatusRow >>> Bot.showDisruptionLevel $ disruption
      let msg = severity <> " on the " <> name <> " line\n"
      let footer = "Sorry, this is all I know."

      let tmpl = Bot.TmplPlainText { text: header <> msg <> footer }

      for_ (renderTemplate user tmpl) $ \rendered -> do
        e <- attempt $ runSendingCtx { recipient: user, config: config } $ callSendAPI' rendered
        either (log <<< Ex.message) (const $ pure unit) e

    sendServiceRecoveryNote
      :: forall eff.
         Bot.User
      -> Bot.RouteInfoRow
      -> Bot.LineStatusRow
      -> Aff ( ajax :: AJAX
             , rethinkdb :: DB.RETHINKDB
             , console :: CONSOLE | eff) Unit
    sendServiceRecoveryNote user (Bot.RouteInfoRow { display }) disruption = do
      let txt = ( display
               <> ": The line has recovered and is operating again "
               <> "with a good service on the entire line. \\o/" )
      let tmpl = Bot.TmplPlainText { text: txt }

      for_ (renderTemplate user tmpl) $ \rendered -> do
        e <- attempt $ runSendingCtx { recipient: user, config: config } $ callSendAPI' rendered
        either (log <<< Ex.message) (const $ pure unit) e

    sendServiceDisruptionNote
      :: forall eff.
         Bot.User
      -> Bot.RouteInfoRow
      -> Bot.LineStatusRow
      -> Aff ( ajax :: AJAX
             , rethinkdb :: DB.RETHINKDB
             , console :: CONSOLE | eff ) Unit
    sendServiceDisruptionNote user routeInfo@(Bot.RouteInfoRow { display }) disruption@(Bot.LineStatusRow { description }) = do
      let headerTmpl = Bot.TmplGenericImage
            { title: display
            , subtitle: pure (Bot.showDisruptionLevel <<< Bot.getLevelFromStatusRow $ disruption)
            , imageUrl: roundelUrlFromRoute routeInfo }
      let infoTmpl = Bot.TmplPlainText { text: description }

      for_ (join $ renderTemplate user <$> [headerTmpl, infoTmpl]) $ \rendered -> do
        e <- attempt $ runSendingCtx { recipient: user, config: config } $ callSendAPI' rendered
        liftEff $ either (EffConsole.log <<< Ex.message) (const $ pure unit) e

setupThreadSettings
  :: forall e.
     Bot.MessengerConfig
  -> Eff (ajax :: AJAX, err :: Ex.EXCEPTION, console :: CONSOLE | e) Unit
setupThreadSettings config = void <<< launchAff $ do
  log $ "Setting up thread."
  let greeting = Bot.Greeting { text: "Welcome to Tube Bot! Why not say hi?" }
  res <- callThreadSettingsAPI config greeting
  logShow res
