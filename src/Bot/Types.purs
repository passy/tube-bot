module Bot.Types where

import Prelude
import Data.Argonaut as J
import Data.String as S
import Bot.JsonHelper ((.??))
import Control.Alt ((<|>))
import Control.Error.Util (note)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut ((~>), (:=), (.?))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(Just, Nothing))
import Network.HTTP.Affjax (URL)
import Network.HTTP.Affjax.Request (toRequest, class Requestable)
import Text.Parsing.StringParser (ParseError)

type SequenceNumber = Int
type MessageId = String

newtype LineStatusRow = LineStatusRow
  { name :: RouteName
  , description :: String
  , disruptions :: Array Disruption
  , level :: Int }

newtype Disruption = Disruption
  { level :: Int
  , summary :: String
  , stops :: Array String }

data DisruptionLevel =
  GoodService | MinorDelays | SevereDelays | PartSuspended | UnknownLevel Int

intToDisruptionLevel :: Int -> DisruptionLevel
intToDisruptionLevel i = case i of
  0 -> GoodService
  1 -> MinorDelays
  2 -> SevereDelays
  3 -> PartSuspended
  otherwise -> UnknownLevel i

getLevelFromStatusRow :: LineStatusRow -> DisruptionLevel
getLevelFromStatusRow (LineStatusRow r) =
  intToDisruptionLevel r.level

newtype RouteInfoRow = RouteInfoRow
  { name :: RouteName
  , display :: String
  , color :: HexColor }

derive instance genericRouteInfoRow :: Generic RouteInfoRow

instance showRouteInfoRow :: Show RouteInfoRow where
  show = gShow

newtype User = User { id :: String }

instance encodeJsonUser :: J.EncodeJson User where
  encodeJson (User { id }) =
    "id" := id ~> J.jsonEmptyObject

-- | The unique name for a line without the "line" suffix, e.g. "district" or
-- "hammersmith & city". Used as key and always lower-case.
-- Only for type-safty reasons.
newtype RouteName = RouteName String

normalizeRouteName :: RouteName -> RouteName
normalizeRouteName (RouteName n) = RouteName $ S.toLower n

derive instance genericRouteName :: Generic RouteName

instance showRouteName :: Show RouteName where
  show = gShow

instance eqRouteName :: Eq RouteName where
  eq = gEq

newtype HexColor = HexColor String

derive instance genericHexColor :: Generic HexColor

instance showHexColor :: Show HexColor where
  show = gShow

instance eqHexColor :: Eq HexColor where
  eq = gEq

newtype MessengerConfig = MessengerConfig { pageAccessToken :: String }

derive instance genericUser :: Generic User

instance showUser :: Show User where
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
  { sender :: User
  , recipient :: User
  , timestamp :: Int
  , message :: Message
  }

derive instance genericMessagingEvent :: Generic MessagingEvent

instance showMessagingEvent :: Show MessagingEvent where
  show = gShow

data Template = TmplPlainText { text :: String }
              | TmplGenericError { err :: Error }
              | TmplParseError { err :: ParseError }
              | TmplImage { imageUrl :: URL }
              | TmplGenericImage { title :: String
                                 , subtitle :: Maybe String
                                 , imageUrl :: URL
                                 }

data MessageResponse = RspText { text :: String, recipient :: User }
                     | RspAttachment { attachment :: Attachment
                                     , recipient :: User }
                     | RspTypingIndicator { indicator :: TypingIndicator
                                          , recipient :: User }

derive instance genericMessageResponse :: Generic MessageResponse

instance showMessageResponse :: Show MessageResponse where
  show = gShow

instance eqMessageResponse :: Eq MessageResponse where
  eq = gEq

instance encodeJsonMessageResponse :: J.EncodeJson MessageResponse where
  encodeJson (RspText { text, recipient })
    = "recipient" := recipient
   ~> "message" := ("text" := text ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject

  encodeJson (RspAttachment { attachment, recipient })
    = "recipient" := recipient
   ~> "message" := ("attachment" := attachment ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject

  encodeJson (RspTypingIndicator { indicator, recipient })
    = "recipient" := recipient
   ~> "sender_action" := indicator
   ~> J.jsonEmptyObject

instance requestableMessageResponse :: Requestable MessageResponse where
  toRequest = toRequest <<< J.encodeJson

data Command = CmdSubscribe { route :: RouteName }
             | CmdUnsubscribe { route :: RouteName }
             | CmdListLines

derive instance genericCommand :: Generic Command

instance showCommand :: Show Command where
  show = gShow

instance eqCommand :: Eq Command where
  eq = gEq

newtype Element = Element
  { title :: String
  , itemUrl :: Maybe URL
  , imageUrl :: Maybe URL
  , subtitle :: Maybe String
  , buttons :: Array Button
  }

derive instance genericElement :: Generic Element

instance showElement :: Show Element where
  show = gShow

instance eqElement :: Eq Element where
  eq = gEq

instance encodeJsonElement :: J.EncodeJson Element where
  encodeJson (Element el)
    = "title" := el.title
   ~> "subtitle" := el.subtitle
   ~> "image_url" := el.imageUrl
   ~> "item_url" := el.itemUrl
   ~> "buttons" := nullEmpty el.buttons
   ~> J.jsonEmptyObject
   where
    nullEmpty :: forall a. Array a -> Maybe (Array a)
    nullEmpty [] = Nothing
    nullEmpty a  = Just a

data Button
  = BtnWeb { title :: String
           , url :: URL }
  | BtnPostback { title :: String
                , payload :: String }
  | BtnPhoneNumber { title :: String
                   , payload :: String }

derive instance genericButton :: Generic Button

instance showButton :: Show Button where
  show = gShow

instance eqButton :: Eq Button where
  eq = gEq

instance encodeJsonButton :: J.EncodeJson Button where
  encodeJson (BtnWeb { title, url })
    = "type" := "web"
   ~> "title" := title
   ~> "url" := url
   ~> J.jsonEmptyObject
  encodeJson (BtnPostback { title, payload })
    = "type" := "postback"
   ~> "title" := title
   ~> "payload" := payload
   ~> J.jsonEmptyObject
  encodeJson (BtnPhoneNumber { title, payload })
    = "type" := "phonenumber"
   ~> "title" := title
   ~> "payload" := payload
   ~> J.jsonEmptyObject

data TypingIndicator = TypingOn | TypingOff

derive instance genericTypingIndicator :: Generic TypingIndicator

instance showTypingIndicator :: Show TypingIndicator where
  show = gShow

instance eqTypingIndicator :: Eq TypingIndicator where
  eq = gEq

instance encodeJsonTypingIndicator :: J.EncodeJson TypingIndicator where
  encodeJson TypingOn = J.fromString "typing_on"
  encodeJson TypingOff = J.fromString "typing_off"

data Attachment = AttImage { url :: URL }
                | AttGenericTemplate { elements :: Array Element }

derive instance genericAttachment :: Generic Attachment

instance showAttachment :: Show Attachment where
  show = gShow

instance eqAttachment :: Eq Attachment where
  eq = gEq

instance encodeJsonAttachment :: J.EncodeJson Attachment where
  encodeJson (AttImage { url })
    = "type" := "image"
   ~> "payload" := ("url" := url ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject
  encodeJson (AttGenericTemplate { elements })
    = "type" := "template"
   ~> "payload" := ( "template_type" := "generic"
                  ~> "elements" := elements
                  ~> J.jsonEmptyObject )
   ~> J.jsonEmptyObject

data SendMessageResponseStatus = StatusOkay
                               | StatusBadRequest
                               | StatusUnknown Int

-- There's a bijection here. Surely there's a typeclass for this.
sendMessageResponseStatusFromInt :: Int -> SendMessageResponseStatus
sendMessageResponseStatusFromInt i =
  case i of
    200 -> StatusOkay
    400 -> StatusBadRequest
    _   -> StatusUnknown i


newtype FacebookError = FacebookError
  { message :: String
  , errorType :: String
  , code :: Int
  , fbtraceId :: String
  }

derive instance genericFacebookError :: Generic FacebookError

instance showFacebookError :: Show FacebookError where
  show = gShow

instance eqFacebookError :: Eq FacebookError where
  eq = gEq

instance decodeMessageResponseError :: J.DecodeJson FacebookError where
  decodeJson json = do
    obj <- J.decodeJson json
    message <- obj .? "message"
    errorType <- obj .? "type"
    code <- obj .? "code"
    fbtraceId <- obj .? "fbtraceId"
    pure $ FacebookError { message, errorType, code, fbtraceId }

data SendMessageResponse = SendMessageResponse
  { error :: Maybe FacebookError
  }

derive instance genericSendMessageResponse :: Generic SendMessageResponse

instance showSendMessageResponse :: Show SendMessageResponse where
  show = gShow

instance eqSendMessageResponse :: Eq SendMessageResponse where
  eq = gEq

instance decodeSendMessageResponse :: J.DecodeJson SendMessageResponse where
  decodeJson json = do
    obj <- note "Expected object" $ J.toObject json
    error <- obj .?? "error"
    pure $ SendMessageResponse { error }

data ThreadSettingsRequest = Greeting { text :: String }

derive instance genericThreadSettingsRequest :: Generic ThreadSettingsRequest

instance showThreadSettingsRequest :: Show ThreadSettingsRequest where
  show = gShow

instance eqThreadSettingsRequest :: Eq ThreadSettingsRequest where
  eq = gEq

instance encodeThreadSettingsRequest :: J.EncodeJson ThreadSettingsRequest where
  encodeJson (Greeting { text })
    = "setting_type" := "greeting"
   ~> "greeting" := ("text" := text ~> J.jsonEmptyObject)
   ~> J.jsonEmptyObject

instance requestableThreadSettingsRequest :: Requestable ThreadSettingsRequest where
  toRequest = toRequest <<< J.encodeJson

-- This should be s/Response/Success and FacebookError as alternative.
data ThreadSettingsResponse = ThreadSettingsResponse { result :: String }

derive instance genericThreadSettingsResponse :: Generic ThreadSettingsResponse

instance showThreadSettingsResponse :: Show ThreadSettingsResponse where
  show = gShow

instance eqThreadSettingsResponse :: Eq ThreadSettingsResponse where
  eq = gEq

instance decodeThreadSettingsResponse :: J.DecodeJson ThreadSettingsResponse where
  decodeJson json = do
    obj <- note "Expected object" $ J.toObject json
    result <- obj .? "result"
    pure $ ThreadSettingsResponse { result }
