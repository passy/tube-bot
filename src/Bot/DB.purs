module Bot.DB
  ( disruptionChanges
  , findRecipientsForDisruption
  , RETHINKDB) where

import Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Eff as Eff
import Bot.Types (RouteName, MessagingParticipant, RouteInfo, LineStatusRow)
import Control.Monad.Eff.Exception (Error)
import Data.Array (head)
import Data.Maybe (Maybe(Just, Nothing))

foreign import data RETHINKDB :: !

foreign import _disruptionChanges
  :: forall e f.
     (Error -> Eff.Eff e Unit)
  -> (LineStatusRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

disruptionChanges :: forall e. Aff.Aff (rethinkdb :: RETHINKDB | e) LineStatusRow
disruptionChanges = Aff.makeAff _disruptionChanges

foreign import _findRecipientsForDisruption
  :: forall e f.
     RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Array MessagingParticipant -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

findRecipientsForDisruption
  :: forall e.
     RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) (Array MessagingParticipant)
findRecipientsForDisruption name = Aff.makeAff $ _findRecipientsForDisruption name

foreign import _findRouteByName
  :: forall e f.
     RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Array RouteInfo -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

findRouteByName
  :: forall e.
     RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) (Maybe RouteInfo)
findRouteByName name = head <$> Aff.makeAff (_findRouteByName name)
