module Bot.DB
  ( disruptionChanges
  , findRecipientsForDisruption
  , subscribeUserToRoute
  , unsubscribeUserFromRoute
  , findRouteByName
  , getAllRoutes
  , RETHINKDB) where

import Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Eff as Eff
import Bot.Types (RouteName, User(User), RouteInfoRow, LineStatusRow)
import Control.Monad.Eff.Exception (Error)
import Data.Array (head)
import Data.Maybe (Maybe())

foreign import data RETHINKDB :: !

foreign import _disruptionChanges
  :: forall e f.
     (Error -> Eff.Eff e Unit)
  -> (LineStatusRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

disruptionChanges :: forall e. Aff.Aff (rethinkdb :: RETHINKDB | e) LineStatusRow
disruptionChanges = Aff.makeAff _disruptionChanges

foreign import _disruptionResolvedChanges
  :: forall e f.
     (Error -> Eff.Eff e Unit)
  -> (LineStatusRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

disruptionResolvedChanges
  :: forall e.
     Aff.Aff (rethinkdb :: RETHINKDB | e) LineStatusRow
disruptionResolvedChanges = Aff.makeAff _disruptionResolvedChanges

foreign import _findRecipientsForDisruption
  :: forall e f.
     RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Array String -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

findRecipientsForDisruption
  :: forall e.
     RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) (Array User)
findRecipientsForDisruption name = map (\id -> User { id: id }) <$> Aff.makeAff (_findRecipientsForDisruption name)

foreign import _findRouteByName
  :: forall e f.
     RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Array RouteInfoRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

findRouteByName
  :: forall e.
     RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) (Maybe RouteInfoRow)
findRouteByName name = head <$> Aff.makeAff (_findRouteByName name)

foreign import _subscribeUserToRoute
  :: forall e f.
     User
  -> RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Unit -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

subscribeUserToRoute
  :: forall e.
     User
  -> RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) Unit
subscribeUserToRoute u route = Aff.makeAff $ _subscribeUserToRoute u route

foreign import _unsubscribeUserFromRoute
  :: forall e f.
     User
  -> RouteName
  -> (Error -> Eff.Eff e Unit)
  -> (Unit -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

unsubscribeUserFromRoute
  :: forall e.
     User
  -> RouteName
  -> Aff.Aff (rethinkdb :: RETHINKDB | e) Unit
unsubscribeUserFromRoute u route = Aff.makeAff $ _unsubscribeUserFromRoute u route

foreign import _allRoutes
  :: forall e f.
     (Error -> Eff.Eff e Unit)
  -> (Array RouteInfoRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

getAllRoutes
  :: forall e.
     Aff.Aff (rethinkdb :: RETHINKDB | e) (Array RouteInfoRow)
getAllRoutes = Aff.makeAff _allRoutes
