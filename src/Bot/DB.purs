module Bot.DB where

import Prelude (Unit)
import Control.Monad.Aff as Aff
import Control.Monad.Eff as Eff
import Control.Monad.Eff.Exception (Error)
import Bot.Types (LineStatusRow)

foreign import data RETHINKDB :: !

foreign import _disruptionChanges
  :: forall e f.
     (Error -> Eff.Eff e Unit)
  -> (LineStatusRow -> Eff.Eff e Unit)
  -> Eff.Eff (rethinkdb :: RETHINKDB | f) Unit

disruptionChanges :: forall e. Aff.Aff (rethinkdb :: RETHINKDB | e) LineStatusRow
disruptionChanges = Aff.makeAff _disruptionChanges
