module Bot.Unsafe where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

foreign import unsafeTraceId :: forall a. a -> a

unsafeTaggedTraceId :: forall a. String -> a -> a
unsafeTaggedTraceId tag s = unsafePerformEff $ do
  log tag
  pure $ unsafeTraceId s
