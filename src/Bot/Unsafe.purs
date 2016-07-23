module Bot.Unsafe where

foreign import unsafeTraceId :: forall a. a -> a
