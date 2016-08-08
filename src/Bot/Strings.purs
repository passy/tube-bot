module Bot.Strings where

import Prelude
import Data.String as S
import Data.Array ((:))

lines :: String -> Array String
lines = S.split "\n"

unlines :: Array String -> String
unlines = S.joinWith "\n"

wrapStringAtColumn :: Int -> String -> String
wrapStringAtColumn length str =
  let l = lines str
      wrapped = join $ wrapLine length <$> l
      trimmed = S.trim <$> wrapped
  in unlines trimmed

wrapLine :: Int -> String -> Array String
wrapLine length str =
    if S.length str <= length then [str]
    else S.take length str : wrapLine length (S.drop length str)
