module Bot.Strings where

import Prelude
import Data.String as S
import Data.Array ((:))
import Data.Foldable (foldl)

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

segmentMessage :: Int -> String -> Array String
segmentMessage length str =
  let wrappedStr = wrapStringAtColumn length str
      lss = lines wrappedStr
      -- Now ensure that each segment is never longer than `length` as
      -- we have the invariant that each line is already capped to it.
      f { res, cur } l
        | S.length l + S.length cur <= length = { res, cur: cur <> l }
        | otherwise = { res: cur : res, cur: "" }
      r = foldl f { res: [], cur: "" } lss
  in r.cur : r.res
