module Bot.Strings where

import Prelude
import Data.String as S
import Data.Array (null, (:), snoc)
import Data.Foldable (foldl)

lines :: String -> Array String
lines = S.split $ S.Pattern "\n"

unlines :: Array String -> String
unlines = S.joinWith "\n"

wrapLine :: Int -> String -> Array String
wrapLine length str =
    if S.length str <= length then pure str
    else
      let trimmed = S.trim str
      in S.take length trimmed : wrapLine length (S.drop length trimmed)

wrapStringAtColumn :: Int -> String -> String
wrapStringAtColumn length str =
  let l = lines str
      wrapped = join $ wrapLine length <$> l
      trimmed = S.trim <$> wrapped
  in unlines trimmed

-- Implementing this using sequences would be much faster, waiting for
-- https://github.com/hdgarrood/purescript-sequences to land.
segmentMessage :: Int -> String -> Array String
segmentMessage length str =
  let wrappedStr = wrapStringAtColumn length str
      lss = lines wrappedStr
      -- Now ensure that each segment is never longer than `length` as
      -- we have the invariant that each line is already capped to it.
      f { res, cur, i } l
        | S.length l + i <= length = { res, cur: snoc cur l, i: i + S.length l + 1 }
        | otherwise = { res: snoc res cur, cur: pure l, i: S.length l + 1 }
      r = foldl f { res: [], cur: [], i: 0 } lss
      merge { res, cur } = unlines <$> if null cur then res else res <> pure cur
  in merge r
