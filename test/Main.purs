module Test.Main where

import Prelude
import Data.String as String
import Test.Unit.Assert as Assert
import Bot.Strings (segmentMessage, wrapStringAtColumn, wrapLine)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foldable (all)
import Data.List (toUnfoldable)
import Test.QuickCheck (Result, (===), (<?>))
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, listOf, chooseInt, sized, elements)
import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

newtype Nat = Nat Int

instance arbitraryNat :: Arbitrary Nat where
  arbitrary = Nat <$> chooseInt 1 400

newtype Message = Message String

-- TODO: Consider adding emoji / unicode characters here. I deliberately exclude
-- them at the moment as there currently is no way in which user messages are
-- repeated back to the user.
words :: Gen String
words = elements "" ["hello", "world", "\n", "doggie", "good", "doggo", "pupper"]

instance arbitraryMessage :: Arbitrary Message where
  arbitrary = sized \n -> do
    len <- chooseInt 0 n
    lwords <- toUnfoldable <$> listOf len words
    pure $ Message $ String.joinWith " " lwords

main
  :: forall eff.
     Eff ( console :: CONSOLE
         , testOutput :: TESTOUTPUT
         , avar :: AVAR
         , random :: RANDOM
         | eff ) Unit
main = runTest do
  suite "test max length" do
    test "wrap long line" do
      let input = "this line is too long"
      Assert.equal ["this ","line ","is to","o lon","g"] (wrapLine 5 input)
    test "individual lines in wrapStringAtColumn are always under limit" do
      quickCheck (\(Nat length) (Message str) ->
        let res = wrapStringAtColumn length str
        in all (\s -> String.length s <= length) (String.split "\n" res)
           <?> ("property didn't hold for s=" <> str <> ", l=" <> show length)
      )
    test "doesn't change conforming lines" do
      let input = "hello\nworld"
      Assert.equal input (wrapStringAtColumn 10 input)
    test "wraps single" do
      let input = "helloworld"
      Assert.equal "hello\nworld" (wrapStringAtColumn 5 input)
    test "strips on wrap" do
      let input = "hello world"
      Assert.equal "hello\nworld" (wrapStringAtColumn 6 input)
    test "recursively wraps" do
      let input = "111222333\n444555"
      Assert.equal "111\n222\n333\n444\n555" (wrapStringAtColumn 3 input)
    test "lines are always shorter than the max length" do
      quickCheck (\(Nat length) (Message str) ->
        let res = wrapStringAtColumn length str
        in all (\s -> String.length s <= length) (String.split "\n" res)
           <?> ("property didn't hold for s=" <> str <> ", l=" <> show length)
      )
  suite "segment message" do
    test "segments are shorter than max length" do
      quickCheck (\(Nat length) (Message str) ->
        let res = segmentMessage length str
        in all (\s -> String.length s <= length) res
           <?> ("property didn't hold for l=" <> show length <> ", s=" <> show str)
      )

    test "simple segments test" do
      let input = "simple message"
      let res = segmentMessage 7 input
      Assert.equal [ "simple", "message" ] res

    test "segments remain in order" do
      let input = "this is a relatively\nlong message we want to segment across multiple payloads"
      let res = segmentMessage 20 input
      Assert.equal [ "this is a relatively"
                   , "long message we want"
                   , "to segment across mu"
                   , "ltiple payloads" ] res

    test "segments retain intermittend line breaks" do
      let input = "this\ngoes\nover\nmany\nlines"
      let res = segmentMessage 10 input
      Assert.equal [ "this\ngoes"
                   , "over\nmany"
                   , "lines" ] res
