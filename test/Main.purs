module Test.Main where

import Prelude

import Bot.Strings (wrapStringAtColumn, wrapLine)

import Data.String as String
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foldable (all)
import Test.QuickCheck (Result(..), (===), (<?>))
import Test.Unit (suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Test.Unit.Assert as Assert

theCommutativeProperty :: Int -> Int -> Result
theCommutativeProperty a b = (a + b) === (b + a)

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
      quickCheck (\length str ->
        if length < 1 then Success
        else let res = wrapStringAtColumn length str
             in all (\s -> String.length s <= length) (String.split "\n" res)
                <?> ("property didn't hold for s=" <> str <> ", l=" <> show length)
      )
