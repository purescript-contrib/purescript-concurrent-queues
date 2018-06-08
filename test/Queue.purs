module Test.Queue where

import Prelude

import Concurrent.Queue as Q
import Control.Alt ((<|>))
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, parallel, sequential)
import Data.Either (Either(..), isLeft, isRight)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

race ∷ ∀ a b. Aff a → Aff b → Aff (Either a b)
race a b = sequential ((parallel (map Left a)) <|> (parallel (map Right b)))

delayMs ∷ Int → Aff Unit
delayMs = delay <<< Milliseconds <<< toNumber

queueSuite ∷ TestSuite
queueSuite = do
  suite "Simple operations" do
    test "inserting and popping elements" do
      q ← Q.new
      Q.write q 1
      Q.write q 2
      r1 ← Q.read q
      r2 ← Q.read q
      Q.write q 3
      r3 ← Q.read q
      Assert.equal r1 1
      Assert.equal r2 2
      Assert.equal r3 3
  suite "Blocking and unblocking" do
    test "reading from an empty Queue blocks" do
      q ← Q.new
      r ← race (delayMs 50) (Q.read q)
      Assert.assert "Not blocked" (isLeft r)
    test "writing unblocks reads" do
      q ← Q.new
      _ ← forkAff (delayMs 20 *> (Q.write q 1))
      r ← race (delayMs 50) (Q.read q)
      Assert.assert "Blocked too long" (isRight r)
  suite "tryRead blocking and unblocking" do
    test "tryRead is non-blocking for empty queue" do
      q ← Q.new
      r ← Q.tryRead q
      Assert.assert "Should've been Nothing" (isNothing r)
    test "tryRead reads from a non-empty queue" do
      q ← Q.new
      Q.write q 1
      r1 ← Q.tryRead q
      r2 ← Q.tryRead q
      Assert.equal r1 (Just 1)
      Assert.assert "Should've been Nothing" (isNothing r2)
    test "tryRead blocks when there are consumers blocked on the queue" do
      q ← Q.new
      _ ← forkAff (Q.read q)
      r ← race (delayMs 20) (Q.tryRead q)
      Assert.assert "Should've been Left" (isLeft r)
