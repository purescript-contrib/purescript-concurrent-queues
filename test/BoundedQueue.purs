module Test.BoundedQueue where

import Prelude

import Concurrent.BoundedQueue as BQ
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, Milliseconds(..), delay, forkAff, parallel, sequential)
import Control.Monad.Aff.AVar (AVAR)
import Data.Either (Either(..), isLeft, isRight)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

race ∷ ∀ a b e. Aff e a → Aff e b → Aff e (Either a b)
race a b = sequential ((parallel (map Left a)) <|> (parallel (map Right b)))

delayMs ∷ ∀ e. Int → Aff e Unit
delayMs = delay <<< Milliseconds <<< toNumber

boundedQueueSuite ∷ ∀ e. TestSuite (avar ∷ AVAR | e)
boundedQueueSuite = do
  suite "Simple operations" do
    test "inserting and popping elements" do
      q ← BQ.new 2
      BQ.write q 1
      BQ.write q 2
      r1 ← BQ.read q
      r2 ← BQ.read q
      Assert.equal r1 1
      Assert.equal r2 2
  suite "Blocking and unblocking" do
    test "writing more than the allowed capacity blocks" do
      q ← BQ.new 1
      BQ.write q 1
      r ← race (delayMs 50) (BQ.write q 2)
      Assert.assert "Not blocked" (isLeft r)
    test "reading unblocks writes blocked on missing capacity" do
      q ← BQ.new 1
      BQ.write q 1
      _ ← forkAff (delayMs 20 *> (BQ.read q))
      r ← race (delayMs 50) (BQ.write q 2)
      Assert.assert "Blocked too long" (isRight r)
  suite "isEmpty" do
    test "an empty queue is empty" do
      q ← BQ.new 1
      r ← BQ.isEmpty q
      Assert.assert "" r
    test "an empty queue with blocked readers is empty" do
      q ← BQ.new 1
      _ ← forkAff (BQ.read q)
      r ← BQ.isEmpty q
      Assert.assert "" r
  suite "tryRead blocking and unblocking" do
    test "tryRead is non-blocking for empty queue" do
      q ← BQ.new 1
      r ← BQ.tryRead q
      Assert.assert "Should've been Nothing" (isNothing r)
    test "tryRead reads from a non-empty queue" do
      q ← BQ.new 1
      BQ.write q 1
      r1 ← BQ.tryRead q
      r2 ← BQ.tryRead q
      Assert.equal r1 (Just 1)
      Assert.assert "Should've been Nothing" (isNothing r2)
    test "tryRead blocks when there are consumers blocked on the queue" do
      q ← BQ.new 1
      _ ← forkAff (BQ.read q)
      r ← race (delayMs 20) (BQ.tryRead q)
      Assert.assert "Should've been Left" (isLeft r)
  suite "tryWrite blocking and unblocking" do
    test "tryWrite is non-blocking for full queue" do
      q ← BQ.new 1
      BQ.write q 1
      r ← BQ.tryWrite q 2
      Assert.assertFalse "Write should've failed" r
    test "tryWrite writes to a non-full queue" do
      q ← BQ.new 1
      rw ← BQ.tryWrite q 1
      r ← BQ.read q
      Assert.assert "tryWrite should've succeeded" rw
      Assert.equal r 1
    test "tryWrite blocks when there are writers blocked on the queue" do
      q ← BQ.new 1
      BQ.write q 1
      _ ← forkAff (BQ.write q 2)
      r ← race (delayMs 20) (BQ.tryWrite q 2)
      Assert.assert "Should've been Left" (isLeft r)
