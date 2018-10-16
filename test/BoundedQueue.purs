module Test.BoundedQueue where

import Prelude

import Concurrent.BoundedQueue as BQ
import Concurrent.BoundedQueue.Sync as BQS
import Control.Alt ((<|>))
import Data.Either (Either(..), isLeft, isRight)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, parallel, sequential)
import Effect.Class (liftEffect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

race ∷ ∀ a b. Aff a → Aff b → Aff (Either a b)
race a b = sequential ((parallel (map Left a)) <|> (parallel (map Right b)))

delayMs ∷ Int → Aff Unit
delayMs = delay <<< Milliseconds <<< toNumber

boundedQueueSuite ∷ TestSuite
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

boundedQueueSyncSuite :: TestSuite
boundedQueueSyncSuite = do
  suite "(Sync) Simple operations" do
    test "(Sync) inserting and popping elements" do
      q ← liftEffect (BQS.new 2)
      BQ.write q 1
      BQ.write q 2
      r1 ← BQ.read q
      r2 ← BQ.read q
      Assert.equal r1 1
      Assert.equal r2 2
  suite "(Sync) Blocking and unblocking" do
    test "(Sync) writing more than the allowed capacity blocks" do
      q ← liftEffect (BQS.new 1)
      BQ.write q 1
      r ← race (delayMs 50) (BQ.write q 2)
      Assert.assert "Not blocked" (isLeft r)
    test "(Sync) reading unblocks writes blocked on missing capacity" do
      q ← liftEffect (BQS.new 1)
      BQ.write q 1
      _ ← forkAff (delayMs 20 *> (BQ.read q))
      r ← race (delayMs 50) (BQ.write q 2)
      Assert.assert "Blocked too long" (isRight r)
  suite "(Sync) isEmpty" do
    test "(Sync) an empty queue is empty" do
      r ← liftEffect do
        q ← BQS.new 1
        BQS.isEmpty q
      Assert.assert "" r
    test "(Sync) an empty queue with blocked readers is empty" do
      q ← BQ.new 1
      _ ← forkAff (BQ.read q)
      r ← BQ.isEmpty q
      Assert.assert "" r
  suite "(Sync) tryRead blocking and unblocking" do
    test "(Sync) tryRead is non-blocking for empty queue" do
      r ← liftEffect do
        q ← BQS.new 1
        BQS.tryRead q
      Assert.assert "Should've been Nothing" (isNothing r)
    test "(Sync) tryRead reads from a non-empty queue" do
      q ← liftEffect (BQS.new 1)
      BQ.write q 1
      Tuple r1 r2 ← liftEffect do
        r1 ← BQS.tryRead q
        r2 ← BQS.tryRead q
        pure (Tuple r1 r2)
      Assert.equal r1 (Just 1)
      Assert.assert "Should've been Nothing" (isNothing r2)
    test ("(Sync) tryRead does not block when there are consumers blocked on "
      <> "the queue") do
      q ← liftEffect (BQS.new 1)
      _ ← forkAff (BQ.read q)
      r ← race (delayMs 20) (liftEffect (BQS.tryRead q))
      Assert.assert "Should've been Right" (isRight r)
  suite "(Sync) tryWrite blocking and unblocking" do
    test "(Sync) tryWrite is non-blocking for full queue" do
      q ← liftEffect (BQS.new 1)
      BQ.write q 1
      r ← liftEffect (BQS.tryWrite q 2)
      Assert.assertFalse "Write should've failed" r
    test "(Sync) tryWrite writes to a non-full queue" do
      Tuple q rw ← do
        q ← BQ.new 1
        rw ← BQ.tryWrite q 1
        pure (Tuple q rw)
      r ← BQ.read q
      Assert.assert "tryWrite should've succeeded" rw
      Assert.equal r 1
    test ("(Sync) tryWrite does not block when there are writers blocked on " <>
      "the queue") do
      q ← liftEffect (BQS.new 1)
      BQ.write q 1
      _ ← forkAff (BQ.write q 2)
      r ← race (delayMs 20) (liftEffect (BQS.tryWrite q 2))
      Assert.assert "Should've been Right" (isRight r)
