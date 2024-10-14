module Test.Queue where

import Prelude

import Concurrent.Queue as Q
import Control.Alt ((<|>))
import Control.Monad.Reader.Class (class MonadReader)
import Data.Either (Either(..), isLeft, isRight)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, parallel, sequential)
import Effect.Aff.Class (class MonadAff, liftAff)
import Test.Util (suite, test, shouldEqual, assert)

race :: forall a b. Aff a -> Aff b -> Aff (Either a b)
race a b = sequential ((parallel (map Left a)) <|> (parallel (map Right b)))

delayMs :: Int -> Aff Unit
delayMs = delay <<< Milliseconds <<< toNumber

queueSuite :: forall m. MonadReader Int m => MonadAff m => m Unit
queueSuite = do
  suite "Simple operations" do
    test "inserting and popping elements" $ liftAff do
      q <- Q.new
      Q.write q 1
      Q.write q 2
      r1 <- Q.read q
      r2 <- Q.read q
      Q.write q 3
      r3 <- Q.read q
      r1 `shouldEqual` 1
      r2 `shouldEqual` 2
      r3 `shouldEqual` 3
  suite "Blocking and unblocking" do
    test "reading from an empty Queue blocks" $ liftAff do
      q <- Q.new
      r <- race (delayMs 50) (Q.read q)
      assert "Not blocked" (isLeft r)
    test "writing unblocks reads" $ liftAff do
      q <- Q.new
      _ <- forkAff (delayMs 20 *> (Q.write q 1))
      r <- race (delayMs 50) (Q.read q)
      assert "Blocked too long" (isRight r)
  suite "tryRead blocking and unblocking" do
    test "tryRead is non-blocking for empty queue" $ liftAff do
      q <- Q.new
      r <- Q.tryRead q
      assert "Should've been Nothing" (isNothing r)
    test "tryRead reads from a non-empty queue" $ liftAff do
      q <- Q.new
      Q.write q 1
      r1 <- Q.tryRead q
      r2 <- Q.tryRead q
      r1 `shouldEqual` (Just 1)
      assert "Should've been Nothing" (isNothing r2)
    test "tryRead blocks when there are consumers blocked on the queue" $ liftAff do
      q <- Q.new
      _ <- forkAff (Q.read q)
      r <- race (delayMs 20) (Q.tryRead q)
      assert "Should've been Left" (isLeft r)
