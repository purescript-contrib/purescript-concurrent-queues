module Concurrent.BoundedQueue.Sync
  ( new
  , isEmpty
  , tryRead
  , tryWrite
  , module Export
  ) where

import Prelude

import Concurrent.BoundedQueue.Internal (BoundedQueue(..))
import Concurrent.BoundedQueue.Internal (BoundedQueue) as Export
import Data.Array (unsafeIndex)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.AVar as AVarEff
import Partial.Unsafe (unsafePartial)

-- | Synchronously creates a new `BoundedQueue` with the given capacity.
new :: forall a. Int -> Effect (BoundedQueue a)
new size = do
  contents <- replicateA size AVarEff.empty
  readPos <- AVarEff.new 0
  writePos <- AVarEff.new 0
  pure (BoundedQueue { size, contents, readPos, writePos })

-- | Synchronously checks whether the given queue is empty. Never blocks.
isEmpty :: forall a. BoundedQueue a -> Effect Boolean
isEmpty (BoundedQueue q) = do
  AVarEff.tryRead q.readPos >>= case _ of
    Nothing -> pure true
    Just r -> AVarEff.tryRead (unsafePartial unsafeIndex q.contents r) <#>
      case _ of
        Nothing -> true
        Just _ -> false

-- | Synchronously attempts to read an element from the given queue. If the
-- | queue is empty, or there is a concurrent reader, returns `Nothing`.
tryRead :: forall a. BoundedQueue a -> Effect (Maybe a)
tryRead (BoundedQueue q) = do
  mr <- AVarEff.tryTake q.readPos
  case mr of
    Just r -> do
      AVarEff.tryTake (unsafePartial unsafeIndex q.contents r) >>= case _ of
        Just v -> do
          _ <- AVarEff.tryPut ((r + 1) `mod` q.size) q.readPos
          pure (Just v)
        Nothing -> do
          _ <- AVarEff.tryPut r q.readPos
          pure Nothing
    Nothing -> pure Nothing

-- | Attempts to write an element into the given queue. If the queue is full,
-- | or there is a concurrent writer, returns `false` otherwise `true`.
tryWrite :: forall a. BoundedQueue a -> a -> Effect Boolean
tryWrite (BoundedQueue q) a = do
  mw <- AVarEff.tryTake q.writePos
  case mw of
    Just w -> do
      AVarEff.tryPut a (unsafePartial unsafeIndex q.contents w) >>=
        if _ then do
          _ <- AVarEff.tryPut ((w + 1) `mod` q.size) q.writePos
          pure true
        else do
          _ <- AVarEff.tryPut w q.writePos
          pure false
    Nothing -> pure false
