-- | A concurrent FIFO data structure with bounded capacity.
-- |
-- | This datastructure is useful in various consumer/producer situations.

module Concurrent.BoundedQueue
  ( new
  , write
  , read
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
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Partial.Unsafe (unsafePartial)

-- | Creates a new `BoundedQueue` with the given capacity,
new :: forall a. Int -> Aff (BoundedQueue a)
new size = do
  contents <- replicateA size AVar.empty
  readPos <- AVar.new 0
  writePos <- AVar.new 0
  pure (BoundedQueue { size, contents, readPos, writePos })

-- | Writes an element to the given queue. Will block if the queue is full until
-- | someone reads from it.
write :: forall a. BoundedQueue a -> a -> Aff Unit
write (BoundedQueue q) a = do
  w <- AVar.take q.writePos
  AVar.put a (unsafePartial unsafeIndex q.contents w)
  AVar.put ((w + 1) `mod` q.size) q.writePos

-- | Reads an element from the given queue, will block if the queue is empty,
-- | until someone writes to it.
read :: forall a. BoundedQueue a -> Aff a
read (BoundedQueue q) = do
  r <- AVar.take q.readPos
  v <- AVar.take (unsafePartial unsafeIndex q.contents r)
  AVar.put ((r + 1) `mod` q.size) q.readPos
  pure v

-- | Checks whether the given queue is empty. Never blocks.
isEmpty :: forall a. BoundedQueue a -> Aff Boolean
isEmpty (BoundedQueue q) = do
  AVar.tryRead q.readPos >>= case _ of
    Nothing -> pure true
    Just r -> AVar.tryRead (unsafePartial unsafeIndex q.contents r) <#> case _ of
      Nothing -> true
      Just _ -> false

-- | Attempts to read an element from the given queue. If the queue is empty,
-- | returns `Nothing`.
-- |
-- | *Careful!* If other readers are blocked on the queue `tryRead` will also
-- | block.
tryRead :: forall a. BoundedQueue a -> Aff (Maybe a)
tryRead (BoundedQueue q) = do
  r <- AVar.take q.readPos
  AVar.tryTake (unsafePartial unsafeIndex q.contents r) >>= case _ of
    Just v -> do
      AVar.put ((r + 1) `mod` q.size) q.readPos
      pure (Just v)
    Nothing -> do
      AVar.put r q.readPos
      pure Nothing

-- | Attempts to write an element into the given queue. If the queue is full,
-- | returns `false` otherwise `true`.
-- |
-- | *Careful!* If other writers are blocked on the queue `tryWrite` will also
-- | block.
tryWrite :: forall a. BoundedQueue a -> a -> Aff Boolean
tryWrite (BoundedQueue q) a = do
  w <- AVar.take q.writePos
  AVar.tryPut a (unsafePartial unsafeIndex q.contents w) >>=
    if _ then do
      AVar.put ((w + 1) `mod` q.size) q.writePos
      pure true
    else do
      AVar.put w q.writePos
      pure false
