-- | A concurrent FIFO data structure with bounded capacity.
-- |
-- | This datastructure is useful in various consumer/producer situations.

module Concurrent.BoundedQueue
  ( BoundedQueue
  , new
  , write
  , read
  , isEmpty
  , tryRead
  , tryWrite
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar as AV
import Data.Array (unsafeIndex)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import Partial.Unsafe (unsafePartial)

newtype BoundedQueue a =
  BoundedQueue
    { size ∷ Int
    , contents ∷ Array (AV.AVar a)
    , readPos ∷ AV.AVar Int
    , writePos ∷ AV.AVar Int
    }

-- | Creates a new `BoundedQueue` with the given capacity,
new ∷ ∀ a eff. Int → Aff (avar ∷ AV.AVAR | eff) (BoundedQueue a)
new size = do
  contents ← replicateA size AV.makeEmptyVar
  readPos ← AV.makeVar 0
  writePos ← AV.makeVar 0
  pure (BoundedQueue { size, contents, readPos, writePos })

-- | Writes an element to the given queue. Will block if the queue is full until
-- | someone reads from it.
write ∷ ∀ a eff. BoundedQueue a → a → Aff (avar ∷ AV.AVAR | eff) Unit
write (BoundedQueue q) a = do
  w ← AV.takeVar q.writePos
  AV.putVar a (unsafePartial unsafeIndex q.contents w)
  AV.putVar ((w + 1) `mod` q.size) q.writePos

-- | Reads an element from the given queue, will block if the queue is empty,
-- | until someone writes to it.
read ∷ ∀ a eff. BoundedQueue a → Aff (avar ∷ AV.AVAR | eff) a
read (BoundedQueue q) = do
  r ← AV.takeVar q.readPos
  v ← AV.takeVar (unsafePartial unsafeIndex q.contents r)
  AV.putVar ((r + 1) `mod` q.size) q.readPos
  pure v

-- | Checks whether the given queue is empty. Never blocks.
isEmpty ∷ ∀ a eff. BoundedQueue a → Aff (avar ∷ AV.AVAR | eff) Boolean
isEmpty (BoundedQueue q) = do
  AV.tryReadVar q.readPos >>= case _ of
    Nothing → pure true
    Just r → AV.tryReadVar (unsafePartial unsafeIndex q.contents r) <#> case _ of
      Nothing → true
      Just _ → false

-- | Attempts to read an element from the given queue. If the queue is empty,
-- | returns `Nothing`.
-- |
-- | *Careful!* If other readers are blocked on the queue `tryRead` will also
-- | block.
tryRead ∷ ∀ a eff. BoundedQueue a → Aff (avar ∷ AV.AVAR | eff) (Maybe a)
tryRead (BoundedQueue q) = do
  r ← AV.takeVar q.readPos
  AV.tryTakeVar (unsafePartial unsafeIndex q.contents r) >>= case _ of
    Just v → do
      AV.putVar ((r + 1) `mod` q.size) q.readPos
      pure (Just v)
    Nothing → do
      AV.putVar r q.readPos
      pure Nothing

-- | Attempts to write an element into the given queue. If the queue is full,
-- | returns `false` otherwise `true`.
-- |
-- | *Careful!* If other writers are blocked on the queue `tryWrite` will also
-- | block.
tryWrite ∷ ∀ a eff. BoundedQueue a → a → Aff (avar ∷ AV.AVAR | eff) Boolean
tryWrite (BoundedQueue q) a = do
  w ← AV.takeVar q.writePos
  AV.tryPutVar a (unsafePartial unsafeIndex q.contents w) >>= if _
    then do
      AV.putVar ((w + 1) `mod` q.size) q.writePos
      pure true
    else do
      AV.putVar w q.writePos
      pure false
