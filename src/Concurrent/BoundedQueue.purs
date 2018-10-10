-- | A concurrent FIFO data structure with bounded capacity.
-- |
-- | This datastructure is useful in various consumer/producer situations.

module Concurrent.BoundedQueue
  ( BoundedQueue
  , new, new'
  , write
  , read
  , isEmpty, isEmpty'
  , tryRead, tryRead'
  , tryWrite, tryWrite'
  ) where

import Prelude

import Data.Array (unsafeIndex)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.AVar as AVarEff
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Partial.Unsafe (unsafePartial)

newtype BoundedQueue a =
  BoundedQueue
    { size ∷ Int
    , contents ∷ Array (AVar a)
    , readPos ∷ AVar Int
    , writePos ∷ AVar Int
    }

-- | Creates a new `BoundedQueue` with the given capacity,
new ∷ ∀ a. Int → Aff (BoundedQueue a)
new size = do
  contents ← replicateA size AVar.empty
  readPos ← AVar.new 0
  writePos ← AVar.new 0
  pure (BoundedQueue { size, contents, readPos, writePos })

-- | Synchronously creates a new `BoundedQueue` with the given capacity.
new' ∷ ∀ a. Int → Effect (BoundedQueue a)
new' size = do
  contents ← replicateA size AVarEff.empty
  readPos ← AVarEff.new 0
  writePos ← AVarEff.new 0
  pure (BoundedQueue { size, contents, readPos, writePos })

-- | Writes an element to the given queue. Will block if the queue is full until
-- | someone reads from it.
write ∷ ∀ a. BoundedQueue a → a → Aff Unit
write (BoundedQueue q) a = do
  w ← AVar.take q.writePos
  AVar.put a (unsafePartial unsafeIndex q.contents w)
  AVar.put ((w + 1) `mod` q.size) q.writePos

-- | Reads an element from the given queue, will block if the queue is empty,
-- | until someone writes to it.
read ∷ ∀ a. BoundedQueue a → Aff a
read (BoundedQueue q) = do
  r ← AVar.take q.readPos
  v ← AVar.take (unsafePartial unsafeIndex q.contents r)
  AVar.put ((r + 1) `mod` q.size) q.readPos
  pure v

-- | Checks whether the given queue is empty. Never blocks.
isEmpty ∷ ∀ a. BoundedQueue a → Aff Boolean
isEmpty (BoundedQueue q) = do
  AVar.tryRead q.readPos >>= case _ of
    Nothing → pure true
    Just r → AVar.tryRead (unsafePartial unsafeIndex q.contents r) <#> case _ of
      Nothing → true
      Just _ → false

-- | Synchronously checks whether the given queue is empty. Never blocks.
isEmpty' ∷ ∀ a. BoundedQueue a → Effect Boolean
isEmpty' (BoundedQueue q) = do
  AVarEff.tryRead q.readPos >>= case _ of
    Nothing → pure true
    Just r → AVarEff.tryRead (unsafePartial unsafeIndex q.contents r) <#>
      case _ of
        Nothing → true
        Just _ → false

-- | Attempts to read an element from the given queue. If the queue is empty,
-- | returns `Nothing`.
-- |
-- | *Careful!* If other readers are blocked on the queue `tryRead` will also
-- | block.
tryRead ∷ ∀ a. BoundedQueue a → Aff (Maybe a)
tryRead (BoundedQueue q) = do
  r ← AVar.take q.readPos
  AVar.tryTake (unsafePartial unsafeIndex q.contents r) >>= case _ of
    Just v → do
      AVar.put ((r + 1) `mod` q.size) q.readPos
      pure (Just v)
    Nothing → do
      AVar.put r q.readPos
      pure Nothing

-- | Synchronously attempts to read an element from the given queue. If the
-- | queue is empty, or there is a concurrent reader, returns `Nothing`.
tryRead' ∷ ∀ a. BoundedQueue a -> Effect (Maybe a)
tryRead' (BoundedQueue q) = do
  mr ← AVarEff.tryTake q.readPos
  case mr of
    Just r → do
      AVarEff.tryTake (unsafePartial unsafeIndex q.contents r) >>= case _ of
        Just v → do
          _ <- AVarEff.tryPut ((r + 1) `mod` q.size) q.readPos
          pure (Just v)
        Nothing → do
          _ <- AVarEff.tryPut r q.readPos
          pure Nothing
    Nothing → pure Nothing

-- | Attempts to write an element into the given queue. If the queue is full,
-- | returns `false` otherwise `true`.
-- |
-- | *Careful!* If other writers are blocked on the queue `tryWrite` will also
-- | block.
tryWrite ∷ ∀ a. BoundedQueue a → a → Aff Boolean
tryWrite (BoundedQueue q) a = do
  w ← AVar.take q.writePos
  AVar.tryPut a (unsafePartial unsafeIndex q.contents w) >>= if _
    then do
      AVar.put ((w + 1) `mod` q.size) q.writePos
      pure true
    else do
      AVar.put w q.writePos
      pure false

-- | Attempts to write an element into the given queue. If the queue is full,
-- | or there is a concurrent writer, returns `false` otherwise `true`.
tryWrite' ∷ ∀ a. BoundedQueue a → a → Effect Boolean
tryWrite' (BoundedQueue q) a = do
  mw ← AVarEff.tryTake q.writePos
  case mw of
    Just w → do
      AVarEff.tryPut a (unsafePartial unsafeIndex q.contents w) >>= if _
        then do
          _ ← AVarEff.tryPut ((w + 1) `mod` q.size) q.writePos
          pure true
        else do
          _ ← AVarEff.tryPut w q.writePos
          pure false
    Nothing → pure false
