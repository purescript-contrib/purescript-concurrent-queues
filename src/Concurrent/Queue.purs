-- | An unbounded FIFO data structure for concurrent access.
module Concurrent.Queue
  ( Queue
  , new
  , read
  , write
  , tryRead
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Data.Maybe (Maybe(..))

-- | An unbounded Queue fit for concurrent access.
newtype Queue a = Queue
  { readEnd ∷ AVar (Stream a)
  , writeEnd ∷ AVar (Stream a)
  }

type Stream a = AVar (QItem a)
data QItem a = QItem a (Stream a)

-- | Creates a new `Queue`.
new ∷ ∀ a. Aff (Queue a)
new = do
  hole ← AVar.empty
  readEnd ← AVar.new hole
  writeEnd ← AVar.new hole
  pure (Queue { readEnd, writeEnd })

-- | Writes a new value into the queue
write ∷ ∀ a. Queue a → a → Aff Unit
write (Queue q) a = do
  newHole ← AVar.empty
  oldHole ← AVar.take q.writeEnd
  AVar.put (QItem a newHole) oldHole
  AVar.put newHole q.writeEnd

-- | Reads a value from the queue. Blocks if the queue is empty, and resumes
-- | when it has been written to.
read ∷ ∀ a. Queue a → Aff a
read (Queue q) = do
  readEnd ← AVar.take q.readEnd
  QItem a newRead ← AVar.read readEnd
  AVar.put newRead q.readEnd
  pure a

-- | Attempts to read a value from the queue. Fails with `Nothing` if the queue
-- | is empty.
-- |
-- | *CAREFUL!* This will block if other readers are blocked on the
-- | queue.
tryRead ∷ ∀ a. Queue a → Aff (Maybe a)
tryRead (Queue q) = do
  readEnd ← AVar.take q.readEnd
  AVar.tryRead readEnd >>= case _ of
    Just (QItem a newRead) → do
      AVar.put newRead q.readEnd
      pure (Just a)
    Nothing → do
      AVar.put readEnd q.readEnd
      pure Nothing
