-- | An unbounded FIFO data structure for concurrent access.
module Concurrent.Queue
  ( Queue
  , new
  , read
  , write
  , tryRead
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar, tryReadVar)
import Data.Maybe (Maybe(..))

-- | An unbounded Queue fit for concurrent access.
newtype Queue a = Queue
  { readEnd ∷ AVar (Stream a)
  , writeEnd ∷ AVar (Stream a)
  }

type Stream a = AVar (QItem a)
data QItem a = QItem a (Stream a)

-- | Creates a new `Queue`.
new ∷ ∀ a e. Aff (avar ∷ AVAR | e) (Queue a)
new = do
  hole ← makeEmptyVar
  readEnd ← makeVar hole
  writeEnd ← makeVar hole
  pure (Queue { readEnd, writeEnd })

-- | Writes a new value into the queue
write ∷ ∀ a e. Queue a → a → Aff (avar ∷ AVAR | e) Unit
write (Queue q) a = do
  newHole ← makeEmptyVar
  oldHole ← takeVar q.writeEnd
  putVar (QItem a newHole) oldHole
  putVar newHole q.writeEnd

-- | Reads a value from the queue. Blocks if the queue is empty, and resumes
-- | when it has been written to.
read ∷ ∀ a e. Queue a → Aff (avar ∷ AVAR | e) a
read (Queue q) = do
  readEnd ← takeVar q.readEnd
  QItem a newRead ← readVar readEnd
  putVar newRead q.readEnd
  pure a

-- | Attempts to read a value from the queue. Fails with `Nothing` if the queue
-- | is empty.
-- |
-- | *CAREFUL!* This will block if other readers are blocked on the
-- | queue.
tryRead ∷ ∀ a e. Queue a → Aff (avar ∷ AVAR | e) (Maybe a)
tryRead (Queue q) = do
  readEnd ← takeVar q.readEnd
  tryReadVar readEnd >>= case _ of
    Just (QItem a newRead) → do
      putVar newRead q.readEnd
      pure (Just a)
    Nothing → do
      putVar readEnd q.readEnd
      pure Nothing
