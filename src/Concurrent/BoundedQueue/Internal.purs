module Concurrent.BoundedQueue.Internal
  ( BoundedQueue(..)
  ) where

import Effect.AVar (AVar)

newtype BoundedQueue a =
  BoundedQueue
    { size :: Int
    , contents :: Array (AVar a)
    , readPos :: AVar Int
    , writePos :: AVar Int
    }
