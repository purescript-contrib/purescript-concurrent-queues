module Test.Main where

import Prelude

import Control.Monad.Reader.Trans (runReaderT)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.BoundedQueue (boundedQueueSuite, boundedQueueSyncSuite)
import Test.Queue (queueSuite)
import Test.Util (suite)

main :: Effect Unit
main = launchAff_ $ flip runReaderT 0 do
  suite "Queue" queueSuite
  suite "BoundedQueue" boundedQueueSuite
  suite "(Sync) BoundedQueue" boundedQueueSyncSuite
