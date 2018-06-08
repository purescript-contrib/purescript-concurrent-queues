module Test.Main where

import Prelude

import Effect (Effect)
import Test.BoundedQueue (boundedQueueSuite)
import Test.Queue (queueSuite)
import Test.Unit (suite)
import Test.Unit.Main (runTest)

main ∷ Effect Unit
main = runTest do
  suite "Queue" queueSuite
  suite "BoundedQueue" boundedQueueSuite
