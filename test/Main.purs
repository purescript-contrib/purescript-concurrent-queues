module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Test.BoundedQueue (boundedQueueSuite)
import Test.Queue (queueSuite)
import Test.Unit (suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main ∷ ∀ e. Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR | e) Unit
main = runTest do
  suite "Queue" queueSuite
  suite "BoundedQueue" boundedQueueSuite
