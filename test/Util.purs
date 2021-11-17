module Test.Util where

import Prelude

import Control.Monad.Reader.Class (class MonadReader, ask, local)
import Data.Monoid (power, guard)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Test.Assert (assertEqual)

-----------------------------------------------------------------

-- Provide similar API to purescript-test-unit to reduce code changes

suite :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
suite = test

test :: forall m. MonadReader Int m => MonadAff m => String -> m Unit -> m Unit
test msg runTest = do
  indentation <- ask
  let spacing = guard (indentation > 0) " "
  liftEffect $ log $ (power ">>" indentation) <> spacing <> msg
  local (_ + 1) runTest

shouldEqual :: forall m a. MonadAff m => Eq a => Show a => a -> a -> m Unit
shouldEqual actual expected =
  liftEffect $ assertEqual { actual, expected }

assert :: forall m. MonadAff m => String -> Boolean -> m Unit
assert _ true = pure unit
assert msg false = liftAff $ error msg

assertFalse :: forall m. MonadAff m => String -> Boolean -> m Unit
assertFalse msg b = assert msg (not b)

-----------------------------------------------------------------
