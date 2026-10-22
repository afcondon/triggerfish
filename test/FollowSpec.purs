-- | **What the rig says as it plays Balistes** (`Triggerfish.Balistes.Follow`):
-- | the fill line's announcements, read exactly, and anything else ignored.
module Test.FollowSpec (runFollowTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')
import Triggerfish.Balistes.Follow (parseFillAt)

check :: String -> Boolean -> Effect Unit
check name ok = do
  assert' ("follow: " <> name) ok
  log ("  ✓ " <> name)

runFollowTests :: Effect Unit
runFollowTests = do
  log "Balistes follow: the fill line"
  check "fills from a step" (parseFillAt "balistes-fill-at 112 1" == Just { step: 112, on: Just true })
  check "stops filling" (parseFillAt "balistes-fill-at 128 0" == Just { step: 128, on: Just false })
  check "no pattern: FILL is the page's own" (parseFillAt "balistes-fill-at 130 -" == Just { step: 130, on: Nothing })
  check "anything else is not a fill frame"
    (parseFillAt "balistes-fill-at 130 maybe" == Nothing && parseFillAt "balistes-now 12 groove" == Nothing && parseFillAt "balistes-fill 1" == Nothing)
