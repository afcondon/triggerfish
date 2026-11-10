-- | The rig's `link` frame, read as the Dashboard reads it.
module Test.LinkHealthSpec (runLinkHealthTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual')
import Triggerfish.LinkHealth as LinkHealth

runLinkHealthTests :: Effect Unit
runLinkHealthTests = do
  log "The Link session's health, from the rig's link frame"
  assertEqual' "peers and every receiver's count"
    { actual: LinkHealth.read "link 2 architeuthis=0 es9-daemon=- continuo=3"
    , expected: Just
        { peers: 2
        , refused:
            [ { receiver: "architeuthis", count: Just 0 }
            , { receiver: "es9-daemon", count: Nothing }
            , { receiver: "continuo", count: Just 3 }
            ]
        }
    }
  assertEqual' "peers alone" { actual: LinkHealth.read "link 0", expected: Just { peers: 0, refused: [] } }
  assertEqual' "not a count" { actual: LinkHealth.read "link x", expected: Nothing }
  assertEqual' "an anchor is not a link frame" { actual: LinkHealth.read "anchor 1 2.0 120.0 4.0", expected: Nothing }
