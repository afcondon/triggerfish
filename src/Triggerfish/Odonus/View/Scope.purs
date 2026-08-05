-- | SCOPE panel — the hero panel on the far left: a scrolling note river, octave
-- | gridlines + labels under the stretched note SVG. Notes emit at the right edge
-- | and flow left, fading with age.
-- |
-- | Since 2026-08-05 the river itself is `Triggerfish.Capture.River` (generalised
-- | out of here when Vetula needed the same thing running the other way); this
-- | module is the Odonus adapter — the panel's sizing, the `FlowLeft` wiring, and
-- | the always-on logbook overlay, which is Odonus's own affordance.
module Triggerfish.Odonus.View.Scope (scopePanel) where

import Prelude

import Data.Array (length)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Capture.River (Flow(..), riverPanel)
import Triggerfish.Odonus.Grid.Types (Action(..), Slots, State)
import Triggerfish.Odonus.Grid.Widgets (headColor, style)
import Triggerfish.Odonus.Logbook (noteCount)

-- | The scope — the hero panel on the far left, full height, flex-grow. The river
-- | fills it; the logbook readout floats on top.
scopePanel :: forall m. State -> H.ComponentHTML Action Slots m
scopePanel s =
  HH.div
    [ style "flex:1 1 360px;min-width:0;height:calc(100vh - var(--tf-bar));position:relative;overflow:hidden" ]
    [ riverPanel
        { flow: FlowLeft, headColor }
        { nowMicros: s.nowMicros, notes: s.notes, marks: s.logbook.marks }
    , logbookOverlay s
    ]

-- | The always-on logbook readout + mark button, floated top-left over the
-- | river. There's no arm — the rig is always capturing; the "● logging" dot
-- | just confirms it. "◆ mark" flags the current instant (a gold line on the
-- | river); the count shows captured notes and flags this session.
logbookOverlay :: forall m. State -> H.ComponentHTML Action Slots m
logbookOverlay s =
  HH.div
    [ style $ "position:absolute;top:10px;left:10px;display:flex;align-items:center;gap:9px;"
        <> "padding:5px 9px;border-radius:8px;background:#ffffff0d;backdrop-filter:blur(2px);"
        <> "border:1px solid #ffffff14;font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#c9c4b4" ]
    [ HH.span [ style "display:flex;align-items:center;gap:4px" ]
        [ HH.span [ style "width:7px;height:7px;border-radius:50%;background:#c65a4a;box-shadow:0 0 5px #c65a4a" ] []
        , HH.text "logging" ]
    , HH.span [ style "opacity:0.7" ]
        [ HH.text (show (noteCount s.logbook) <> " notes · " <> show (length s.logbook.marks) <> " ◆") ]
    , HH.button
        [ HE.onClick \_ -> MarkNow
        , style $ "padding:2px 9px;border-radius:6px;cursor:pointer;font-family:Georgia,serif;font-size:10px;"
            <> "color:#e8c14a;border:1px solid #e8c14a55;background:#e8c14a1a" ]
        [ HH.text "◆ mark" ]
    ]
