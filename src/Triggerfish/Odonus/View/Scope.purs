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

import Halogen as H
import Halogen.HTML as HH
import Triggerfish.Capture.River (Flow(..), riverPanel)
import Triggerfish.Odonus.Grid.Types (Action, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (headColor, style)

-- | The scope — the hero panel on the far left, full height, flex-grow. The river
-- | fills it; the logbook readout floats on top.
scopePanel :: forall m. State -> H.ComponentHTML Action Slots m
scopePanel s =
  HH.div
    [ style "flex:1 1 360px;min-width:0;height:calc(100vh - var(--tf-bar));position:relative;overflow:hidden" ]
    [ riverPanel
        { flow: FlowLeft, headColor }
        { nowMicros: s.nowMicros, notes: s.notes, marks: s.logbook.marks }
    , loggingDot
    ]

-- | The "● logging" dot, floated top-left over the river. There's no arm — the
-- | rig is always capturing — so this just confirms it.
-- |
-- | It used to carry ◆ mark and the note/mark counts too, duplicating the nav's
-- | copy of both with identical numbers. The nav won: mark belongs to PERFORM and
-- | REVIEW alike, and a control shared by two stages belongs to the chrome rather
-- | than to either surface. What's left is the one thing that is genuinely about
-- | this river rather than about the session.
loggingDot :: forall m. H.ComponentHTML Action Slots m
loggingDot =
  HH.div
    [ style $ "position:absolute;top:10px;left:10px;display:flex;align-items:center;gap:5px;"
        <> "padding:4px 9px;border-radius:8px;background:#ffffff0d;backdrop-filter:blur(2px);"
        <> "border:1px solid #ffffff14;font-family:'SF Mono',Menlo,monospace;font-size:9px;color:#c9c4b4" ]
    [ HH.span [ style "width:7px;height:7px;border-radius:50%;background:#c65a4a;box-shadow:0 0 5px #c65a4a" ] []
    , HH.text "logging"
    ]
