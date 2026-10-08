-- | SCOPE panel — the hero panel on the far left: a scrolling note river, octave
-- | gridlines + labels under the stretched note SVG. Notes emit at the right edge
-- | and flow left, fading with age.
-- |
-- | Since 2026-08-05 the river itself is `Triggerfish.Capture.River` (generalised
-- | out of here when Vetula needed the same thing running the other way); this
-- | module is the Odonus adapter — the panel's sizing, its head colours, and
-- | the always-on logbook overlay, which is Odonus's own affordance.
module Triggerfish.Odonus.View.Scope (scopePanel) where

import Halogen as H
import Halogen.HTML as HH
import Triggerfish.Capture.River (riverPanel)
import Triggerfish.Odonus.Grid.Types (Action, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (headColor, style)

-- | The scope — the hero panel on the far left, full height, flex-grow. The river
-- | fills it; the logbook readout floats on top.
scopePanel :: forall m. State -> H.ComponentHTML Action Slots m
scopePanel s =
  HH.div
    [ style "flex:1 1 360px;min-width:0;height:calc(100vh - var(--tf-bar));position:relative;overflow:hidden" ]
    [ riverPanel
        { headColor }
        { nowMicros: s.nowMicros, notes: s.notes, marks: s.logbook.marks }
    ]
