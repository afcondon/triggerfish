-- | REPLAY tab — Odonus's adapter over the machine-agnostic capture surface
-- | (`Triggerfish.Capture.View`, #28). All the drawing (timeline, marks, loop
-- | regions, playhead, control card) now lives in the shared view; this module just
-- | wires Odonus's `State`/`Action` to it in the `Horizontal` orientation and reads
-- | Odonus-specific bits (voice colour, harmonic context) into the wiring.
-- |
-- | The old on-surface CLIPS strip is gone (#28): ⧉ clip lifts a region straight
-- | into the shared library, and clips are played/managed in the library modal.
-- |
-- | `modeBar` (the LIVE / REPLAY switch) stays here — it's Odonus's own `OdonusView`.
module Triggerfish.Odonus.View.Replay (replayPanel, modeBar) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Triggerfish.Capture.Types (Orientation(..))
import Triggerfish.Capture.View (capturePanel)
import Triggerfish.Odonus.Grid.Types (Action(..), OdonusView(..), Slots, State, replayTimelineId)
import Triggerfish.Odonus.Grid.Widgets (headColor, style)
import Triggerfish.Odonus.Patch (harmonicSummary)

-- | The whole-recording capture surface, laid out horizontally (time → X).
replayPanel :: forall m. State -> H.ComponentHTML Action Slots m
replayPanel s = capturePanel wiring cap
  where
  cap =
    { logbook: s.logbook, playing: s.playing
    , regionDrag: s.regionDrag, contextOpen: s.contextOpen }
  wiring =
    { orientation: Horizontal
    , timelineId: replayTimelineId
    , headColor
    , contextSummary: harmonicSummary
    , regionDown: RegionDown
    , stopPlay: StopPlay
    , saveClip: SaveMarkClip
    , saveScene: Just SaveMarkScene
    , toggleContext: ToggleContext
    }

-- | The floating LIVE / REPLAY switch, top-right of the Odonus surface.
modeBar :: forall m. State -> H.ComponentHTML Action Slots m
modeBar s =
  HH.div
    [ style $ "position:absolute;top:9px;right:11px;z-index:6;display:flex;gap:2px;padding:2px;"
        <> "border-radius:8px;background:#00000022;border:1px solid #ffffff14" ]
    [ tab s VLive "LIVE", tab s VReplay "REPLAY" ]
  where
  tab st v label =
    let on = st.view == v
    in HH.button
        [ HE.onClick \_ -> SetView v
        , style $ "padding:3px 11px;border-radius:6px;cursor:pointer;border:none;font-family:Georgia,serif;"
            <> "font-size:10px;letter-spacing:0.08em;"
            <> (if on then "background:#efece1;color:#2b2822;font-weight:600"
                      else "background:transparent;color:#e8e4d8aa") ]
        [ HH.text label ]
