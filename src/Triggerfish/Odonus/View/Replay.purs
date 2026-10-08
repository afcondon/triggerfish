-- | REPLAY tab — Odonus's adapter over the machine-agnostic capture surface
-- | (`Triggerfish.Capture.View`, #28). All the drawing (timeline, marks, loop
-- | regions, playhead, control card) now lives in the shared view; this module just
-- | wires Odonus's `State`/`Action` to it in the `Horizontal` orientation and reads
-- | Odonus-specific bits (voice colour, harmonic context) into the wiring.
-- |
-- | The old on-surface CLIPS strip is gone (#28): ⧉ clip lifts a region straight
-- | into the shared library, and clips are played/managed in the library modal.
-- |
-- | The surface-size toggle that used to live here moved to `Odonus.View.Nav`
-- | (2026-08-06) along with ◆ mark — the two capture gestures now sit together.
module Triggerfish.Odonus.View.Replay (replayPanel) where

import Data.Maybe (Maybe(..))
import Halogen as H
import Triggerfish.Capture.Types (Orientation(..))
import Triggerfish.Capture.View (capturePanel)
import Triggerfish.Odonus.Grid.Types (Action(..), Slots, State, replayTimelineId)
import Triggerfish.Odonus.Grid.Widgets (headColor)
import Triggerfish.Odonus.Patch (asCode, markContext)

-- | The whole-recording capture surface, laid out horizontally (time → X).
replayPanel :: forall m. State -> H.ComponentHTML Action Slots m
replayPanel s = capturePanel wiring cap
  where
  cap =
    { logbook: s.logbook, playing: s.playing
    , regionDrag: s.regionDrag, contextOpen: s.contextOpen, codeOpen: s.codeOpen, zoom: s.zoom
    , rig: if s.rigLoops then Just { micros: s.nowMicros, beat: s.clockBeat, tempo: s.clockTempo } else Nothing
    , cutting: s.cutting, cutSel: s.cutSel, cardShut: s.cardShut }
  wiring =
    { orientation: Horizontal
    , timelineId: replayTimelineId
    , headColor
    , contextSummary: markContext
    , regionDown: RegionDown
    , stopPlay: StopPlay
    , saveClip: SaveMarkClip
    , saveScene: Just SaveMarkScene
    , deleteMark: Just DeleteMark
    , dismissCard: DismissCard
    , toggleContext: ToggleContext
    , setZoom: SetZoom
    , machine: "odonus"
    , ownCode: \m -> asCode m.patch m.now
    , toggleCode: ToggleCode
    , toLimulus: MarkToLimulus
    , edits: if s.rigLoops then Just { trim: TrimLog, undo: UndoLog, cut: Just { arm: ArmCut, down: CutDown } } else Nothing
    }
