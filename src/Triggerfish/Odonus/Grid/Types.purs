-- | Shared types for the Odonus grid: the knob-drag target algebra, the
-- | component State, and the Action set. Held low in the module DAG so every
-- | view module can refer to them without a cycle.
module Triggerfish.Odonus.Grid.Types
  ( KnobTarget(..)
  , targetRange
  , applyTarget
  , DragState
  , PendingInput
  , Scene
  , replayTimelineId
  -- Chunk / Mark / RegionEdge / RegionDrag / Logbook / PlayState / PlaySource
  -- now come from Capture.Types (#28), re-exported via the module below.
  , module Triggerfish.Capture.Types
  , module Triggerfish.Clips
  , module Reef.Gen
  , genLabel
  , genSub
  , marblesPadId
  , Stage(..)
  , stagePath
  , stageFromPath
  , TwisterField(..)
  , twisterFieldLabel
  , State
  , PolyInst
  , Action(..)
  , Slots
  ) where

import Prelude

import Data.Array as Array

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Widgets.Select as Select
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Reef.Input as RI
import Reef.Route as Route
import Triggerfish.Odonus.Samples as Samples
import Triggerfish.Odonus.Model as M
import Triggerfish.Poly as Poly
import Reef.Voices as RV
import Triggerfish.Odonus.Marbles as Marbles
import Triggerfish.Clips (NoteEvent, MidiClip)
-- The always-on capture types (logbook / marks / loop regions / replay play-state)
-- are now machine-agnostic in `Triggerfish.Capture.Types` (#28); imported here and
-- re-exported below so every Odonus view module reaches them unchanged.
import Triggerfish.Capture.Types (Chunk, Mark, RegionEdge(..), RegionDrag, PlaySource(..), PlayState, Logbook, Zoom)
import Triggerfish.Preset (Preset)
import Triggerfish.Glyph (ChipView)
import Triggerfish.Transport (Sounding)
-- The gen-source descriptor moved to the portable reef package (Reef.Gen) so
-- generation runs on both runtimes; re-export it here under its historical home
-- (the `module Reef.Gen` in the export list) so every existing importer of
-- Grid.Types is unchanged, while the names stay in unqualified scope here.
import Reef.Gen
  ( GenKind(..), GenSource, genKinds, genDefaultRate, genDefaultAmt, rateMax
  , periodOf, toggleGen, setRate, setAmt )
import Binnacle (Binnacle)
import Binnacle.Midi as Midi
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Binnacle.Scheduler as Scheduler

data KnobTarget
  = CellNote Int
  | CellDur Int
  | CellRatchet Int  -- per-cell retrigger count (RATCHET field)
  | CellVel Int      -- per-cell base velocity (VEL field)
  | HeadDir Int
  | HeadSpeed Int
  | HeadTransp Int
  | HeadOffset Int
  | HeadLen Int
  | HeadDiv Int      -- per-voice Euclidean pulse count (DIV) = k in E(k,n)
  | HeadEStep Int    -- per-voice Euclidean step count (STEPS) = n in E(k,n)
  | Spread
  | GateLen
  | SwingAmt          -- groove: % a step delays off-beat model steps (lives on State)
  | VelHuman          -- velocity humanise range ± (lives on State)
  | FanOff           -- Reichian FAN: spread head offsets into a canon
  | StaggerLen       -- Reichian STAGGER: ramp head lengths for metric phasing
  | HeadSpread       -- Reichian SPREAD: fan head transposes into a register-canon
  | GenRate GenKind  -- a source's randomisation rate, lives on State (see DragMove)
  | GenAmt GenKind   -- a source's mutation depth / intensity (0..100)

targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  CellNote _ -> { lo: 36, hi: 84 }
  CellDur _ -> { lo: 1, hi: 8 }
  CellRatchet _ -> { lo: 1, hi: 8 }
  CellVel _ -> { lo: 1, hi: 127 }
  HeadDir _ -> { lo: 0, hi: 2 }
  HeadSpeed _ -> { lo: 0, hi: length M.speedTable - 1 }
  HeadTransp _ -> { lo: -24, hi: 24 }
  HeadOffset _ -> { lo: 0, hi: 15 }
  HeadLen _ -> { lo: 1, hi: 16 }
  HeadDiv _ -> { lo: 0, hi: M.maxEsteps }
  HeadEStep _ -> { lo: 1, hi: M.maxEsteps }
  Spread -> { lo: 1, hi: 12 }
  GateLen -> { lo: 10, hi: 200 }
  SwingAmt -> { lo: 0, hi: 60 }
  VelHuman -> { lo: 0, hi: 40 }
  FanOff -> { lo: 0, hi: 5 }
  StaggerLen -> { lo: 0, hi: 5 }
  HeadSpread -> { lo: 0, hi: length M.spreadVoicings - 1 }
  GenRate _ -> { lo: 0, hi: rateMax }
  GenAmt _ -> { lo: 0, hi: 100 }

applyTarget :: KnobTarget -> Int -> M.Odonus -> M.Odonus
applyTarget t v = case t of
  CellNote i -> M.setNote i v
  CellDur i -> M.setCellDur i v
  CellRatchet i -> M.setCellRatchet i v
  CellVel i -> M.setCellVel i v
  HeadDir h -> M.setHeadDir h v
  HeadSpeed h -> M.setHeadSpeedIx h v
  HeadTransp h -> M.setHeadTransp h v
  HeadOffset h -> M.setHeadOffset h v
  HeadLen h -> M.setHeadLen h v
  HeadDiv h -> M.setHeadPulses h v
  HeadEStep h -> M.setHeadEuclidSteps h v
  Spread -> M.setSpread v
  GateLen -> M.setGatePct v
  FanOff -> M.fanOffsets v
  StaggerLen -> M.staggerLengths v
  HeadSpread -> M.spreadOctaves v
  SwingAmt -> identity     -- handled at State level in DragMove, not on Odonus
  VelHuman -> identity      -- handled at State level in DragMove, not on Odonus
  GenRate _ -> identity   -- handled at State level in DragMove, not on Odonus
  GenAmt _ -> identity     -- handled at State level in DragMove, not on Odonus

-- | `curVal` tracks the live value as the knob is dragged, so on release the
-- | lockstep client can broadcast the FINAL value as a Set* input (P4c). A knob
-- | edit syncs on release, not per-move — the backend jumps to the settled value.
type DragState = { target :: KnobTarget, startY :: Int, startVal :: Int, curVal :: Int }

-- | A locally-queued, tick-tagged input awaiting its beat (lockstep, P4c). A user
-- | gesture that must stay in sync with the rig is deferred: it's enqueued here at
-- | `step = soundingStep + buffer` AND broadcast to the BEAM tagged with the SAME
-- | step, so both runtimes apply it on the same model step and the co-simulation
-- | never flams. The Step loop drains entries whose `step` has arrived.
type PendingInput = { step :: Int, input :: RI.Input }

-- | `NoteEvent` (one emitted note in the scrolling monitor / logbook / clip) now
-- | lives in `Triggerfish.Clips` — the one shared definition — and is re-exported
-- | from this module (via `module Triggerfish.Clips` above), so existing importers
-- | are unchanged.

-- | `PlaySource` / `PlayState` (what a REPLAY loop is playing + its scheduling
-- | watermark) moved to `Triggerfish.Capture.Types` (#28) and are re-exported here.

-- | A captured performance clip is now `Triggerfish.Clips.MidiClip` (recording axis
-- | #27): the same self-contained, rebased-to-zero note buffer, promoted to a
-- | machine-agnostic library type with room for metadata (tags/notes/bpm/key). The
-- | old `patch` field became `MidiClip.context`. Persisted in the SHARED clip store
-- | (`Triggerfish.Clips.Store`), not the Odonus envelope.

-- | A saved whole-Odonus setting under a name — the recallable PRESET and the
-- | unit of composition (sequencing scenes builds flowing fugues with key
-- | changes and voices dropping in and out). The setting is stored as its
-- | Lepidoptera `text` (the full authored patch rendered to eDSL — the same
-- | canonical, transferable form the Tidal page shows), parsed back on recall.
-- | Lossless: the A3 round-trip is byte-stable.
type Scene = { name :: String, text :: String }

-- | `Chunk` / `Mark` / `RegionEdge` / `RegionDrag` / `Logbook` (the always-on
-- | capture types) moved to `Triggerfish.Capture.Types` (#28) and are re-exported
-- | here, so every Odonus view module reaches them by their historical names.

-- | The display strings for each gen source — UI-only, so they stay here (the
-- | descriptor type `GenKind` itself, and the engine, live in `Reef.Gen`).
genLabel :: GenKind -> String
genLabel = case _ of
  GNotes -> "NOTES"
  GGate -> "GATE"
  GSkip -> "SKIP"
  GGlide -> "GLIDE"
  GLen -> "LEN"
  GRatchet -> "RATCHET"
  GHeads -> "HEADS"
  GTransp -> "INT"
  GPattern -> "PATTERN"
  GSpeed -> "SPEED"
  GKey -> "KEY · SCALE"
  GVel -> "VELOCITY"

genSub :: GenKind -> String
genSub = case _ of
  GNotes -> "pitch · Marbles"
  GGate -> "rest a step"
  GSkip -> "drop a step"
  GGlide -> "tie / slew"
  GLen -> "note length"
  GRatchet -> "retrigger roll"
  GHeads -> "voice combination"
  GTransp -> "per-head interval"
  GPattern -> "access pattern"
  GSpeed -> "voice speed"
  GKey -> "fifths · mode · degree"
  GVel -> "accent drift"

-- | DOM id of the Marbles X-Y pad, shared by the view (the element) and the
-- | handler (which looks it up to read pointer position).
marblesPadId :: String
marblesPadId = "tf-marbles-xy"

-- | DOM id of the REPLAY timeline, so a region drag can read the pointer's
-- | normalised X within it (via `Pointer.padNorm`) and map straight to a time.
replayTimelineId :: String
replayTimelineId = "tf-replay-timeline"

-- | The Odonus component's child-component slots. One entry so far: the shared
-- | Hylograph Select widget driving the KEY pane's SCALE picker. The whole view
-- | tree carries this row (concrete, not `()`), so any further shared widget is a
-- | one-line addition here rather than a tree-wide retype.
type Slots = ( scaleSelect :: Select.Slot Unit )

-- | **Odonus's STAGE** — the same mode axis Vetula has (`Vetula.App.Stage`),
-- | minus Hunt: Vetula is the harmonic authority, so Odonus has nothing to hunt.
-- | Two stages, and the names are shared across both machines deliberately.
-- |
-- |   * `Perform` — the instrument: scope · playheads · grid · generate · params.
-- |   * `Review` — the capture surface given the whole window, where a phrase is
-- |     big enough to pick out and lift into the clip library.
-- |
-- | A stage is what you're LOOKING AT, not what's running: the generator keeps
-- | generating in both, which is the whole point when you're listening for a bit
-- | worth lifting. The transport lives in the shell's top nav.
-- |
-- | This was `VPanels | VFull` (and `VLive | VReplay` before that). `VFull` was
-- | honest about being a size and so gave the surface no NAME — which is why the
-- | toggle read as `⛶ full` while Vetula called the same thing REPLAY. Naming it
-- | REVIEW on both machines fixes that: a verb, in the same mood as PERFORM, for
-- | a surface you go to in order to work, not to read an archive.
data Stage = Perform | Review

derive instance eqStage :: Eq Stage

-- | The URL segments for a stage (`Triggerfish.Route`). Odonus owns this
-- | vocabulary; the shell carries the segments opaquely.
stagePath :: Stage -> Array String
stagePath = case _ of
  Perform -> [ "perform" ]
  Review -> [ "review" ]

-- | The inverse. `Nothing` for anything unrecognised, so a stale or hand-typed
-- | URL switches machine and leaves the stage alone rather than guessing.
stageFromPath :: Array String -> Maybe Stage
stageFromPath = case _ of
  [ "perform" ] -> Just Perform
  [ "review" ] -> Just Review
  _ -> Nothing

-- | Which grid the MidiFighter Twister's 16 rotaries currently drive (bank 1). A
-- | PUSH switch selects it (row 1 = the four cell VALUE grids, row 2 = the three
-- | cell BOOLEAN grids + the MACRO pane); every rotary then edits that grid.
-- | NOTE/LEN/RATCHET/VEL set a per-cell value; GATE/SKIP/GLIDE set a per-cell
-- | boolean (right = on); MACRO maps the 16 rotaries onto the pane's global knobs
-- | (octave/degree/marbles, generation, feel) rather than per-cell.
data TwisterField
  = FNote | FLen | FRatchet | FVel
  | FGate | FSkip | FGlide
  | FMacro

derive instance eqTwisterField :: Eq TwisterField

twisterFieldLabel :: TwisterField -> String
twisterFieldLabel = case _ of
  FNote -> "NOTE"
  FLen -> "LEN"
  FRatchet -> "RATCHET"
  FVel -> "VEL"
  FGate -> "GATE"
  FSkip -> "SKIP"
  FGlide -> "GLIDE"
  FMacro -> "MACRO"

-- | One polyphonic instrument the rack can drive, and the allocator state that
-- | belongs to it.
-- |
-- | Separate states rather than one, because the instruments do not share
-- | anything: a note on Rings takes no oscillator away from the Saïch. What IS
-- | shared is per-instrument — several Odonus heads routed to the same module
-- | compete for its voices — which is why the state hangs off the instrument
-- | and not off the route.
type PolyInst =
  { inst :: RM.InstrumentId
  , rig :: Poly.Rig
  , voices :: RV.Voices
  }

type State =
  { odo :: M.Odonus
  , sounding :: Sounding     -- the ONE transport value (control-surface MISU refactor):
                             -- Silent = stopped, Local = play local Web-MIDI, Rig = rig
                             -- authoritative (muted locally). Replaces running/master/audible.
  -- Per-head rig routing (4 entries) + whether its config modal is open.
  , dragging :: Maybe DragState
  , dragSub :: Maybe H.SubscriptionId
  , notes :: Array NoteEvent
  , logbook :: Logbook            -- always-on performance capture (#151)
  , stage :: Stage                -- Perform (the instrument) | Review (the capture surface)
  , selEuclid :: Maybe Int        -- which voice's Euclid ring is selected for arrow-key
                                  -- editing (`Triggerfish.Ui.Euclid`); Nothing = none.
                                  -- Selection-before-editing, as in Selene: the ring is
                                  -- read at a glance and changed deliberately.
  , navScenes :: Boolean          -- Odonus's secondary-nav scene menu open?
  , playing :: Maybe PlayState    -- a REPLAY loop in flight (Nothing = not replaying)
  , regionDrag :: Maybe RegionDrag  -- a loop-region resize/slide in progress
  , contextOpen :: Boolean          -- REPLAY control card: harmonic-context panel open
  , zoom :: Zoom                    -- REPLAY: how much of the take the surface shows
  , clips :: Array MidiClip         -- captured clips (shared library, #27), newest-first
  , twisterField :: TwisterField    -- which cell attribute the Twister's rotaries drive (bank 1)
  , binnacle :: Maybe Binnacle
  , nowMicros :: Number
  -- Every MIDI output port, plus the routing table saying which of them each
  -- head uses. Replaces the pair of hardcoded handles (IAC + FH-2) that shipped
  -- earlier today: routing is DATA now, resolved per leg at emit time, so a head
  -- can fan out to any number of destinations on any number of devices.
  -- See Triggerfish.Routing.Model.
  , outs :: RO.Outs
  , routing :: RM.Table
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , scenes :: Array Scene
  , sceneNameInput :: String  -- the name typed in the SCENES form for the next capture
  , publishMsg :: Maybe String  -- transient status from a publish-scene-to-Amphora click
  , stepDiv :: Int          -- global clock divider (1=1/16 .. 16=whole note)
  , headNote :: Array (Maybe Int)  -- the held/sounding MIDI note per head (4)
  -- Voice allocation for any DPoly leg — one entry per instrument the rack can
  -- drive, because two of them are two independent allocators: a note on Rings
  -- takes nothing away from the Saïch. `rig` is where the instrument reaches
  -- and how to correct its pitch (tables fetched from Amphora at startup, all
  -- Nothing until they arrive, which plays at nominal 1 V/oct rather than
  -- refusing); `voices` is that allocator's live state.
  , polys :: Array PolyInst
  -- The Rample's ONE allocator. Not in `polys` because that array is keyed by
  -- `RM.InstrumentId` and driven over the ES-9 socket; the Rample is reached by
  -- MIDI and has no rig entry. Same shape of state, different wire.
  , rampleVoices :: RV.Voices
  , polyNote :: Maybe String  -- why poly is degraded, if it is
  , swing :: Number          -- groove: fraction of a step that off-beats lag (0..0.6)
  , velHumanize :: Int       -- velocity jitter range ± (0 = dead-flat)
  , gen :: Array GenSource    -- the randomisation matrix — one source per aspect
  , genSpread :: Number       -- Marbles X-Y pad: spread ∈ [0,1] (NOTES source)
  , genBias :: Number         -- Marbles X-Y pad: bias ∈ [0,1] (NOTES source)
  , genSeed :: Marbles.Seed   -- the shared PRNG every source draws from
  , genFrozen :: Boolean      -- all generation paused (config kept) — synced via SetFrozen
  , pending :: Array PendingInput  -- lockstep (P4c): tick-tagged inputs awaiting their step
  , nextModelStep :: Int     -- lockstep (P5): the absolute model step the NEXT Step loop will
                             -- emit. `odo`/`gen`/`genSeed` are exactly that step's input, so a
                             -- handoff stamped with this step (`reef-sim-at`) plays the pushed
                             -- state on the SAME absolute step on the BEAM — phase-zero by
                             -- construction (no more +1-step / +1-beat flam on Push).
  , collapsed :: Array String  -- panel labels currently collapsed (accordion)
  , lastTap :: String          -- last toggle target (debounce the double-dispatch)
  , lastTapMicros :: Number
  -- One-shot connect-time rig reconcile: false until the first Frame fires a
  -- `hush` to the rig (clears any voices orphaned by a PREVIOUS session's push —
  -- a reload starts with nothing armed, so the rig should start silent, and the
  -- arm/disarm rig-stop is edge-triggered with no "leaving Rig" edge on reload).
  -- Hidden from the user; arming re-pushes. Odonus hosts it as the always-first-
  -- mounted instrument (the shell owns no rig socket).
  , reconciled :: Boolean
  -- The harmony feeds this page last took from the rig's `odonus/feeds`
  -- (Triggerfish.Odonus.Feeds): applied here unless the rig is sounding,
  -- whose own moves then arrive as reef-inputs.
  , feedsSeen :: Route.Feeds
  -- Odonus's patterns sampled by the rig for this page to play itself
  -- (Triggerfish.Odonus.Samples): what it holds, what it last asked for, and
  -- the sample last applied (applied only when it changes, as the rig does).
  , samples :: Samples.Samples
  , sampleAsked :: Maybe { key :: String, from :: Int }
  , lastSample :: Maybe String
  -- The unified glyph-chip PRESET bank (docs/DESIGN-scene-modal.md): captured live
  -- patches, anonymous or named, freely intermixed — distinct from the named SCENE
  -- library. `identity` is the parked preset's text (the chip glyph; ghosts when the
  -- live patch diverges from it); `lastChip` guards the Frame → shell status-board
  -- emit so it only raises on change.
  , presets :: Array Preset
  , identity :: Maybe String
  , lastChip :: Maybe ChipView
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | RigFrame String         -- a frame from the rig: an `odonus` move's tagged gestures to follow
  -- every output port, status line
  | MidiReady RO.Outs String
  | RoutingStored   -- another tab saved the routing table (the dashboard's router)
  | ToggleGlide Int
  | ToggleGate Int
  | ToggleSkip Int
  | SetAllNotes Int
  | SeedMelody              -- fill cells with a random in-harmony melodic line
  | StampForm Int           -- lay a named figure (Odonus.Forms) into the note field
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  | SetHeadDir Int Int      -- head, direction (0 fwd / 1 back / 2 pend) — radio
  | SetHeadSpeed Int Int    -- head, speedIx — two-row speed radio
  | NudgeHeadPulses Int Int  -- head, ±delta (k) — relative Euclid edit
  | NudgeHeadSteps Int Int   -- head, ±delta (n) — relative Euclid edit
  | SelectEuclid Int         -- click a voice's Euclid ring: select it for arrow-key editing
  | DeselectEuclid Int       -- that ring lost focus (a click anywhere else): drop the selection
  | EuclidKey KeyboardEvent  -- a keystroke on the focused ring; arrows nudge k / n
  | UnifyHeads
  | PhaseShift Int          -- Reichian PHASE ±: rotate the whole canon
  | CycleScaleType Int
  | PickScale String          -- jump to a named preset scale (the Select widget)
  | ToggleDist
  | SetRoot Int
  | SetOctave Int
  | SetDegShift Int
  | ToggleScaleNote Int
  | CaptureScene
  | SetSceneName String
  | PublishScene Int         -- publish scene i to the Amphora store (odonus-scene)
  | RecallScene Int
  | RecallGesture Int
  | DeleteScene Int
  | MarkNow                 -- flag "a good bit" at the current instant (logbook)
  | DeleteMark Int          -- drop a flagged instant
  | ClearLog                -- purge the whole logbook manually
  | SetStage Stage          -- switch stage: Perform (instrument) | Review (capture)
  | ToggleSceneMenu         -- secondary nav: open/close the scene menu
  | PlayRegion Int          -- start looping the region around mark i (REPLAY)
  | StopPlay                -- stop the REPLAY loop
  | RegionDown Int RegionEdge Int Int  -- grab a region: markIdx, edge, clientX, clientY
  | RegionMove Int Int      -- pointer moved during a region drag: clientX, clientY
  | RegionUp                -- release a region drag (click→play, or finalize resize)
  | SaveMarkScene Int       -- promote a mark's captured patch into the SCENES list
  | SaveMarkClip Int        -- lift a mark's region out as a captured clip (#151, R2d)
  | PlayClip Int            -- audition a captured clip (loops, like a region)
  | RenameClip Int String   -- rename a captured clip (commits on blur; persists)
  | DeleteClip Int          -- drop a captured clip
  | ToggleContext           -- REPLAY card: show/hide the active mark's harmonic context
  | SetZoom Zoom            -- REPLAY: whole / last N / crop to a loop
  | SetStepDiv Int
  | KnobDown KnobTarget Int
  | DragMove Int
  | DragEnd
  | ToggleGen GenKind          -- enable/disable a randomisation source
  | ToggleFreeze               -- pause / resume ALL generation (deferred-on-both)
  | MarblesPad Int Int Int     -- X-Y pad: clientX, clientY, buttons (read sync)
  | MarblesRoll                -- one-shot: regenerate all cell notes now
  | ReseedTo Int               -- pin the PRNG seed to a known value (golden tests):
                               -- sets genSeed = seedFrom n; the next Push hands the
                               -- fixed seed to the rig for a reproducible take
  | CollapsePanel String       -- fold a panel to a tab (idempotent)
  | ExpandPanel String         -- reopen a panel (idempotent)
  | PushToRig                  -- encode the whole Odonus record (Reef.Protocol)
                               -- and send it over the rig WS as `reef-odonus <json>`,
                               -- to run on the BEAM via the shared reef engine
  | HushRig                    -- send `hush` over the rig WS (stops the reef voice
                               -- the push started, plus everything else on the rig)
  | TwisterMsg Int Int Int     -- a raw MIDI message from the MidiFighter Twister
  -- VOICE config: which FH-2 polyenv envelopes each head fires.
                               -- (status, data1, data2). Bank 1: encoders 0..15 map
                               -- 1:1 onto the 16 cells — rotate sets the cell note
                               -- (absolute), push toggles SKIP. Decoded in the handler.


