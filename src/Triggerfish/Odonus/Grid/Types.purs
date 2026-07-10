-- | Shared types for the Odonus grid: the knob-drag target algebra, the
-- | component State, and the Action set. Held low in the module DAG so every
-- | view module can refer to them without a cycle.
module Triggerfish.Odonus.Grid.Types
  ( KnobTarget(..)
  , targetRange
  , applyTarget
  , DragState
  , NoteEvent
  , PendingInput
  , Scene
  , Chunk
  , Mark
  , RegionEdge(..)
  , RegionDrag
  , replayTimelineId
  , Logbook
  , PlayState
  , PlaySource(..)
  , Clip
  , module Reef.Gen
  , genLabel
  , genSub
  , marblesPadId
  , SourceTag(..)
  , OdonusView(..)
  , State
  , Action(..)
  , Slots
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe)
import Halogen as H
import Hylograph.Halogen.UI.Select as Select
import Reef.Input as RI
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Marbles as Marbles
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
  HeadDiv _ -> { lo: 0, hi: 16 }
  HeadEStep _ -> { lo: 1, hi: 16 }
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

-- | One emitted note in the scrolling monitor / logbook. `fireUnixMicros` is the
-- | wall-clock instant it sounds; the river positions it by how long ago that was
-- | (so the visual onset lands exactly on the audio onset). `vel`/`gateMs` are
-- | carried so REPLAY (#151) can re-emit the note faithfully — velocity dynamics
-- | and note length are part of "the good bit". The scope ignores them.
type NoteEvent = { pitch :: Int, headIdx :: Int, fireUnixMicros :: Number, vel :: Int, gateMs :: Number }

-- | A REPLAY loop in progress (#151, R2b): the region bounds in recording time,
-- | which mark it came from, the perf-clock instant the NEXT loop iteration
-- | should be scheduled at, and the 0..1 playhead position for the view.
-- | What the REPLAY loop is currently playing: a region around a mark on the
-- | timeline, or a captured clip. The scheduler is source-agnostic (it reads the
-- | rebased `events`); the source only decides what the view highlights.
data PlaySource = FromRegion Int | FromClip Int

derive instance eqPlaySource :: Eq PlaySource

type PlayState =
  { source :: PlaySource
  , events :: Array NoteEvent  -- the loop's notes, rebased to [0, lenMicros)
  , lenMicros :: Number        -- loop length; the notes repeat every lenMicros
  , fromMicros :: Number       -- region bounds on the timeline (FromRegion playhead only)
  , toMicros :: Number
  , loopStartMs :: Number      -- perf-now ms that the loop's phase-0 aligns to
  , scheduledUntilMs :: Number  -- watermark: notes are queued up to this perf-now ms
  , playheadFrac :: Number
  }

-- | A captured performance clip (#151, R2d): a span of the logbook lifted out as
-- | a self-contained, replayable artefact — its notes copied and rebased to zero
-- | (so it survives the buffer reset and can be scheduled anywhere), its length,
-- | and the Odonus patch that made it (harmonic context / promote-to-scene). The
-- | durable harvest, as against the ephemeral logbook it came from.
type Clip =
  { name :: String
  , events :: Array NoteEvent  -- rebased to [0, lenMicros)
  , lenMicros :: Number
  , patch :: String            -- the Odonus patch (Lepidoptera text) at capture
  }

-- | A saved whole-Odonus setting under a name — the recallable PRESET and the
-- | unit of composition (sequencing scenes builds flowing fugues with key
-- | changes and voices dropping in and out). The setting is stored as its
-- | Lepidoptera `text` (the full authored patch rendered to eDSL — the same
-- | canonical, transferable form the Tidal page shows), parsed back on recall.
-- | Lossless: the A3 round-trip is byte-stable.
type Scene = { name :: String, text :: String }

-- | One frozen span of the always-on logbook: a chunk of captured notes with
-- | its time bounds. Chunking keeps the live append O(current chunk) instead of
-- | O(whole session), and makes retention a matter of dropping whole chunks.
type Chunk = { fromMicros :: Number, toMicros :: Number, events :: Array NoteEvent }

-- | A flagged good bit: WHEN it happened (wall clock + the absolute Link `beat`),
-- | the loop window `from`/`to` (recording micros — bar-aligned at capture, then
-- | freely draggable/resizable), and the Odonus `patch` (Lepidoptera text) live
-- | at that instant. So a mark carries the notes that came out (via its span in
-- | the note stream), an editable loop region, and the machine state that made it.
type Mark = { atMicros :: Number, beat :: Number, from :: Number, to :: Number, patch :: String }

-- | Which part of a loop region a drag grabbed: its left edge (move the start),
-- | right edge (move the end), or body (slide the whole window).
data RegionEdge = EdgeFrom | EdgeTo | EdgeBody

derive instance eqRegionEdge :: Eq RegionEdge

-- | A region drag in progress. `grabMicros` is the pointer position (in recording
-- | micros) where the grab began; `moved` distinguishes a resize/slide from a bare
-- | click (a click on the body starts playback instead).
type RegionDrag =
  { markIdx :: Int, edge :: RegionEdge, grabMicros :: Number
  , startFrom :: Number, startTo :: Number, moved :: Boolean
  }

-- | The always-on performance logbook (#151): the scope's note stream WITHOUT
-- | the ~8s prune, so what actually happened survives. The rig is always
-- | capturing — no arm. `live` is the growing current chunk (newest-first, like
-- | `notes`); once it fills, it freezes into `chunks` (newest-first) and a new
-- | live chunk starts. `marks` are wall-clock instants the performer tapped to
-- | flag a good bit — the seam to lift a span into a scene later. Retention: on
-- | each freeze, chunks older than the window are dropped UNLESS a mark falls
-- | within them ("the recent past plus anything I flagged"). Frontend-only.
type Logbook =
  { live :: Array NoteEvent    -- current growing chunk, newest-first
  , liveFrom :: Number         -- wall-clock start of the live chunk
  , chunks :: Array Chunk      -- frozen chunks, newest-first
  , marks :: Array Mark        -- flagged good bits (instant + patch), newest-first
  }

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

-- | Which pitch source drives the quantizer — the KEY pane's top-level choice.
-- | `SScale` snaps to the scale; `SVetula` to a followed Vetula voice. Derived
-- | from `chord.on` + `follow` (`Triggerfish.Odonus.Patch.sourceOf`).
data SourceTag = SScale | SVetula

derive instance eqSourceTag :: Eq SourceTag

-- | Which surface the Odonus instrument shows: the LIVE performance panels, or
-- | the REPLAY editor over the logbook (#151). A tab within Odonus — replay
-- | reviews Odonus's own capture, and lives where that data + the emit path are.
data OdonusView = VLive | VReplay

derive instance eqOdonusView :: Eq OdonusView

type State =
  { odo :: M.Odonus
  , sounding :: Sounding     -- the ONE transport value (control-surface MISU refactor):
                             -- Silent = stopped, Local = play local Web-MIDI, Rig = rig
                             -- authoritative (muted locally). Replaces running/master/audible.
  , dragging :: Maybe DragState
  , dragSub :: Maybe H.SubscriptionId
  , notes :: Array NoteEvent
  , logbook :: Logbook            -- always-on performance capture (#151)
  , view :: OdonusView            -- LIVE panels vs the REPLAY editor over the logbook
  , playing :: Maybe PlayState    -- a REPLAY loop in flight (Nothing = not replaying)
  , regionDrag :: Maybe RegionDrag  -- a loop-region resize/slide in progress
  , contextOpen :: Boolean          -- REPLAY control card: harmonic-context panel open
  , clips :: Array Clip             -- captured performance clips (#151, R2d), newest-first
  , binnacle :: Maybe Binnacle
  , nowMicros :: Number
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , scenes :: Array Scene
  , sceneNameInput :: String  -- the name typed in the SCENES form for the next capture
  , chain :: Boolean        -- auto-advance scenes at bar boundaries
  , sceneIx :: Int          -- current scene in the chain
  , sceneBarAnchor :: Int   -- bar at which the current scene started
  , barsPerScene :: Int
  , stepDiv :: Int          -- global clock divider (1=1/16 .. 16=whole note)
  , headNote :: Array (Maybe Int)  -- the held/sounding MIDI note per head (4)
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
  -- The live Vetula→Odonus follow bridge. `voiceChords` is the latest poll of
  -- the shell (each Odonus-bound Vetula voice's current block chord, keyed by id);
  -- `follow` selects one of those ids (or none), whose chord the quantiser snaps to.
  , voiceChords :: Array { id :: Int, pcs :: Array Int }
  , follow :: Maybe Int
  -- The chosen pitch SOURCE (the KEY pane radio). An explicit intent, NOT derived
  -- from the overlay state — so "Vetula selected but no signal yet" is a real,
  -- selectable state (the sub-section then shows it's waiting). `chord.on` still
  -- tracks whether a live chord is actually driving the snap.
  , source :: SourceTag
  -- One-shot connect-time rig reconcile: false until the first Frame fires a
  -- `hush` to the rig (clears any voices orphaned by a PREVIOUS session's push —
  -- a reload starts with nothing armed, so the rig should start silent, and the
  -- arm/disarm rig-stop is edge-triggered with no "leaving Rig" edge on reload).
  -- Hidden from the user; arming re-pushes. Odonus hosts it as the always-first-
  -- mounted instrument (the shell owns no rig socket).
  , reconciled :: Boolean
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleGlide Int
  | ToggleGate Int
  | ToggleSkip Int
  | SetAllNotes Int
  | SeedMelody              -- fill cells with a random in-harmony melodic line
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  | SetHeadDir Int Int      -- head, direction (0 fwd / 1 back / 2 pend) — radio
  | SetHeadSpeed Int Int    -- head, speedIx — two-row speed radio
  | NudgeHeadPulses Int Int  -- head, ±delta (k) — Euclidean-circle corner clicker (relative)
  | NudgeHeadSteps Int Int   -- head, ±delta (n) — Euclidean-circle corner clicker (relative)
  | UnifyHeads
  | PhaseShift Int          -- Reichian PHASE ±: rotate the whole canon
  | CycleScaleType Int
  | PickScale String          -- jump to a named preset scale (the Select widget)
  | ToggleDist
  | ToggleChord
  | SetSource SourceTag       -- pick the quantizer's pitch source (KEY pane)
  | SetFollow (Maybe Int)    -- follow a Vetula Odonus-bound voice by id (Nothing = free)
  | SetRoot Int
  | SetOctave Int
  | SetDegShift Int
  | ToggleScaleNote Int
  | CaptureScene
  | SetSceneName String
  | RecallScene Int
  | RecallGesture Int
  | DeleteScene Int
  | MarkNow                 -- flag "a good bit" at the current instant (logbook)
  | DeleteMark Int          -- drop a flagged instant
  | ClearLog                -- purge the whole logbook manually
  | SetView OdonusView      -- switch the Odonus surface (LIVE / REPLAY)
  | PlayRegion Int          -- start looping the region around mark i (REPLAY)
  | StopPlay                -- stop the REPLAY loop
  | RegionDown Int RegionEdge Int Int  -- grab a region: markIdx, edge, clientX, clientY
  | RegionMove Int Int      -- pointer moved during a region drag: clientX, clientY
  | RegionUp                -- release a region drag (click→play, or finalize resize)
  | SaveMarkScene Int       -- promote a mark's captured patch into the SCENES list
  | SaveMarkClip Int        -- lift a mark's region out as a captured clip (#151, R2d)
  | PlayClip Int            -- audition a captured clip (loops, like a region)
  | DeleteClip Int          -- drop a captured clip
  | ToggleContext           -- REPLAY card: show/hide the active mark's harmonic context
  | ToggleChain
  | BumpBars Int
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
