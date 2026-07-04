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
  , module Reef.Gen
  , genLabel
  , genSub
  , marblesPadId
  , SourceTag(..)
  , State
  , Action(..)
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe)
import Halogen as H
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
  | ChordStep        -- chord-progression clock: steps per chord
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
  ChordStep -> { lo: M.chordPeriodMin, hi: M.chordPeriodMax }
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
  ChordStep -> M.setChordPeriod v
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

-- | One emitted note in the scrolling monitor. `fireUnixMicros` is the
-- | wall-clock instant it sounds; the river positions it by how long ago
-- | that was (so the visual onset lands exactly on the audio onset).
type NoteEvent = { pitch :: Int, headIdx :: Int, fireUnixMicros :: Number }

-- | A saved whole-Odonus setting under a name — the recallable PRESET and the
-- | unit of composition (sequencing scenes builds flowing fugues with key
-- | changes and voices dropping in and out). The setting is stored as its
-- | Lepidoptera `text` (the full authored patch rendered to eDSL — the same
-- | canonical, transferable form the Tidal page shows), parsed back on recall.
-- | Lossless: the A3 round-trip is byte-stable.
type Scene = { name :: String, text :: String }

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
  GTransp -> "TRANSP"
  GPattern -> "PATTERN"
  GSpeed -> "SPEED"
  GKey -> "KEY · SCALE"

genSub :: GenKind -> String
genSub = case _ of
  GNotes -> "pitch · Marbles"
  GGate -> "rest a step"
  GSkip -> "drop a step"
  GGlide -> "tie / slew"
  GLen -> "note length"
  GRatchet -> "retrigger roll"
  GHeads -> "voice combination"
  GTransp -> "scalar transpose"
  GPattern -> "access pattern"
  GSpeed -> "voice speed"
  GKey -> "fifths · mode · degree"

-- | DOM id of the Marbles X-Y pad, shared by the view (the element) and the
-- | handler (which looks it up to read pointer position).
marblesPadId :: String
marblesPadId = "tf-marbles-xy"

-- | Which pitch source drives the quantizer — the KEY pane's top-level choice.
-- | `SScale` snaps to the scale; `SChord` to the internal McMullen progression;
-- | `SVetula` to a followed Vetula voice. Derived from `chord.on` + `follow`
-- | (`Triggerfish.Odonus.View.Key.sourceTagOf`), consistent with the
-- | `PitchSource` print/parse model.
data SourceTag = SScale | SChord | SVetula

derive instance eqSourceTag :: Eq SourceTag

type State =
  { odo :: M.Odonus
  , sounding :: Sounding     -- the ONE transport value (control-surface MISU refactor):
                             -- Silent = stopped, Local = play local Web-MIDI, Rig = rig
                             -- authoritative (muted locally). Replaces running/master/audible.
  , dragging :: Maybe DragState
  , dragSub :: Maybe H.SubscriptionId
  , notes :: Array NoteEvent
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
  | SetHeadPulses Int Int   -- head, pulses (k) — Euclidean-circle corner clicker
  | SetHeadSteps Int Int    -- head, esteps (n) — Euclidean-circle corner clicker
  | UnifyHeads
  | PhaseShift Int          -- Reichian PHASE ±: rotate the whole canon
  | CycleScaleType Int
  | ToggleDist
  | ToggleChord
  | ChordRoll
  | SetSource SourceTag       -- pick the quantizer's pitch source (KEY pane)
  | SetFollow (Maybe Int)    -- follow a Vetula Odonus-bound voice by id (Nothing = free)
  | SetRoot Int
  | SetOctave Int
  | SetDegShift Int
  | ToggleScaleNote Int
  | CaptureScene
  | SetSceneName String
  | RecallScene Int
  | DeleteScene Int
  | ToggleChain
  | BumpBars Int
  | SetStepDiv Int
  | KnobDown KnobTarget Int
  | DragMove Int
  | DragEnd
  | ToggleGen GenKind          -- enable/disable a randomisation source
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
