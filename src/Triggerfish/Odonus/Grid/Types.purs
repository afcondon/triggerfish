-- | Shared types for the Odonus grid: the knob-drag target algebra, the
-- | component State, and the Action set. Held low in the module DAG so every
-- | view module can refer to them without a cycle.
module Triggerfish.Odonus.Grid.Types
  ( KnobTarget(..)
  , targetRange
  , applyTarget
  , DragState
  , NoteEvent
  , Scene
  , GenKind(..)
  , GenSource
  , genKinds
  , genLabel
  , genSub
  , genDefaultRate
  , genDefaultAmt
  , rateMax
  , periodOf
  , toggleGen
  , setRate
  , setAmt
  , marblesPadId
  , State
  , Action(..)
  ) where

import Prelude

import Data.Array (length)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe)
import Data.Number (pow)
import Halogen as H
import Triggerfish.Odonus.Model as M
import Triggerfish.Odonus.Marbles as Marbles
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
  ChordStep -> M.setChordPeriod v
  SwingAmt -> identity     -- handled at State level in DragMove, not on Odonus
  VelHuman -> identity      -- handled at State level in DragMove, not on Odonus
  GenRate _ -> identity   -- handled at State level in DragMove, not on Odonus
  GenAmt _ -> identity     -- handled at State level in DragMove, not on Odonus

type DragState = { target :: KnobTarget, startY :: Int, startVal :: Int }

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

-- | A randomisation aspect: one independent slow-drift source. Each picks a
-- | random element of its domain when it fires and mutates it by one notch —
-- | a steady, low-probability evolution rather than a one-shot scramble.
-- | (OFFSET and head LENGTH are deliberately excluded — those get direct
-- | Reichian phase controls instead.)
data GenKind
  = GNotes      -- reroll one cell's note from the Marbles Beta distribution
  | GGate       -- occasionally rest a step (biased toward mostly-gated)
  | GSkip       -- occasionally drop a step (biased toward few skips)
  | GGlide      -- occasionally tie/slew a step (biased toward few glides)
  | GLen        -- drift one cell's note length ±1
  | GRatchet    -- occasionally ratchet a step (biased toward few rolls), like GLen
  | GHeads      -- walk the active-playhead combination (one bit on the 4-cube)
  | GTransp     -- nudge one head's scalar transpose
  | GPattern    -- advance one head's access pattern
  | GSpeed      -- nudge one head's speed
  | GKey        -- shift key by a fifth, change mode, or toggle a scale note

derive instance eqGenKind :: Eq GenKind

genKinds :: Array GenKind
genKinds = [ GNotes, GGate, GSkip, GGlide, GLen, GRatchet, GHeads, GTransp, GPattern, GSpeed, GKey ]

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

-- | One source's stored config: enabled, a rate index 0..`rateMax` (→ firing
-- | period via `periodOf`), and an `amt` 0..100 giving the mutation DEPTH —
-- | how big each change is when the source fires (a small constant nudge vs a
-- | proper shake-up). The two axes are independent: how OFTEN, and how MUCH.
type GenSource = { kind :: GenKind, on :: Boolean, rate :: Int, amt :: Int }

-- | A source's initial rate index. LEN drifts more freely (a lower index = a
-- | shorter period = more frequent) since note-length changes read as phrasing,
-- | not chaos; everything else starts conservative.
genDefaultRate :: GenKind -> Int
genDefaultRate = case _ of
  GLen -> 72
  GRatchet -> 72
  _ -> 96

-- | A source's initial mutation depth (0..100). Chosen so a single source,
-- | turned on alone, makes an audible difference — TRANSP needs more depth to
-- | clear re-quantization, KEY stays gentle (mostly fifths).
genDefaultAmt :: GenKind -> Int
genDefaultAmt = case _ of
  GNotes -> 20
  GTransp -> 40
  GKey -> 25
  GLen -> 25
  GRatchet -> 25
  _ -> 30

-- | The rate-index resolution. Drag the bare-number control across this range.
rateMax :: Int
rateMax = 200

-- | Map a rate index to a firing PERIOD in model steps — the bare number the
-- | control shows. Geometric from 1 (every step, chaos) up to ~16384 (a change
-- | roughly every few thousand steps, i.e. rare drift). One change per N steps.
periodOf :: Int -> Int
periodOf r =
  let rr = if r < 0 then 0 else if r > rateMax then rateMax else r
  in max 1 (round (pow 16384.0 (toNumber rr / toNumber rateMax)))

-- | Flip a source's enable.
toggleGen :: GenKind -> Array GenSource -> Array GenSource
toggleGen k = map \s -> if s.kind == k then s { on = not s.on } else s

-- | Set a source's rate index (from a drag on its bare-number control).
setRate :: GenKind -> Int -> Array GenSource -> Array GenSource
setRate k v = map \s -> if s.kind == k then s { rate = v } else s

-- | Set a source's mutation depth (from a drag on its AMT control).
setAmt :: GenKind -> Int -> Array GenSource -> Array GenSource
setAmt k v = map \s -> if s.kind == k then s { amt = v } else s

-- | DOM id of the Marbles X-Y pad, shared by the view (the element) and the
-- | handler (which looks it up to read pointer position).
marblesPadId :: String
marblesPadId = "tf-marbles-xy"

type State =
  { odo :: M.Odonus
  , running :: Boolean       -- the ARM/cue flag (sticky); sounds only when master too
  , master :: Boolean        -- the shell's master transport (pushed via SetMaster)
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
  , collapsed :: Array String  -- panel labels currently collapsed (accordion)
  , lastTap :: String          -- last toggle target (debounce the double-dispatch)
  , lastTapMicros :: Number
  -- The live Vetula→Odonus follow bridge. `voiceChords` is the latest poll of
  -- the shell (each Odonus-bound Vetula voice's current block chord, keyed by id);
  -- `follow` selects one of those ids (or none), whose chord the quantiser snaps to.
  , voiceChords :: Array { id :: Int, pcs :: Array Int }
  , follow :: Maybe Int
  }

data Action
  = Initialize
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ToggleRun
  | ToggleGlide Int
  | ToggleGate Int
  | ToggleSkip Int
  | SetAllNotes Int
  | SeedMelody              -- fill cells with a random in-harmony melodic line
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  | SetHeadDir Int Int      -- head, direction (0 fwd / 1 back / 2 pend) — radio
  | UnifyHeads
  | PhaseShift Int          -- Reichian PHASE ±: rotate the whole canon
  | CycleScaleType Int
  | ToggleDist
  | ToggleChord
  | ChordRoll
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
  | CollapsePanel String       -- fold a panel to a tab (idempotent)
  | ExpandPanel String         -- reopen a panel (idempotent)
