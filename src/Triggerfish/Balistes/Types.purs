-- | Triggerfish.Balistes.Types — the stable DAG root for the Balistes component:
-- | the `State` record, the `Action` set, the small closed tag types they lean on
-- | (`Active`, `NoteRef`, `DragKind`, `KnobTarget`), and the domain constants +
-- | pure vocab (`targetRange`, `knobValue`, `applyKnob`, `drumChannel`, …).
-- |
-- | This module owns no rendering and no state machine — it's the shared
-- | vocabulary both the view modules (`Balistes.View.*`, `Balistes.Widgets`,
-- | `Balistes.Snapshot`) and the controller (`Balistes.Component`) import, so
-- | neither imports the other. Mirrors `Odonus.Grid.Types`.
module Triggerfish.Balistes.Types
  ( Flash
  , KnobTarget(..)
  , targetRange
  , knobValue
  , applyKnob
  , Active(..)
  , NoteRef(..)
  , DragKind(..)
  , Drag
  , State
  , Action(..)
  , activePattern
  , selectedPattern
  , patternAt
  , rhythmEntries
  , rigUrl
  , gridCfg
  , stepsPerBar
  , midiPortName
  , drumChannel
  , cycleSteps
  , editVel
  , flashWindow
  , padId
  , eqTrigName
  , jackNoteOf
  ) where

import Prelude

import Data.Array (mapMaybe, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..))
import Binnacle as Binnacle
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler
import Halogen as H
import Reef.Balistes.Input as RBI
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Glyph as G
import Triggerfish.Preset (Preset)
import Triggerfish.Balistes.TriSnapshot (Brain, rhythmOfContent)
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Transport (Sounding)

-- | A recent hit, kept just long enough to flash the transport pilot lamps.
type Flash = { inst :: Int, accent :: Boolean, fireUnixMicros :: Number }

data KnobTarget = KDens Int | KRand | KPush Int | KOpen

-- | The value range each knob spans (so the drag scales correctly per target).
targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  KDens _ -> { lo: 0, hi: 255 }
  KRand -> { lo: 0, hi: 255 }
  KPush _ -> { lo: -50, hi: 50 }
  KOpen -> { lo: 0, hi: 255 }

knobValue :: KnobTarget -> M.Balistes -> Int
knobValue (KDens i) b = M.densityOf i b
knobValue KRand b = b.randomness
knobValue (KPush i) b = M.pushOf i b
knobValue KOpen b = M.openOf b

applyKnob :: KnobTarget -> Int -> M.Balistes -> M.Balistes
applyKnob (KDens i) v = M.setDensity i v
applyKnob KRand v = M.setRandomness v
applyKnob (KPush i) v = M.setPush i v
applyKnob KOpen v = M.setOpen v

-- | What the panel is currently playing — one drum-brain at a time (the tab
-- | bar's projection). `AGrids` is the generative MI-Grids morph engine (owns
-- | the CONTROL column); `AFixed i` is a literal rhythm from the library
-- | (`library !! i`), played verbatim; `ASelene` is the relocated POLYTRIG jack
-- | rack (browser-only) — named jacks + lane-spanning routes, all → ch 10.
data Active = AGrids | AFixed Int | ASelene

derive instance eqActive :: Eq Active

-- | Which MIDI note a note-drag edits: a Grids lane (0..3) or a fixed-pattern
-- | lane (`NFixed patternIx lane`).
data NoteRef = NGrids Int | NFixed Int Int

-- | A document-tracked drag turns a knob, subdivides a Grids cell into ratchets
-- | (`DCell lane step`), or nudges a lane's MIDI note (`DNote`). One plumbing.
data DragKind = DKnob KnobTarget | DCell Int Int | DNote NoteRef

type Drag = { kind :: DragKind, startY :: Int, startVal :: Int }

type State =
  { bal :: M.Balistes
  , sounding :: Sounding       -- the ONE transport value (MISU refactor): Silent | Local | Rig.
                               -- Replaces running/master/audible; `== Rig` also replaces `pushed`
                               -- (the rig voice is running iff we're rig-authoritative).
  , playStep :: Int
  -- the ABSOLUTE model step the current `bal` will next be played from (Grids
  -- mode). PushBalistes stamps the handoff with this so the rig holds the pushed
  -- state until the same step — the Odonus #57 phase-alignment, for Balistes.
  , nextModelStep :: Int
  -- tick-tagged gestures awaiting their model step (deferred-on-both lockstep):
  -- applied in the Step loop when step <= tick.index, on both runtimes.
  , pending :: Array { step :: Int, input :: RBI.BInput }
  , flash :: Array Flash
  , binnacle :: Maybe Binnacle.Binnacle
  , midiOut :: Maybe Midi.MidiOut
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , nowMicros :: Number
  , dragging :: Maybe Drag
  , dragSub :: Maybe H.SubscriptionId
  -- the PRESET bank (unified model, docs/DESIGN-scene-modal.md): a growing list,
  -- each preset a captured playing-state of ANY brain (Mutable / Grids / Tidal)
  -- rendered to text, anonymous (glyph-only) or named, freely intermixed. Capture
  -- DEDUPS by content. Capture is the `c` hotkey; recall/star/delete live on the
  -- status-board chip's menu. (The old per-machine SEQUENCE path was stripped once
  -- the rig-wide scene grid + macro-tidal lanes subsumed it — 2026-07-30.)
  , presets :: Array Preset
  -- the IDENTITY CHIP's parked content: the preset TEXT the machine is currently
  -- "on" (last recalled, or just captured). The chip renders its glyph SOLID while
  -- the live state still matches and GHOSTED once you diverge — the continuous
  -- dirty indicator (see docs/DESIGN-scene-modal.md). Transient (not persisted).
  , identity :: Maybe String
  -- the last chip-view raised to the shell's status board — bookkeeping so the
  -- Frame loop only re-raises `IdentityChanged` when the view actually changes.
  , lastChip :: Maybe G.ChipView
  -- the pattern family: which one is playing, and the fixed-rhythm library.
  , active :: Active
  -- an EPHEMERAL fixed rhythm played from a recalled `TSFixed` snapshot: when
  -- `Just`, it overrides the `AFixed` library index (played read-only), so
  -- recalling a snapshot never mutates the library. Cleared on any deliberate
  -- tab / library selection. See `activePattern`.
  , scratchFixed :: Maybe P.FixedPattern
  -- EDIT mode for a fixed rhythm: reveal all 16 lanes (greyed where empty) so
  -- you can add voices; cells are click-to-toggle either way.
  , editing :: Boolean
  , presetsOpen :: Boolean       -- the bank modal
  -- Bank filter: `Nothing` shows every brain's entries, `Just b` narrows to one.
  -- Presentation only — the collection stays whole (the point of the fold).
  , bankFilter :: Maybe Brain
  -- macro-tidal: this machine's arrangement lane, MIRRORED from the shell (which
  -- owns and persists it). Edits raise back up; the shell pushes changes down.
  , lane :: String
  , laneReadout :: String        -- the lane's live current-token label
  , fixedSel :: Int              -- the SELECTED RYTM rhythm (see `selectedPattern`);
                                 -- independent of whether RYTM is the live brain
  -- the cell the NOTE inspector is editing (lane, step) on the active rhythm.
  , selected :: Maybe { lane :: Int, step :: Int }
  -- the POLYTRIG jack rack (SELENE DRUMS tab) — browser-only, no reef path.
  , trig :: M.TrigBank
  -- transient status for the "publish to Amphora" action on the active rhythm.
  , publishMsg :: Maybe String
  }

data Action
  = Initialize
  -- The preset modal: the rhythm library (was a 14-chip wall on the surface) and
  -- the snapshot bank (was reachable only from the shell's status-board chip
  -- menu, i.e. not from inside Balistes at all). Recall/star/delete mirror the
  -- shell's `RecallSlot`/`StarSlot`/`DeleteSlot` queries at the local altitude.
  | BankNow                   -- bank the live state under its content glyph (the `c` hotkey, as a click)
  | BankBrain Active          -- bank ONE brain's state, sounding or not (its band's glyph)
  | SetLane String            -- edit this machine's macro-tidal arrangement lane
  | InsertLaneToken String    -- append a pattern name to the lane (click-to-assemble)
  | OpenPresets
  | ClosePresets
  | RecallPreset Int
  | StarPreset Int
  | DeletePreset Int
  -- Name a banked snapshot. NOT merely cosmetic: naming is PROMOTION — an
  -- anonymous, glyph-identified capture becomes a curated artefact (see
  -- `Triggerfish.Preset.presetLabel`, "the name if promoted, else the glyph
  -- alias", and docs/DESIGN-balistes-bank-coherence.md). Blank clears back to
  -- anonymous, matching the Store's "empty name = anonymous".
  | RenamePreset Int String
  | SetBankFilter (Maybe Brain)  -- narrow the bank list to one brain (Nothing = all)
  -- Save one bank entry to Amphora, any brain. The generalisation of
  -- PublishActive, which could only ever save a RYTM rhythm because the store
  -- write-back was wired to the RYTM-only `library`.
  | SavePreset Int
  | Step Scheduler.Tick
  | Frame
  | MidiReady (Maybe Midi.MidiOut) String
  | ResetPat
  | Dice
  | PadAt Int Int Int          -- clientX clientY buttons
  | PadRelease                 -- pad pointer-up: broadcast the settled X/Y to the rig
  | StartDrag DragKind Int     -- kind, startVal
  | DragMove Int
  | DragEnd
  | DillaPreset
  | FlatGroove
  | SelectPattern Active       -- switch the playing pattern (Grids / a rhythm)
  | ToggleEdit                 -- reveal all 16 lanes on the active fixed rhythm
  | CellClick Int Int Boolean  -- select a cell (lane, step); shift = clear
  | SetCellVel Int             -- nudge the selected cell's velocity
  | SetCellProb Int            -- nudge its probability
  | SetCellRatchet Int         -- nudge its ratchet count
  | CycleCellCond              -- step its trig condition
  | ClearSelected              -- clear the selected cell + deselect
  | NewPattern                 -- append a fresh empty rhythm + select it
  | SetPatternName String      -- rename the active rhythm
  | PublishActive              -- publish the active rhythm to Amphora (persist + share)
  | PushBalistes               -- lockstep handoff: push BalSim to the rig (ch 11)
  -- POLYTRIG (SELENE DRUMS tab) editor — browser-only, no rig sync.
  | SetJackSource Int String   -- jack i's per-jack pattern
  | SetJackName Int String     -- jack i's route-addressable name
  | SetJackNote Int Int        -- nudge jack i's MIDI note
  | SetRoute Int String        -- route line i
  | AddRoute                   -- append an empty route line
  | RemoveRoute Int            -- drop route line i
  | NoOp

-- | The fixed rhythm currently in view on the RYTM tab: the ephemeral
-- | `scratchFixed` (a recalled snapshot, played read-only) if set, else the
-- | library entry the `AFixed` index points at. `Nothing` off the RYTM tab.
-- | What is SOUNDING: the rhythm the RYTM brain plays, or `Nothing` when RYTM
-- | isn't the live brain. Read by `Step` and the rig push — output questions.
activePattern :: State -> Maybe P.FixedPattern
activePattern s = case s.active of
  AFixed i -> case s.scratchFixed of
    Just p -> Just p
    Nothing -> patternAt s i
  _ -> Nothing

-- | What you are LOOKING AT and EDITING in the RYTM band — independent of which
-- | brain is live, because all three bands are on screen at once and a ghosted
-- | band stays editable (you build the next beat while the current one plays).
-- |
-- | This is the split that `Active` used to blur: `AFixed i` meant both "RYTM is
-- | the sounding brain" AND "rhythm i is selected", so with GRIDS live there
-- | was no selected rhythm at all and the RYTM band rendered empty. `fixedSel`
-- | holds the selection; `active` holds only the output choice.
selectedPattern :: State -> Maybe P.FixedPattern
selectedPattern s = case s.scratchFixed of
  Just p -> Just p
  Nothing -> patternAt s s.fixedSel

-- | The rhythm held in bank entry `i`, with its envelope name injected — the
-- | single-source-of-truth seam (bank content is name-stripped; see
-- | `TriSnapshot.rhythmContent`). `Nothing` when the entry belongs to another
-- | brain, does not parse, or is out of range. That last case is new and real:
-- | `fixedSel` now indexes the WHOLE bank, so it can point at a Grids or Tidal
-- | entry, which the old `Array FixedPattern` could not represent.
patternAt :: State -> Int -> Maybe P.FixedPattern
patternAt s i = case s.presets !! i of
  Nothing -> Nothing
  Just p -> rhythmOfContent (fromMaybe "" p.name) p.content

-- | Every bank entry that is a rhythm, paired with its ORIGINAL bank index —
-- | which is what selection and edits address. Filtering without carrying the
-- | index through would renumber every rhythm and point edits at the wrong entry.
rhythmEntries :: State -> Array (Tuple Int P.FixedPattern)
rhythmEntries s =
  mapMaybe
    (\(Tuple i p) -> Tuple i <$> rhythmOfContent (fromMaybe "" p.name) p.content)
    (mapWithIndex Tuple s.presets)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

rigUrl :: String
rigUrl = "ws://127.0.0.1:3012/ws"

-- | One Grids step = a 16th note (32 steps = two bars). Same lookahead as Odonus.
gridCfg :: Scheduler.GridConfig
gridCfg = { stepBeats: 0.25, lookaheadMs: 120.0, tickMs: 25 }

-- | Sixteenth-note steps per 4/4 bar — the unit the snapshot sequence counts in.
stepsPerBar :: Int
stepsPerBar = 16

midiPortName :: String
midiPortName = "IAC"

-- | GM drum channel (MIDI ch 10) — the Grids device (BD/SD/HH). Canonical
-- | drums channel from the routing map, converted to WebMIDI's 0-indexed form.
drumChannel :: Int
drumChannel = Routing.toWire Routing.drumsChannel

-- | One Tidal cycle == this many POLYTRIG grid steps (one bar). Matches Selene.
cycleSteps :: Int
cycleSteps = 16

-- | Velocity a freshly-clicked fixed-rhythm cell lands at (a firm hit).
editVel :: Int
editVel = 98

-- | Keep a hit around ~0.4s — long enough for the pilot lamps to glow.
flashWindow :: Number
flashWindow = 400000.0

padId :: String
padId = "balistes-pad"

-- | Route atoms address jacks case-insensitively (`BD` fires `bd`).
eqTrigName :: String -> String -> Boolean
eqTrigName a b = toLower a == toLower b

-- | The current MIDI note of POLYTRIG jack `i` (default GM ladder if absent).
jackNoteOf :: M.TrigBank -> Int -> Int
jackNoteOf tb i = maybe (36 + i) _.note (tb.jacks !! i)
