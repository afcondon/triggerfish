-- | Triggerfish.Balistes.Types — the stable DAG root for the Balistes component:
-- | the `State` record, the `Action` set, the small closed tag types they lean on
-- | (`Active`, `DragKind`, `KnobTarget`), and the domain constants +
-- | pure vocab (`targetRange`, `knobValue`, `applyKnob`, `drumChannel`, …).
-- |
-- | This module owns no rendering and no state machine — it's the shared
-- | vocabulary both the view modules (`Balistes.View.*`, `Balistes.Widgets`,
-- | `Balistes.Snapshot`) and the controller (`Balistes.Component`) import, so
-- | neither imports the other. Mirrors `Odonus.Grid.Types`.
module Triggerfish.Balistes.Types
  ( KnobTarget(..)
  , targetRange
  , knobValue
  , applyKnob
  , Active(..)
  , ClickMode(..)
  , DragKind(..)
  , Paint(..)
  , Spot
  , Copied
  , Drag
  , State
  , fillShown
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
  , editVel
  , padId
  ) where

import Prelude

import Data.Array (mapMaybe, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Binnacle as Binnacle
import Triggerfish.Routing.Model as RM
import Triggerfish.Routing.Out as RO
import Binnacle.Scheduler as Scheduler
import Halogen as H
import Reef.Balistes.Input as RBI
import Triggerfish.Balistes.Model as M
import Triggerfish.Balistes.Pattern as P
import Triggerfish.Glyph as G
import Data.Map (Map)
import Triggerfish.Balistes.Follow as Follow
import Triggerfish.Library as Library
import Triggerfish.Preset (Preset)
import Triggerfish.Balistes.TriSnapshot (rhythmOfContent)
import Triggerfish.Midi.Routing as Routing
import Triggerfish.Transport (Sounding)
import Web.Event.Event as E

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
-- | (`library !! i`), played verbatim.
data Active = AGrids | AFixed Int

derive instance eqActive :: Eq Active

-- | What clicking a beat's 2-glyph DOES. AC's ask (2026-08-07), the same shape as
-- | Vetula's HUNT/PERFORM: one visible switch, one gesture, two intents.
-- |
-- |   Audition — play it now. You are jamming, hunting for what works.
-- |   Assemble — append its token to the lane. You are writing, and the glyphs
-- |              are your alphabet.
-- |
-- | Before this, the ASSEMBLE chips only ever appended, so there was no way to
-- | hear a banked beat without leaving the surface for the preset modal — which
-- | covers the bands you are trying to listen to. See docs/DESIGN-balistes-beds.md.
data ClickMode = Audition | Assemble

derive instance eqClickMode :: Eq ClickMode

-- | A document-tracked drag turns a knob or subdivides a Grids cell into
-- | ratchets (`DCell lane step`). One plumbing.
data DragKind = DKnob KnobTarget | DCell Int Int | DHit Int Int

-- | What a press on a RYTM cell turned out to be (AC, 2026-10-14). A press on
-- | an empty cell paints; on a hit it is `Undecided` until the pointer moves:
-- | along the row it erases, up or down it shapes the hit's velocity, and a
-- | release without moving removes the hit.
-- | `Shaping` carries the group being shaped with each hit's velocity when the
-- | drag began; `Selecting` the selection before a shift-drag began, which the
-- | rectangle adds to.
data Paint
  = Undecided
  | Painting
  | Erasing
  | Shaping (Array { spot :: Spot, vel :: Int })
  | Selecting (Array Spot)

-- | A cell of the RYTM grid.
type Spot = { lane :: Int, step :: Int }

-- | A copied hit: its lane, its step counted from the copy's first step.
type Copied = { lane :: Int, step :: Int, cell :: P.Cell }

derive instance eqPaint :: Eq Paint

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
  , binnacle :: Maybe Binnacle.Binnacle
  -- The rig's reading of each lane source (POLYTRIG jacks and routes), by
  -- source text: the page reads no Tidal itself (Triggerfish.LaneShapes).
  -- Every MIDI output port, not one: the routing table may name any of them, so
  -- the emit path resolves per leg rather than holding a single handle. See
  -- Triggerfish.Routing.Out.
  , outs :: RO.Outs
  , routing :: RM.Table
  , midiName :: String
  , clockTempo :: Number
  , clockLocked :: Boolean
  , clockBeat :: Number
  , clockBar :: Int
  , anchorCount :: Int
  , nowMicros :: Number
  , dragging :: Maybe Drag
  , paint :: Maybe Paint
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
  -- What a beat-glyph click means. Not persisted: it is a posture you are in for
  -- the next few minutes, not a preference (cf. Transport.Store, which persists
  -- the authority mode and nothing about what was playing).
  , clickMode :: ClickMode
  -- The lane EDITOR modal: a full-size surface for writing the tidal expression
  -- of a combined beat, with the notation guide beside it. The ASSEMBLE panel's
  -- inline box stays for quick edits; this is where you compose.
  , laneEditOpen :: Boolean
  -- macro-tidal: this machine's arrangement lane, MIRRORED from the shell (which
  -- owns and persists it). Edits raise back up; the shell pushes changes down.
  , lane :: String
  , laneReadout :: String        -- the lane's live current-token label
  , fixedSel :: Int              -- the SELECTED RYTM rhythm (see `selectedPattern`);
                                 -- independent of whether RYTM is the live brain
  -- the hits the strip edits, on the selected rhythm (AC, 2026-10-14: a
  -- group, by shift-click and shift-drag); the last cell pressed, where a
  -- paste lands; and what ⌘C copied.
  , selected :: Array Spot
  , cursor :: Maybe Spot
  , copied :: Array Copied
  -- the muted kit lanes, one bit a lane (Reef.Balistes.Kit): not a
  -- rhythm's, so never saved; sent to the rig as `balistes-mute`
  , mutes :: Int
  -- FILL (AC, 2026-10-14): the rhythm plays its fill, and the grid shows and
  -- edits it; performance state like the mutes, sent as `balistes-fill`
  , fill :: Boolean
  , fillHeld :: Boolean          -- the fill is on because F is held
  -- What the rig's fill line makes of FILL (`balistes $ fill "<f!7 t>"`),
  -- applied on its step: Nothing with no line. Never written into `fill`,
  -- which is the player's own and what the page tells the rig.
  , rigFill :: Maybe Boolean
  , rigFillQueue :: Array Follow.FillAt
  -- the layer a press on the grid edits, fixed when it starts: a fill line
  -- flipping FILL mid-stroke must not split one stroke across two layers
  , pressFill :: Maybe Boolean
  -- the POLYTRIG jack rack (SELENE DRUMS tab) — browser-only, no reef path.
  -- The named rhythms the rig's stage holds, by word (Balistes.Stage), which
  -- `balistes $ "<a b>"` plays; Nothing until the stage has answered a
  -- subscribe on this connect.
  , stageRhythms :: Maybe (Map String String)
  -- the bank as Amphora keeps it (`Triggerfish.Library`); Nothing until opened
  , shelf :: Maybe (Library.Shelf Preset)
  -- the drawer's last delete, which its undo puts back where it was
  , undoDelete :: Maybe { at :: Int, preset :: Preset }
  -- Following a `balistes $` line the rig plays (Balistes.Follow): the beat
  -- shown came in at step `from`, or a rest. Shown, never sounded or pushed;
  -- the page's own push takes the voice back and ends it.
  , follow :: Maybe { from :: Int, rest :: Boolean }
  -- what the rig said it will play next, each applied on its step
  , followQueue :: Array Follow.Now
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
  | RecallPreset Int
  | StarPreset Int
  | DeletePreset Int
  | SetClickMode ClickMode       -- audition-vs-assemble: what clicking a beat does
  | OpenLaneEdit                 -- open the lane-expression editor
  | CloseLaneEdit
  | Step Scheduler.Tick
  | Frame
  | MidiReady RO.Outs String
  | StageOpen                 -- the rig socket (re)connected: subscribe to the stage
  | StageFrameIn String       -- a frame from the rig (the stage's table among them)
  | LibraryChanged             -- the bank changed in Amphora (another page)
  | Dice
  | DiceDensities
  | DiceDilla
  | PadAt Int Int Int          -- clientX clientY buttons
  | PadRelease                 -- pad pointer-up: broadcast the settled X/Y to the rig
  | StartDrag DragKind Int     -- kind, startVal
  | DragMove Int
  | DragEnd
  | FlatGroove
  | SelectPattern Active       -- switch the playing pattern (Grids / a rhythm)
  | ToggleEdit                 -- reveal all 16 lanes on the active fixed rhythm
  | CellDown Int Int Boolean   -- a press on a cell (lane, step); shift selects
  | PaintOver Int Int          -- the pointer entered a cell mid-press
  | SetCellVel Int             -- nudge the selected cell's velocity
  | SetCellProb Int            -- nudge its probability
  | SetCellRatchet Int         -- nudge its ratchet count
  | CycleCellCond              -- step its trig condition
  | ClearSelected              -- clear the selected hits + deselect
  | SetFill Boolean            -- the fill on or off
  | ToggleFillMute Int         -- mute the lane in the fill, or unmute it
  | ToggleMute (Array Int)     -- mute the lanes, or unmute them if all are muted
  | RytmKeyUp E.Event          -- F let go: a held fill ends
  | RytmKey E.Event            -- ⌫, ←/→, ⌘C/⌘V and Esc on the selection
  | SetLength Int              -- the rhythm's length, the new bars repeating it
  | NudgeLength Int            -- a step longer (silent) or shorter
  | PushToRytm                 -- bake what Grids plays into a new RYTM rhythm
  | NewPattern                 -- append a fresh empty rhythm + select it
  | HearLane Int               -- one hit of a kit lane, where the routing sends it
  | PushBalistes               -- lockstep handoff: the routing table, then the brain's state, to the rig
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

-- | Velocity a freshly-clicked fixed-rhythm cell lands at (a firm hit).
editVel :: Int
editVel = 98

padId :: String
padId = "balistes-pad"


-- | The FILL the page shows, lights and edits: the player's own (button or
-- | F), or the rig's fill line filling now. AC, 2026-10-22: letting the
-- | line flip the grid lets a deft hand edit the fill and the rhythm as they
-- | come round, without stopping anything; a line too fast for that is
-- | stopped from the REPL (`balistes $ fill off`).
fillShown :: State -> Boolean
fillShown s = s.fill || s.rigFill == Just true
