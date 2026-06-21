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
  , State
  , Action(..)
  ) where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe)
import Halogen as H
import Triggerfish.Odonus.Model as M
import Binnacle (Binnacle)
import Binnacle.Midi as Midi
import Binnacle.Scheduler as Scheduler

data KnobTarget
  = CellNote Int
  | CellDur Int
  | HeadDir Int
  | HeadSpeed Int
  | HeadTransp Int
  | HeadOffset Int
  | HeadLen Int
  | Spread
  | GateLen

targetRange :: KnobTarget -> { lo :: Int, hi :: Int }
targetRange = case _ of
  CellNote _ -> { lo: 36, hi: 84 }
  CellDur _ -> { lo: 1, hi: 8 }
  HeadDir _ -> { lo: 0, hi: 2 }
  HeadSpeed _ -> { lo: 0, hi: length M.speedTable - 1 }
  HeadTransp _ -> { lo: -24, hi: 24 }
  HeadOffset _ -> { lo: 0, hi: 15 }
  HeadLen _ -> { lo: 1, hi: 16 }
  Spread -> { lo: 1, hi: 12 }
  GateLen -> { lo: 10, hi: 200 }

applyTarget :: KnobTarget -> Int -> M.Odonus -> M.Odonus
applyTarget t v = case t of
  CellNote i -> M.setNote i v
  CellDur i -> M.setCellDur i v
  HeadDir h -> M.setHeadDir h v
  HeadSpeed h -> M.setHeadSpeedIx h v
  HeadTransp h -> M.setHeadTransp h v
  HeadOffset h -> M.setHeadOffset h v
  HeadLen h -> M.setHeadLen h v
  Spread -> M.setSpread v
  GateLen -> M.setGatePct v

type DragState = { target :: KnobTarget, startY :: Int, startVal :: Int }

-- | One emitted note in the scrolling monitor. `fireUnixMicros` is the
-- | wall-clock instant it sounds; the river positions it by how long ago
-- | that was (so the visual onset lands exactly on the audio onset).
type NoteEvent = { pitch :: Int, headIdx :: Int, fireUnixMicros :: Number }

-- | A saved whole-Odonus setting: notes, heads, scale — the unit of
-- | composition. Sequencing scenes builds flowing fugues with key changes
-- | and voices dropping in and out.
type Scene = { name :: String, odo :: M.Odonus }

type State =
  { odo :: M.Odonus
  , running :: Boolean
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
  , chain :: Boolean        -- auto-advance scenes at bar boundaries
  , sceneIx :: Int          -- current scene in the chain
  , sceneBarAnchor :: Int   -- bar at which the current scene started
  , barsPerScene :: Int
  , stepDiv :: Int          -- global clock divider (1=1/16 .. 16=whole note)
  , headNote :: Array (Maybe Int)  -- the held/sounding MIDI note per head (4)
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
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  | UnifyHeads
  | CycleScaleType Int
  | ToggleDist
  | SetRoot Int
  | SetOctave Int
  | SetDegShift Int
  | ToggleScaleNote Int
  | CaptureScene
  | RecallScene Int
  | DeleteScene Int
  | ToggleChain
  | BumpBars Int
  | SetStepDiv Int
  | KnobDown KnobTarget Int
  | DragMove Int
  | DragEnd
