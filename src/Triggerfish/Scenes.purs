-- | `Triggerfish.Scenes` — the Ableton-like arrangement layer: a rig-wide grid of
-- | SCENES, one of the two sequencers the rig carries (the other being the
-- | Tidal-like per-machine macro lanes — see `docs/DESIGN-scene-modal.md`,
-- | "Resolution 2026-07-30").
-- |
-- | A **scene** is a tuple across the live machines — one cell per machine, each
-- | holding that machine's chosen preset **by glyph alias** (`"owl-bomb"`), the
-- | stable content identity the preset banks and a future macro lane share. An
-- | EMPTY cell means **leave-as-is** — a scene only touches the machines you set
-- | (this is the defining difference from a macro lane, where an empty step is a
-- | `~` rest = silence). Launching a scene recalls each non-empty cell on its
-- | machine at once; the tab-bar chips flip to those glyphs (the pictographic
-- | score animates for free).
-- |
-- | This module is pure model only — the shell owns the launch/recall wiring, the
-- | bar-clock advance, and rendering. A scene references presets by alias (not slot
-- | index), so it survives bank reorder/delete; a deleted preset just shows an
-- | unresolved cell to re-point.
module Triggerfish.Scenes
  ( SceneCell
  , Scene
  , sceneMachines
  , emptyScene
  , cellAt
  , setCellAt
  ) where

import Prelude

import Data.Array ((!!), modifyAt, replicate, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Triggerfish.Transport (Which(..))

-- | One machine's slot in a scene: `Just alias` recalls that preset; `Nothing`
-- | leaves the machine as it is.
type SceneCell = Maybe String

-- | A scene: an optional name (unnamed = fast/numbered) and one cell per machine
-- | in `sceneMachines` order.
type Scene =
  { name :: Maybe String
  , cells :: Array SceneCell
  }

-- | The machines a scene spans, in column order. Balistes and the Selene rack
-- | left for pages of their own on 2026-09-29, taking their columns with them
-- | (`Scenes.Store` drops them from a grid saved before then); they rejoin
-- | through the stage, not this page.
sceneMachines :: Array Which
sceneMachines = [ Odo, Vet ]

-- | A blank scene — every machine left-as-is.
emptyScene :: Scene
emptyScene = { name: Nothing, cells: replicate (length sceneMachines) Nothing }

-- | The cell for machine-column `i` (out of range ⇒ leave-as-is).
cellAt :: Int -> Scene -> SceneCell
cellAt i s = fromMaybe Nothing (s.cells !! i)

-- | Set machine-column `i`'s cell (out of range ⇒ unchanged).
setCellAt :: Int -> SceneCell -> Scene -> Scene
setCellAt i cell s = s { cells = fromMaybe s.cells (modifyAt i (const cell) s.cells) }
