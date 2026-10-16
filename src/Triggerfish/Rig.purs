-- | The physical modular rig, as far as Triggerfish's *routing* cares about it.
-- |
-- | The point of this module is negative space: a `Target` (Selene's routing
-- | atom — an ES-9 block, an FH-2 bank, a MIDI channel) is only offerable in a
-- | routing menu if the hardware that would receive it is actually patched into
-- | the rack. So the rig is modelled as *how many 8-wide blocks each expander
-- | family contributes*, and `targetGroups` expands that into exactly the menu
-- | leaves that are known-good — never a `GT 3` when only two gate blocks are
-- | present.
-- |
-- | This is deliberately NOT the es9-config / fh2-config SysEx model: those
-- | describe the internal routing of each module; this describes only which
-- | destination *slots exist* on the current patch. `defaultRig` is the shape
-- | of AC's own rig — edit it here when the hardware changes (or, later, drive
-- | it from a discovery handshake).
module Triggerfish.Rig
  ( Es9Rig
  , Fh2Rig
  , RigConfig
  , defaultRig
  , availableTargets
  , rigOutputs
  , targetGroups
  ) where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.Widgets.Select as Select
import Triggerfish.Selene.Layout (Output)
import Triggerfish.Selene.Model (Target(..), targetWire)

-- | ES-9 side. `gtBlocks` = 8-gate blocks (ES-5 + each ESX-8GT); `cvBlocks` =
-- | 8-CV blocks (each ESX-8CV). The ES-9's own eight panel jacks are `ES9Main`,
-- | always present when the ES-9 is.
type Es9Rig = { gtBlocks :: Int, cvBlocks :: Int }

-- | FH-2 side. The FH-2's own eight are always present; `gtExpanders` counts
-- | FHX-8GTs and `cvExpanders` FHX-8CVs (the names `FH2.Roles.Profile` uses).
-- | Each family numbers its expanders from 1 by its own jumpers, so the counts
-- | assume they are set 1, 2, … with no gaps.
type Fh2Rig = { gtExpanders :: Int, cvExpanders :: Int }

-- | The whole routing-visible rig. `Nothing` for a family means it isn't
-- | patched in, so none of its destinations are offered at all.
type RigConfig =
  { es9 :: Maybe Es9Rig
  , fh2 :: Maybe Fh2Rig
  , midiChannels :: Int
  }

-- | AC's rig: ES-9 with two gate blocks (ES-5 + one ESX-8GT) and one ESX-8CV,
-- | an FH-2 with one FHX-8GT and one FHX-8CV, 16 MIDI channels. Adjust to
-- | match the actual patch.
defaultRig :: RigConfig
defaultRig =
  { es9: Just { gtBlocks: 2, cvBlocks: 1 }
  , fh2: Just { gtExpanders: 1, cvExpanders: 1 }
  , midiChannels: 16
  }

-- | The flat set of every destination the rig can currently receive, in menu
-- | order (ES-9 main → gates → CVs, then FH-2 banks, then MIDI channels).
availableTargets :: RigConfig -> Array Target
availableTargets cfg =
  es9 cfg.es9 <> fh2 cfg.fh2 <> midi cfg.midiChannels
  where
  es9 = case _ of
    Nothing -> []
    Just r -> [ ES9Main ] <> blocks r.gtBlocks ES9Gt <> blocks r.cvBlocks ES9Cv
  fh2 = case _ of
    Nothing -> []
    Just r -> fh2Targets r
  midi n = guard (n >= 1) (map Midi (range 1 n))

-- | Every PHYSICAL OUTPUT the rig has, in `Selene.Layout`'s address vocabulary.
-- | This is the row set of the output-backward view — one row per jack, whether
-- | or not anything claims it, because "which jacks are free?" is a first-class
-- | answer rather than a leftover.
-- |
-- | The ES-9's own eight panel jacks are listed, though no `Destination`
-- | addresses them today, so they will always read as free. That is truthful:
-- | they exist and nothing here can reach them.
rigOutputs :: RigConfig -> Array Output
rigOutputs cfg = es9 cfg.es9 <> fh2 cfg.fh2
  where
  jacks device bank = map (\slot -> { device, bank, slot }) (range 0 7)

  es9 = case _ of
    Nothing -> []
    Just r ->
      jacks "es9" "main"
        <> expanders "es9" "gt" r.gtBlocks
        <> expanders "es9" "cv" r.cvBlocks

  fh2 = case _ of
    Nothing -> []
    Just r -> jacks "fh2" "main" <> expanders "fh2" "gt" r.gtExpanders <> expanders "fh2" "cv" r.cvExpanders

  expanders device prefix n =
    guard (n >= 1) (range 0 (n - 1)) >>= \i -> jacks device (prefix <> show i)

-- | The same targets, arranged as a one-level cascade for `Select`: top-level
-- | family rows (ES-9 / FH-2 / MIDI) each flying out to their bounded leaves.
-- | Empty families are dropped so the menu never shows a headed-but-empty group.
targetGroups :: RigConfig -> Array Select.OptionGroup
targetGroups cfg =
  nonEmpty (es9 cfg.es9) <> nonEmpty (fh2 cfg.fh2)
    <> nonEmpty { label: "MIDI", options: map (opt leafLabel) (midiTargets cfg.midiChannels) }
  where
  es9 = case _ of
    Nothing -> { label: "ES-9", options: [] }
    Just r ->
      { label: "ES-9"
      , options: map (opt leafLabel) ([ ES9Main ] <> blocks r.gtBlocks ES9Gt <> blocks r.cvBlocks ES9Cv)
      }
  fh2 = case _ of
    Nothing -> { label: "FH-2", options: [] }
    Just r -> { label: "FH-2", options: map (opt leafLabel) (fh2Targets r) }
  midiTargets n = guard (n >= 1) (map Midi (range 1 n))
  nonEmpty g = guard (g.options /= []) [ g ]

-- | The FH-2's banks in menu order: its own eight, the FHX-8GTs, the FHX-8CVs.
fh2Targets :: Fh2Rig -> Array Target
fh2Targets r = [ FH2 0 ] <> blocks r.gtExpanders (\i -> FH2 (i + 1)) <> blocks r.cvExpanders FH2Cv

-- | A bounded run of expander blocks `f 0 .. f (n-1)`, empty when `n <= 0` (so
-- | `range 0 (-1)` never produces the descending `[0,-1]`).
blocks :: Int -> (Int -> Target) -> Array Target
blocks n f = guard (n >= 1) (map f (range 0 (n - 1)))

opt :: (Target -> String) -> Target -> Select.Option
opt lbl t = { value: targetWire t, label: lbl t }

-- | Short leaf labels — the family is already named by the enclosing group, so
-- | drop the "ES-9 · " / "FH-2 · " prefix `targetLabel` carries.
leafLabel :: Target -> String
leafLabel = case _ of
  ES9Main -> "MAIN"
  ES9Gt n -> "GT " <> show n
  ES9Cv n -> "CV " <> show n
  FH2 0 -> "MAIN"
  FH2 n -> "GT " <> show (n - 1)
  FH2Cv n -> "CV " <> show n
  Midi n -> "CH " <> show n
  Virtual s -> s
