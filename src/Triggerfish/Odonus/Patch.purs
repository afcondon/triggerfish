-- | Triggerfish.Odonus.Patch — the bridge between the component State and the
-- | Lepidoptera `OdonusPatch`. Held in its own module so both the Source view
-- | and the grid component can capture / apply / render a patch without a cycle
-- | (the view is imported by the component, and Lepidoptera imports Grid.Types,
-- | so neither can host this). `capturePatch` reads the authored slice of State;
-- | `applyPatch` writes it back (leaving transport / runtime untouched).
module Triggerfish.Odonus.Patch
  ( capturePatch
  , applyPatch
  , patchText
  ) where

import Prelude

import Triggerfish.Odonus.Grid.Types (State)
import Triggerfish.Odonus.Lepidoptera (OdonusPatch, printPatch)

-- | The authored slice of State, ready to render / persist. The live patch
-- | carries the fixed name "live" until the library manager (A5) names entries.
capturePatch :: State -> OdonusPatch
capturePatch s =
  { name: "live"
  , odo: s.odo
  , follow: s.follow
  , gen: s.gen
  , genSpread: s.genSpread
  , genBias: s.genBias
  , swing: s.swing
  , velHumanize: s.velHumanize
  , stepDiv: s.stepDiv
  }

-- | Load a patch over State: replace the authored fields, leave the transport,
-- | clock, MIDI, scenes and runtime alone.
applyPatch :: OdonusPatch -> State -> State
applyPatch p s = s
  { odo = p.odo
  , follow = p.follow
  , gen = p.gen
  , genSpread = p.genSpread
  , genBias = p.genBias
  , swing = p.swing
  , velHumanize = p.velHumanize
  , stepDiv = p.stepDiv
  }

-- | The live patch rendered to eDSL text — the Source pane body and the
-- | shell's `AskSource` answer.
patchText :: State -> String
patchText = printPatch <<< capturePatch
