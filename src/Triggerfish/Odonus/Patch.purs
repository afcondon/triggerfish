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
  , recallText
  , loadText
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Triggerfish.Odonus.Grid.Types (State)
import Triggerfish.Odonus.Lepidoptera (OdonusPatch, printPatch, parsePatch)
import Triggerfish.Odonus.Model as M

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

-- | The live patch rendered to eDSL text — the shell's `AskSource` answer and
-- | the form a scene is saved in.
patchText :: State -> String
patchText = printPatch <<< capturePatch

-- | Recall a scene from its eDSL text, PHASE-PRESERVING: apply the saved patch
-- | but carry the live playhead phase across (cursor / seqPos / accumulator /
-- | pendStep), so a live scene change flows like a continuing fugue with key
-- | changes and voices coming and going. Unparseable text → no-op.
recallText :: String -> State -> State
recallText txt s = case parsePatch txt of
  Just p -> (applyPatch p s) { odo = M.recallScene s.odo p.odo }
  Nothing -> s

-- | Load a patch from its eDSL text, HARD-RESETTING the playheads — the cold
-- | load from the Tidal library manager. Unparseable text → no-op.
loadText :: String -> State -> State
loadText txt s = case parsePatch txt of
  Just p -> applyPatch p s
  Nothing -> s
