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
  , recallGestureText
  , loadText
  , HarmonicContext
  , harmonicSummary
  ) where

import Prelude

import Data.Array (find, (!!))
import Data.Maybe (Maybe(..), isJust)
import Data.String.Common (joinWith)
import Triggerfish.Odonus.Grid.Types (GenSource, SourceTag(..), State, genKinds, genDefaultRate, genDefaultAmt)
import Triggerfish.Odonus.Lepidoptera (OdonusPatch, printPatch, parsePatch)
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale

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
  , source = deriveSource p.follow p.odo.chord.on
  , gen = reconcileGen p.gen
  , genSpread = p.genSpread
  , genBias = p.genBias
  , swing = p.swing
  , velHumanize = p.velHumanize
  , stepDiv = p.stepDiv
  }

-- | Reconcile a loaded gen array against the full generator set: keep every
-- | source the patch saved, and fill in any generator the patch PREDATES (e.g.
-- | a scene authored before GVel/VELOCITY existed) with its default, off. This
-- | is the fix for a dead generator LED: without it, a source absent from the
-- | loaded array is absent from `s.gen`, so its LED reads permanently off and
-- | its toggle no-ops — `toggleGen` only maps over sources already present.
-- | Iterating `genKinds` also fixes the on-screen order to the canonical one.
reconcileGen :: Array GenSource -> Array GenSource
reconcileGen loaded =
  genKinds <#> \k -> case find (\g -> g.kind == k) loaded of
    Just g -> g
    Nothing -> { kind: k, on: false, rate: genDefaultRate k, amt: genDefaultAmt k }

-- | The source intent a loaded patch implies (it isn't serialised separately):
-- | a follow → Vetula; else Scale.
deriveSource :: Maybe Int -> Boolean -> SourceTag
deriveSource follow _ =
  if isJust follow then SVetula else SScale

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

-- | Recall a scene's GESTURE into the CURRENT harmonic context (#150): apply
-- | the saved cells / playheads / register transforms / gen matrix / feel, but
-- | KEEP the live harmony — root, scale, distribution, any Vetula pitchSet, the
-- | chord overlay, and whether we're following a voice. Because cell notes are
-- | scale degrees, the saved riff re-voices through whatever key or progression
-- | is sounding now: the same lick in the current key. Playhead phase carries
-- | across (as `recallText`). The gen matrix / marbles / swing / stepDiv ARE
-- | part of the gesture, so they come from the scene; `follow`/`source` do not.
-- | Unparseable text → no-op.
recallGestureText :: String -> State -> State
recallGestureText txt s = case parsePatch txt of
  Just p -> s
    { odo = M.recallGesture s.odo p.odo
    , gen = reconcileGen p.gen
    , genSpread = p.genSpread
    , genBias = p.genBias
    , swing = p.swing
    , velHumanize = p.velHumanize
    , stepDiv = p.stepDiv
    }
  Nothing -> s

-- | Load a patch from its eDSL text, HARD-RESETTING the playheads — the cold
-- | load from the Tidal library manager. Unparseable text → no-op.
loadText :: String -> State -> State
loadText txt s = case parsePatch txt of
  Just p -> applyPatch p s
  Nothing -> s

-- | The harmonic reading of a captured patch, for the REPLAY card's "show
-- | harmonic context" — what a guitarist needs to jam over a looped good bit:
-- | the key (root + auto-named scale), the scale's pitches as note names, and
-- | the current chord (as note names) when the chord overlay is driving.
type HarmonicContext =
  { root :: String          -- e.g. "D"
  , scale :: String         -- auto-recognised scale name, e.g. "dorian"
  , chord :: Maybe String   -- the sounding chord as note names, if the overlay is on
  , notes :: Array String   -- the scale's pitch classes as note names
  }

-- | Read the harmony out of a mark's stored patch text. Unparseable → Nothing.
harmonicSummary :: String -> Maybe HarmonicContext
harmonicSummary txt = case parsePatch txt of
  Nothing -> Nothing
  Just p ->
    let
      odo = p.odo
      chordPcs = if odo.chord.on then odo.chord.feed !! odo.chord.ix else Nothing
    in
      Just
        { root: Scale.rootName odo.rootPc
        , scale: M.scaleTypeName odo
        , chord: map (joinWith " " <<< map Scale.rootName) chordPcs
        , notes: map Scale.rootName (Scale.pitchClassesOf (M.scaleOf odo))
        }
