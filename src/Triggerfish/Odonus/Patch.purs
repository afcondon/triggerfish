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
  , markText
  , asCode
  , nowText
  , soundingOf
  , markContext
  , recallText
  , recallGestureText
  , loadText
  , HarmonicContext
  , harmonicSummary
  ) where

import Prelude

import Data.Array (find)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith)
import Reef.PitchSet (PitchSet(..))
import Reef.Route (Feed(..), Feeds, printFeeds)
import Triggerfish.Odonus.Grid.Types (GenSource, State, genKinds, genDefaultRate, genDefaultAmt)
import Reef.Odonus.Patch (OdonusPatch, parseNow, parsePatch, printNow, printPatch, reconcileGen, withPhases)
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale

-- | The authored slice of State, ready to render / persist. The live patch
-- | carries the fixed name "live" until the library manager (A5) names entries.
-- |
-- | Odonus alone: what the harmony routes are feeding it is context, not its
-- | own setting (AC, 2026-10-03), so a fed scale pattern or harmony is left
-- | out and the authored scale shown; a mark keeps the routes and what they
-- | gave in `nowText`. (A fed key's pitch set and a fed output scale are
-- | never printed.)
capturePatch :: State -> OdonusPatch
capturePatch s =
  { name: "live"
  , odo: ownOdo s.feedsSeen s.odo
  , gen: s.gen
  , genSpread: s.genSpread
  , genBias: s.genBias
  , swing: s.swing
  , velHumanize: s.velHumanize
  , stepDiv: s.stepDiv
  }

-- | Odonus with what the routes feed taken out.
ownOdo :: Feeds -> M.Odonus -> M.Odonus
ownOdo fs o = unOut (unGrid o)
  where
  unGrid x = case fs.grid of
    FeedScale _ -> M.setScalePattern Nothing x
    _ -> x
  unOut x = case fs.out of
    FeedHarmony _ -> x { harmony = Nothing, chord = Nothing }
    _ -> x

-- | Odonus as a mark gives it to Limulus: two blocks, each a recall Limulus
-- | can evaluate (`odonus $ odonusPatch …`, `odonus $ odonusNow …`), the
-- | patch first (docs/kb/plans/the-deck.md).
markText :: State -> String
markText s = asCode (patchText s) (nowText s)

-- | A patch and its instant as Limulus blocks, separated by a blank line.
asCode :: String -> String -> String
asCode patch now = "odonus $ " <> patch <> (if now == "" then "" else "\n\nodonus $ " <> now)

-- | What a patch leaves out, at this instant (Reef.Odonus.Patch.OdonusNow):
-- | where the playheads were and the generators' seed, so a moving patch can
-- | play on exactly as it was; what Odonus was quantising to; and the
-- | harmony routes and what they gave.
nowText :: State -> String
nowText s =
  printNow
    { step: s.nextModelStep
    , tempo: round s.clockTempo
    , seed: s.genSeed
    , frozen: s.genFrozen
    , phases: map (\hd -> { cursor: hd.cursor, seqPos: hd.seqPos, accumulator: hd.accumulator, pendStep: hd.pendStep, etick: hd.etick, hold: hd.hold }) s.odo.heads
    , sounding: soundingOf s
    , routes: s.routesText
    , feeds: let t = printFeeds s.feedsSeen in if t == "" then Nothing else Just t
    }

-- | Load a patch over State: replace the authored fields, leave the transport,
-- | clock, MIDI, scenes and runtime alone.
applyPatch :: OdonusPatch -> State -> State
applyPatch p s = s
  { odo = p.odo
  , gen = reconcileGen p.gen
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

-- | Recall a scene's GESTURE into the CURRENT harmonic context (#150): apply
-- | the saved cells / playheads / register transforms / gen matrix / feel, but
-- | KEEP the live harmony — root, scale, distribution, any Vetula pitchSet, and
-- | the harmony pattern. Because cell notes are
-- | scale degrees, the saved riff re-voices through whatever key or progression
-- | is sounding now: the same lick in the current key. Playhead phase carries
-- | across (as `recallText`). The gen matrix / marbles / swing / stepDiv ARE
-- | part of the gesture, so they come from the scene; the harmony does not.
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
-- | the harmony pattern when one is set.
type HarmonicContext =
  { root :: String          -- e.g. "D"
  , scale :: String         -- auto-recognised scale name, e.g. "dorian"
  , chord :: Maybe String   -- the harmony pattern (`<c'maj7 a'min7>/2`), if one is set
  , notes :: Array String   -- the scale's pitch classes as note names
  }

-- | What Odonus is quantising to now, as pitch classes: the effective scale
-- | (a fed key, a sampled scale pattern, or the hand-set one) and the chord.
soundingOf :: State -> { root :: Int, scale :: Array Int, chord :: Maybe (Array Int) }
soundingOf s =
  let PitchSet ps = M.effectivePitchSet s.odo
  in { root: ps.root `mod` 12, scale: ps.offsets, chord: s.odo.chord }

-- | A mark's harmonic context for the ♫ panel: what was sounding at the mark,
-- | else (a mark from before 2026-10-03) what its patch says.
markContext :: { patch :: String, sounding :: Maybe { root :: Int, scale :: Array Int, chord :: Maybe (Array Int) } | _ } -> Maybe HarmonicContext
markContext m = case m.sounding of
  Just sd -> Just
    { root: Scale.rootName sd.root
    , scale: Scale.recogniseScale sd.scale
    , chord: map (\c -> joinWith " " (map Scale.rootName c)) sd.chord
    , notes: map (\iv -> Scale.rootName ((sd.root + iv) `mod` 12)) sd.scale
    }
  Nothing -> harmonicSummary m.patch

-- | Read the harmony out of a mark's stored patch text. Unparseable → Nothing.
harmonicSummary :: String -> Maybe HarmonicContext
harmonicSummary txt = case parsePatch txt of
  Nothing -> Nothing
  Just p ->
    let
      odo = p.odo
    in
      Just
        { root: Scale.rootName odo.rootPc
        , scale: M.scaleTypeName odo
        , chord: odo.harmony
        , notes: map Scale.rootName (Scale.pitchClassesOf (M.scaleOf odo))
        }
