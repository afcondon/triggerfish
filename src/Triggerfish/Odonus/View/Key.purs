-- | The READ-ONLY harmonic-context display: what Odonus's grid quantises to and
-- | what its output snaps to. Since 2026-10-02 the harmony routes decide where
-- | each comes from (`routing/harmony`, the dashboard's Harmony matrix: a scale,
-- | Vetula's key, a Vetula card's chords, a harmony pattern); Vetula is one
-- | source among them, no longer the authority. This shows the result.
-- |
-- | Since 2026-10-01 the chords arrive as a Tidal pattern, the harmony
-- | (`odonus $ harmony "..."`, set by Vetula or typed in Limulus): the strip
-- | shows the pattern beside the scale, and rings the chord it gives this step.
-- |
-- | Was the KEY *panel* — a whole right-hand column carrying this plus SCENES plus
-- | LOGBOOK. Retired 2026-08-06 (AC): the column cost a fifth of the width while
-- | PARAMETERS next to it needed a scrollbar. This is now a compact strip in
-- | Odonus's secondary nav (`Odonus.View.Nav`), which is where a live player wants
-- | it anyway — glanceable, not a panel to read.
module Triggerfish.Odonus.View.Key (quantPieces) where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (toUpper)
import Reef.Route as Route
import Halogen as H
import Halogen.HTML as HH
import Reef.PitchSet (PitchSet(..))
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, style)

-- | Odonus's two quantisations, each opening the panel it acts on, read right
-- | to left as the notes travel (AC, 2026-10-05): the grid's cells are mapped onto
-- | the GRID set, the playheads shift them, the OUTPUT set snaps them, and they
-- | leave through the river. Each piece: its label, the set, and where the set
-- | comes from. A readout, not a control: routes are patched on the dashboard's
-- | chart, or set by a line in Limulus.
-- |
-- |   GRID    the scale a cell's value is mapped onto (q1, `odonus.grid`)
-- |   OUTPUT  the set each note snaps to past its head's offset (q2,
-- |           `odonus.out`): a chord, an output scale, or none
quantPieces :: forall m. State -> { output :: H.ComponentHTML Action Slots m, grid :: H.ComponentHTML Action Slots m }
quantPieces s =
  { output: piece "Output quantisation" outName (provenance Route.OdonusOut s)
      "What each note snaps to at the end, past its head's offset: a chord colours the melody without changing its shape. Patched on the dashboard's chart (odonus.out), or set by odonus $ harmony in Limulus"
  , grid: piece "Grid quantisation" gridName (provenance Route.OdonusGrid s)
      "What a cell's value is mapped onto: the melody's shape. Patched on the dashboard's chart (odonus.grid), or set by odonus $ scale in Limulus"
  }
  where
  ctx = contextInfo s.odo
  gridName = case s.odo.scalePattern of
    Just sp -> "scale \"" <> sp <> "\""
    Nothing -> Scale.rootName ctx.rootPc <> " " <> ctx.name
  outName = case s.odo.outScale, s.odo.harmony of
    Just o, _ -> "scale \"" <> o.pattern <> "\" on " <> Scale.rootName o.root
    _, Just h -> "harmony \"" <> h <> "\""
    _, _ -> "none"
  piece label name from tip =
    HH.div [ style "display:flex;align-items:center;gap:8px;min-width:0;margin:-6px 0 14px;padding-bottom:10px;border-bottom:1px solid #00000018", HH.attr (HH.AttrName "title") tip ]
      [ HH.span [ style "font-family:Georgia,serif;font-size:18px;color:#7a6a3a;line-height:1" ] [ HH.text "\x2190" ]
      , HH.div [ style "display:flex;flex-direction:column;min-width:0" ]
          [ HH.span [ style $ engrave <> ";font-size:8px;letter-spacing:0.16em;color:#5a4a1f;white-space:nowrap" ]
              [ HH.text (toUpper label) ]
          , HH.span [ style "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;min-width:0" ]
              [ HH.span [ style "font-family:Georgia,serif;font-size:13px;color:#2a271e" ] [ HH.text name ]
              , HH.span [ style "font-family:Georgia,serif;font-style:italic;font-size:12px;color:#6a5820" ]
                  [ HH.text ("  \x00b7 " <> from) ]
              ]
          ]
      ]

-- | Where an input's set comes from: its harmony route if it has one (saying
-- | so when a Vetula source has nothing behind it, so the input falls back to
-- | Odonus's own scale), else a line that set it, else Odonus's own.
provenance :: Route.Input -> State -> String
provenance input s = case Route.sourceOf input routes of
  Just Route.VetulaKey -> vetula "Vetula\x2019s key"
  Just (Route.VetulaVoice n) -> vetula ("Vetula voice " <> show n)
  Just (Route.Scale _) -> "a scale route"
  Just (Route.Harmony _) -> "a harmony route"
  Nothing -> case input of
    Route.OdonusGrid
      | isJust s.odo.scalePattern -> "a line in Limulus"
      | otherwise -> "Odonus\x2019s own scale"
    Route.OdonusOut
      | isJust s.odo.harmony || isJust s.odo.outScale -> "a line in Limulus"
      | otherwise -> "no route"
  where
  routes = fromMaybe [] (s.routesText >>= hush <<< Route.parse)
  fed = case input of
    Route.OdonusGrid -> s.feedsSeen.grid
    Route.OdonusOut -> s.feedsSeen.out
  vetula name = if fed == Route.Unfed then name <> ", silent: Odonus\x2019s own" else name

-- | Extract the display facts from the effective pitch set. `root` is a MIDI note;
-- | its pitch class is the scale root, and each interval mapped over it gives the
-- | lit pitch classes. `recogniseScale` names the interval shape.
contextInfo :: M.Odonus -> { rootPc :: Int, pcs :: Array Int, name :: String }
contextInfo odo = case M.effectivePitchSet odo of
  PitchSet ps ->
    { rootPc: mod ps.root 12
    , pcs: map (\iv -> mod (ps.root + iv) 12) ps.offsets
    , name: Scale.recogniseScale ps.offsets
    }
