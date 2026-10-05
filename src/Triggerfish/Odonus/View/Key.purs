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
module Triggerfish.Odonus.View.Key (contextStrip) where

import Prelude

import Data.Array (elem, range)
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

-- | The nav strip: Odonus's two quantisations, one row each, as the dashboard's
-- | patch bay names its two inputs (AC, 2026-10-05: prominent, clearly labelled,
-- | and saying where each comes from).
-- |
-- |   GRID    the scale a cell's value is mapped onto (q1, `odonus.grid`)
-- |   OUTPUT  the set the output snaps to past each head's offset (q2,
-- |           `odonus.out`): a chord, an output scale, or the grid's own scale
-- |
-- | Each row: the twelve pitch classes (lit in the set, the root accented), the
-- | set's name, and `← its source`: a harmony route (Vetula's key, a Vetula
-- | voice, a scale or harmony route), a line typed in Limulus, or Odonus's own
-- | scale. A readout, not a control: routes are patched on the dashboard's chart.
contextStrip :: forall m. State -> H.ComponentHTML Action Slots m
contextStrip s =
  let
    ctx = contextInfo s.odo
    outSet = s.odo.chord
    outRoot = case s.odo.outScale of
      Just o -> o.root
      Nothing -> case outSet of
        Just _ -> -1
        Nothing -> ctx.rootPc
  in
    HH.div [ style "display:grid;grid-template-columns:auto auto auto auto;align-items:center;column-gap:10px;row-gap:3px;min-width:0" ]
      ( row "Grid" ctx.rootPc ctx.pcs (gridName ctx) (provenance Route.OdonusGrid s)
          "What a cell's value is mapped onto: the melody's shape. Patched on the dashboard's chart (odonus.grid), or set by odonus $ scale in Limulus"
        <> row "Output" outRoot (fromMaybe ctx.pcs outSet) (outName ctx) (provenance Route.OdonusOut s)
          "What each note snaps to at the end, past each head's offset: a chord colours the melody without changing its shape. Patched on the dashboard's chart (odonus.out), or set by odonus $ harmony in Limulus"
      )
  where
  gridName ctx = case s.odo.scalePattern of
    Just sp -> "scale \"" <> sp <> "\""
    Nothing -> Scale.rootName ctx.rootPc <> " " <> ctx.name
  outName _ = case s.odo.outScale, s.odo.harmony of
    Just o, _ -> "scale \"" <> o.pattern <> "\" on " <> Scale.rootName o.root
    _, Just h -> "harmony \"" <> h <> "\""
    _, _ -> "as the grid"
  row label root lit name from tip =
    [ HH.span [ style $ engrave <> ";font-size:9px;letter-spacing:0.14em;color:#5a4a1f;white-space:nowrap", HH.attr (HH.AttrName "title") tip ]
        [ HH.text (toUpper label) ]
    , pcKeyboardRO root lit []
    , HH.span
        [ style "font-family:Georgia,serif;font-size:13px;color:#2a271e;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:240px"
        , HH.attr (HH.AttrName "title") name ]
        [ HH.text name ]
    , HH.span
        [ style "font-family:Georgia,serif;font-style:italic;font-size:12px;color:#6a5820;white-space:nowrap"
        , HH.attr (HH.AttrName "title") tip ]
        [ HH.text ("\x2190 " <> from) ]
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

-- | A 12-key chromatic strip, non-interactive: in-context pitch classes lit, the
-- | root accented. Sized for the nav bar rather than a panel — fixed key width, so
-- | it doesn't stretch across whatever room the nav happens to have.
pcKeyboardRO :: forall m. Int -> Array Int -> Array Int -> H.ComponentHTML Action Slots m
pcKeyboardRO rootPc lit chord =
  HH.div [ style "display:flex;gap:2px" ]
    (map (pcKeyRO rootPc lit chord) (range 0 11))

-- | One key: lit if in the scale, the root accented, ringed if in the chord the
-- | harmony gives this step.
pcKeyRO :: forall m. Int -> Array Int -> Array Int -> Int -> H.ComponentHTML Action Slots m
pcKeyRO rootPc lit chord pc =
  let
    on = elem pc lit
    ringed = elem pc chord
    isRoot = pc == rootPc
    bg = if isRoot then "#b5832b" else if on then "#8a9b6e" else "#bdb8a7"
    fg = if isRoot || on then "#1c1a12" else "#7d7868"
  in
    HH.div
      [ style $ "width:17px;height:20px;border-radius:3px;box-sizing:border-box;border:"
          <> (if ringed then "2px solid #2a271e" else "1px solid #00000018") <> ";background:" <> bg
          <> ";display:flex;align-items:flex-end;justify-content:center;padding-bottom:1px" ]
      [ HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:7px;color:" <> fg ]
          [ HH.text (Scale.rootName pc) ] ]
