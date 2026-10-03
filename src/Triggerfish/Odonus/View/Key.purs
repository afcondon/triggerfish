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
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Reef.PitchSet (PitchSet(..))
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, style)

-- | The nav strip: what Odonus is quantising to, as a name plus the twelve pitch
-- | classes with the in-context ones lit and the root accented. Horizontal, short,
-- | and non-interactive — a readout, not a control.
contextStrip :: forall m. State -> H.ComponentHTML Action Slots m
contextStrip s =
  let ctx = contextInfo s.odo
  in HH.div
    [ style "display:flex;align-items:center;gap:10px;min-width:0" ]
    $ [ HH.span [ style $ engrave <> ";font-size:8px;color:#7a6a3a;white-space:nowrap" ]
        [ HH.text "Grid" ]
    , HH.span
        [ style "font-family:Georgia,serif;font-size:13px;color:#2a271e;white-space:nowrap"
        , HH.attr (HH.AttrName "title")
            "What the grid quantises to. Its source is a harmony route (the dashboard's Harmony matrix): a scale, Vetula's key, or nothing" ]
        [ HH.text (Scale.rootName ctx.rootPc <> " " <> ctx.name) ]
    , pcKeyboardRO ctx.rootPc ctx.pcs (M.currentChordPCs s.odo)
    ]
    <> case s.odo.scalePattern of
      Nothing -> []
      Just sp ->
        [ HH.span
            [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#2a271e;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:200px"
            , HH.attr (HH.AttrName "title") ("scale \"" <> sp <> "\" — Tidal scale names as a pattern; the lit keys are the scale it gives now") ]
            [ HH.text ("scale \"" <> sp <> "\"") ]
        ]
    <> case s.odo.harmony of
      Nothing -> []
      Just h ->
        [ HH.span
            [ style "font-family:'SF Mono',Menlo,monospace;font-size:11px;color:#2a271e;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:260px"
            , HH.attr (HH.AttrName "title") ("harmony \"" <> h <> "\" — a Tidal pattern; the ringed keys are the chord it gives now") ]
            [ HH.text ("harmony \"" <> h <> "\"") ]
        ]

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
