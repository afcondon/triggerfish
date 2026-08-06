-- | The READ-ONLY harmonic-context display. Vetula is the single harmonic
-- | authority (macro-tidal harmonic-authority decision): Odonus no longer owns a
-- | scale, it follows whatever Vetula supplies (the key's resting scale, a firing
-- | chord from a progression, or the `→ odo` box's chord). This shows that
-- | inherited context; to change it you set the scale in Vetula or via `# scale`.
-- |
-- | Was the KEY *panel* — a whole right-hand column carrying this plus SCENES plus
-- | LOGBOOK. Retired 2026-08-06 (AC): the column cost a fifth of the width while
-- | PARAMETERS next to it needed a scrollbar. This is now a compact strip in
-- | Odonus's secondary nav (`Odonus.View.Nav`), which is where a live player wants
-- | it anyway — glanceable, not a panel to read.
module Triggerfish.Odonus.View.Key (contextStrip) where

import Prelude

import Data.Array (elem, range)
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
    [ HH.span [ style $ engrave <> ";font-size:8px;color:#7a6a3a;white-space:nowrap" ]
        [ HH.text "◀ Vetula" ]
    , HH.span
        [ style "font-family:Georgia,serif;font-size:13px;color:#2a271e;white-space:nowrap"
        , HH.attr (HH.AttrName "title")
            "Vetula owns the harmonic context — the active chord of a progression, the → odo box's chord, or the browsed scale" ]
        [ HH.text (Scale.rootName ctx.rootPc <> " " <> ctx.name) ]
    , pcKeyboardRO ctx.rootPc ctx.pcs
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
pcKeyboardRO :: forall m. Int -> Array Int -> H.ComponentHTML Action Slots m
pcKeyboardRO rootPc lit =
  HH.div [ style "display:flex;gap:2px" ]
    (map (pcKeyRO rootPc lit) (range 0 11))

pcKeyRO :: forall m. Int -> Array Int -> Int -> H.ComponentHTML Action Slots m
pcKeyRO rootPc lit pc =
  let
    on = elem pc lit
    isRoot = pc == rootPc
    bg = if isRoot then "#b5832b" else if on then "#8a9b6e" else "#bdb8a7"
    fg = if isRoot || on then "#1c1a12" else "#7d7868"
  in
    HH.div
      [ style $ "width:17px;height:20px;border-radius:3px;border:1px solid #00000018;background:" <> bg
          <> ";display:flex;align-items:flex-end;justify-content:center;padding-bottom:1px" ]
      [ HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:7px;color:" <> fg ]
          [ HH.text (Scale.rootName pc) ] ]
