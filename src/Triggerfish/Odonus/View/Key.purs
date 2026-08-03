-- | KEY panel — now a READ-ONLY harmonic-context display. Vetula is the single
-- | harmonic authority (macro-tidal harmonic-authority decision): Odonus no longer
-- | owns a scale, it follows whatever Vetula supplies (the key's resting scale, or a
-- | firing chord from a progression). This pane shows that inherited context; to
-- | change it you set the scale in Vetula (its key/scale pickers) or via a macro
-- | `# scale`. The SCENES song machinery stacks below in the same column (#139).
module Triggerfish.Odonus.View.Key (quantizerPanel) where

import Prelude

import Data.Array (elem, range)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Reef.PitchSet (PitchSet(..))
import Triggerfish.Odonus.Model as M
import Triggerfish.Scale as Scale
import Triggerfish.Odonus.Grid.Types (Action, Slots, State)
import Triggerfish.Odonus.Grid.Widgets (engrave, panelShell, style)
import Triggerfish.Odonus.View.Scenes (scenesBody)

-- | The merged KEY pane (#139): the read-only harmonic-context display, then the
-- | SCENES machinery below in the same scrolling column.
quantizerPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
quantizerPanel s =
  panelShell s.collapsed "KEY" "Context · Song" "flex:0 1 290px;min-width:min-content"
    ( [ contextDisplay s.odo ] <> scenesBody s )

-- | The inherited harmonic context, read from Odonus's effective pitch set (the
-- | one Vetula pushed): its root + recognised scale name, the pitch classes lit on
-- | a non-interactive keyboard, and whether a chord is firing over it.
contextDisplay :: forall m. M.Odonus -> H.ComponentHTML Action Slots m
contextDisplay odo =
  let ctx = contextInfo odo
  in HH.div_
    [ HH.div [ style "display:flex;align-items:baseline;justify-content:space-between;margin-bottom:8px" ]
        [ HH.span [ style $ engrave <> ";font-size:9px" ] [ HH.text "HARMONIC CONTEXT" ]
        , HH.span [ style $ engrave <> ";font-size:8px;color:#7a6a3a" ] [ HH.text "◀ Vetula" ]
        ]
    , HH.div [ style "font-family:Georgia,serif;font-size:15px;color:#2a271e;margin-bottom:9px" ]
        [ HH.text (Scale.rootName ctx.rootPc <> "  " <> ctx.name) ]
    , pcKeyboardRO ctx.rootPc ctx.pcs
    , HH.div [ style "font-family:Georgia,serif;font-size:9px;color:#8a8576;line-height:1.5;font-style:italic" ]
        [ HH.text "Vetula owns the harmonic context — the active chord of a progression, or the browsed scale. Set it in Vetula, or with a macro `# scale`." ]
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
-- | root accented. (The interactive version — click to toggle scale membership —
-- | is gone; Odonus no longer authors its scale.)
pcKeyboardRO :: forall m. Int -> Array Int -> H.ComponentHTML Action Slots m
pcKeyboardRO rootPc lit =
  HH.div [ style "display:flex;gap:2px;margin-bottom:10px" ]
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
      [ style $ "flex:1;height:34px;border-radius:3px;border:1px solid #00000018;background:" <> bg
          <> ";display:flex;align-items:flex-end;justify-content:center;padding-bottom:2px" ]
      [ HH.span [ style $ "font-family:'SF Mono',Menlo,monospace;font-size:7px;color:" <> fg ]
          [ HH.text (Scale.rootName pc) ] ]
